# AlphaForge Insight — backend API entry point (Phase 6: full backtest API)
#
# Run from anywhere:
#   Rscript backend/plumber.R
# or, from inside backend/:
#   Rscript plumber.R
#
# Defaults: host 127.0.0.1, port 8000.
# Override with environment variables PLUMBER_HOST and PLUMBER_PORT
# (see backend/.Renviron.example).
#
# Phase 1 implemented GET /health. Phase 2 added database + authentication
# (POST /auth/register, POST /auth/login, GET /auth/me). Phase 3 adds the
# market-data + technical-indicator layer:
#   GET /stocks/validate   (public)     ticker existence check
#   GET /market-data       (JWT)        historical daily OHLCV
# Phase 4 adds the strategy engine (validation, defaults, signals):
#   GET  /strategies/defaults (public)  documented default parameters
#   POST /strategies/preview  (JWT)     validate config -> signals (no trades)
# Phase 5 adds the pure backtesting engine and performance-metric layer:
#   backend/analytics/execution.R      t+1-open execution, cash/shares,
#                                      trade log, equity curve (run_backtest)
#   backend/analytics/metrics.R        8 metrics + summary + result object
# Phase 6 adds the full backtest API:
#   POST /backtest/run       (JWT)  full backtest pipeline -> JSON + DB persist
#   GET  /backtest/history   (JWT)  paginated user backtest list
#   GET  /backtest/<id>      (JWT)  full backtest record by ID
#   GET  /backtest/<id>/trades   (JWT)  trade history for a backtest
#   GET  /backtest/<id>/metrics  (JWT)  performance metrics for a backtest
#   GET  /market/tickers/search (public) Yahoo Finance symbol lookup
#                                        (autocomplete for the ML ticker inputs)

library(plumber)

# Resolve this file's directory so relative includes work under both
# evaluation modes of the self-plumbing pattern:
#   1. Direct run with Rscript            -> cwd is wherever the user launched
#      from; commandArgs() carries --file=<path>.
#   2. Second pass inside plumb()         -> plumb() temporarily sets cwd to
#      the API file's directory, so plumber.R exists in cwd.
resolve_script_dir <- function() {
  # Pass 2 (plumb evaluation): plumber.R already exists in the working
  # directory because plumb() switches cwd to the file's folder.
  if (file.exists("plumber.R")) {
    return(getwd())
  }
  # Pass 1 (sourced or run directly).
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) {
    return(dirname(normalizePath(ofile)))
  }
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    return(dirname(normalizePath(file_arg[1])))
  }
  getwd()
}

script_dir <- resolve_script_dir()

# Load environment variables from backend/.Renviron (git-ignored; contains
# DATABASE_URL and JWT_SECRET in development). This mirrors R's own startup
# behaviour but works regardless of the initial working directory.
renviron_file <- file.path(script_dir, ".Renviron")
if (file.exists(renviron_file)) {
  readRenviron(renviron_file)
}
# Convenience fallback: a root-level .env file (KEY=VALUE, used by the
# project's local configuration workflow) is loaded when backend/.Renviron
# is absent. Both files are git-ignored.
root_env <- file.path(script_dir, "..", ".env")
if (file.exists(root_env)) {
  readRenviron(root_env)
}

# Load health handler (kept separate so it is unit-testable).
source(file.path(script_dir, "api", "health.R"))

# Load database + auth modules (Phase 2). Functions defined here are visible
# to routers mounted below (the parent environment of a plumbed router).
source(file.path(script_dir, "database", "connection.R"))
source(file.path(script_dir, "database", "queries.R"))
source(file.path(script_dir, "api", "auth_helpers.R"))

# Load analytics modules (Phase 3 + 4): market-data retrieval, technical
# indicators, signal generation, and strategy validation. quantmod/TTR are
# attached by these files (see backend/analytics/*.R).
source(file.path(script_dir, "analytics", "data_retrieval.R"))
source(file.path(script_dir, "analytics", "indicators.R"))
source(file.path(script_dir, "analytics", "signals.R"))
source(file.path(script_dir, "analytics", "utils", "validation.R"))

# Load the Phase 5 backtesting engine (execution) and performance-metric
# layer. These define pure functions only (no side effects at load time);
# run_backtest() orchestrates execution -> metrics -> result object and will
# be called by the Phase 6 /backtest/run endpoint.
source(file.path(script_dir, "analytics", "execution.R"))
source(file.path(script_dir, "analytics", "metrics.R"))

# Load backtest API handlers (Phase 6). These define handle_backtest_run(),
# handle_backtest_history() and handle_backtest_get().
source(file.path(script_dir, "api", "backtest.R"))

# Load the ML/DL extension (ML Phases A–H). Everything here is ADDITIVE: the
# ML module reuses fetch_ohlcv (market data), the TTR indicator conventions,
# run_backtest/compute_performance_metrics (the ONLY execution + metric
# engine) and require_auth. It exposes POST /ml/features, /ml/train,
# /ml/compare, /ml/predict and /ml/backtest through handlers in api/ml.R.
source(file.path(script_dir, "analytics", "ml", "features.R"))
source(file.path(script_dir, "analytics", "ml", "labels.R"))
source(file.path(script_dir, "analytics", "ml", "preprocessing.R"))
source(file.path(script_dir, "analytics", "ml", "evaluation.R"))
source(file.path(script_dir, "analytics", "ml", "models.R"))
source(file.path(script_dir, "analytics", "ml", "lstm.R"))
source(file.path(script_dir, "analytics", "ml", "hyperparameters.R"))
source(file.path(script_dir, "analytics", "ml", "adapter.R"))
source(file.path(script_dir, "analytics", "ml", "persistence.R"))
source(file.path(script_dir, "analytics", "ml", "python_ts.R"))
source(file.path(script_dir, "analytics", "ml", "pipeline.R"))
source(file.path(script_dir, "api", "ml.R"))

# Load the market-data route handlers (Phase 3) and strategy route handlers
# (Phase 4) directly into this environment so that plumb(self) picks them up
# as inline endpoints — avoiding the multiple-router mount issue that occurs
# when two routers are both mounted at "/".
source(file.path(script_dir, "api", "data.R"))
source(file.path(script_dir, "api", "strategies.R"))
source(file.path(script_dir, "api", "search.R"))

# Load the live news provider and API handlers (Phase H.4). Fetches recent
# financial news from Marketaux (free tier) with caching and error handling.
source(file.path(script_dir, "analytics", "news_live.R"))
source(file.path(script_dir, "api", "news.R"))

#* @apiTitle AlphaForge Insight API
#* @apiDescription Backend API for the AlphaForge Insight Quant Strategy Backtester.
#*   Phase 2: database (Neon PostgreSQL) + authentication
#*   (register, login, JWT, protected /auth/me). Phase 3: market data
#*   (ticker validation + historical OHLCV) and technical indicators.
#*   Phase 4: strategy engine (MA Crossover + RSI signals, validation, defaults).
#*   Phase 5: pure backtesting engine (t+1-open execution, cash/shares, trade
#*   log, equity curve) and the 8-metric performance layer (no API endpoints
#*   yet; those arrive with Phase 6).
#*   Phase 6: full backtest API (POST /backtest/run, GET /backtest/history,
#*   GET /backtest/:id, GET /backtest/:id/trades, GET /backtest/:id/metrics),
#*   database persistence, results dashboard.
#*   Phase 7 UX: public GET /market/tickers/search for the ticker autocomplete.
#* @apiVersion 0.6.0

#* Development CORS filter (Phase 1, preserved).
#* Allows the local Vite dev server (default http://localhost:5173) to call
#* this API directly during development. Production CORS will be tightened
#* during deployment (IMPLEMENTATION_PLAN.md section O).
#* @filter cors
cors <- function(req, res) {
  allowed_origins <- c(
    "http://localhost:5173",
    "https://se-theory-project.vercel.app"
  )

  request_origin <- req$HTTP_ORIGIN

  if (!is.null(request_origin) &&
      request_origin %in% allowed_origins) {
    res$setHeader(
      "Access-Control-Allow-Origin",
      request_origin
    )
    res$setHeader("Vary", "Origin")
  }

  res$setHeader(
    "Access-Control-Allow-Methods",
    "GET, POST, PUT, PATCH, DELETE, OPTIONS"
  )
  res$setHeader(
    "Access-Control-Allow-Headers",
    "Authorization, Content-Type"
  )

  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 204
    return("")
  }

  plumber::forward()
}

#* Health check endpoint.
#* Returns a small JSON object confirming the API is running.
#* @get /health
#* @serializer unboxedJSON
function() {
  health_handler()
}

#* Search Yahoo Finance ticker symbols for autocomplete (public)
#* @get /market/tickers/search
#* @serializer unboxedJSON
function(q = NULL, limit = 5, req, res) {
  out <- handle_ticker_search(q, limit)
  res$status <- out$status
  out$body
}

#* Validate a stock ticker (public)
#* @get /stocks/validate
#* @serializer unboxedJSON
function(ticker = NULL, req, res) {
  out <- handle_stocks_validate(ticker)
  res$status <- out$status
  out$body
}

#* Retrieve historical daily OHLCV (JWT required)
#* @get /market-data
#* @serializer unboxedJSON
function(ticker = NULL, start = NULL, end = NULL, req, res) {
  out <- handle_market_data(ticker, start, end, req)
  res$status <- out$status
  out$body
}

#* Retrieve cached historical OHLCV for the trusted Python forecasting service.
#* Requires X-AlphaForge-Internal-Token; never expose this token to the frontend.
#* @get /internal/market-data
#* @serializer unboxedJSON
function(ticker = NULL, start = NULL, end = NULL, req, res) {
  out <- handle_internal_market_data(ticker, start, end, req)
  res$status <- out$status
  out$body
}

#* Get strategy defaults (public)
#* @get /strategies/defaults
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_strategy_defaults()
  res$status <- out$status
  out$body
}

#* Preview strategy signals without executing trades (JWT required)
#* @post /strategies/preview
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_strategy_preview(req)
  res$status <- out$status
  out$body
}

#* Run a full backtest (JWT required)
#* @post /backtest/run
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_backtest_run(req)
  res$status <- out$status
  out$body
}

#* Get backtest history (JWT required)
#* @get /backtest/history
#* @serializer unboxedJSON
function(req, res, limit = 20, offset = 0) {
  out <- handle_backtest_history(req, limit, offset)
  res$status <- out$status
  out$body
}

#* Get a backtest by ID (JWT required)
#* @get /backtest/<id>
#* @serializer unboxedJSON
function(req, res, id) {
  out <- handle_backtest_get(req, id)
  res$status <- out$status
  out$body
}

#* Get the trade history of a backtest (JWT required)
#* @get /backtest/<id>/trades
#* @serializer unboxedJSON
function(req, res, id) {
  out <- handle_backtest_trades(req, id)
  res$status <- out$status
  out$body
}

#* Get the performance metrics of a backtest (JWT required)
#* @get /backtest/<id>/metrics
#* @serializer unboxedJSON
function(req, res, id) {
  out <- handle_backtest_metrics(req, id)
  res$status <- out$status
  out$body
}

#* Generate the ML feature matrix for a ticker/date range (JWT required)
#* @post /ml/features
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_ml_features(req)
  res$status <- out$status
  out$body
}

#* Train one ML/DL model and return metrics + metadata (JWT required)
#* @post /ml/train
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_ml_train(req)
  res$status <- out$status
  out$body
}

#* Train/evaluate multiple models and return the comparison table (JWT required)
#* @post /ml/compare
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_ml_compare(req)
  res$status <- out$status
  out$body
}

#* Generate standardized ML predictions + P(BUY) ranking (JWT required)
#* @post /ml/predict
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_ml_predict(req)
  res$status <- out$status
  out$body
}

#* Run the Top-N ML portfolio backtest through the existing engine (JWT required)
#* @post /ml/backtest
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_ml_backtest(req)
  res$status <- out$status
  out$body
}

#* Get recent live news for tickers (public)
#* @get /news/live
#* @serializer unboxedJSON
function(tickers = NULL, limit = 3, req, res) {
  out <- handle_news_live(tickers, limit, req)
  res$status <- out$status
  out$body
}

# Start the server only when this file is executed directly with Rscript
# (sys.nframe() == 0). The guard prevents imports/tests from starting the API.
if (sys.nframe() == 0) {
  host <- Sys.getenv("PLUMBER_HOST", "0.0.0.0")
  port <- as.integer(Sys.getenv("PLUMBER_PORT", "8000"))

  # Test hook: when PLUMBER_PIDFILE is set (used by tests/backend/test_health.R),
  # write this process's PID so the test can terminate the server cleanly.
  pidfile <- Sys.getenv("PLUMBER_PIDFILE", "")
  if (nzchar(pidfile)) {
    writeLines(as.character(Sys.getpid()), con = pidfile)
  }

  cat("AlphaForge Insight API starting on", host, "port", port, "\n")

  # Warn at startup if JWT_SECRET is missing: registration works without it,
  # but login and all token validation will fail at request time.
  if (!nzchar(Sys.getenv("JWT_SECRET", unset = ""))) {
    cat("WARNING: JWT_SECRET is not set. POST /auth/login will fail until it is\n")
    cat("  configured in backend/.Renviron or the root .env file (long random\n")
    cat("  string, >= 16 chars). See backend/.Renviron.example.\n")
  }

  pr <- plumb(file.path(script_dir, "plumber.R"))

  # Mount authentication routes (backend/api/auth.R). The router is mounted
  # at the root; its paths are /auth/register, /auth/login, /auth/me.
  # Market-data routes (/stocks/validate, /market-data) are sourced directly
  # into this file's environment (see above), so they need no separate mount.
  pr_auth <- plumb(file.path(script_dir, "api", "auth.R"))
  pr_mount(pr, "/", pr_auth)

  pr$run(host = host, port = port)
}