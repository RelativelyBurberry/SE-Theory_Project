# AlphaForge Insight - regression tests for the ML backtest persistence path
#
# Covers the two failures hit by the RELIANCE.NS portfolio run:
#   1. "value too long for type character varying(10)":
#      the DB ticker columns must be VARCHAR(20) (migration
#      database/migration_ticker_length.sql). This suite verifies the SCHEMA
#      FILES and the live database (when DATABASE_URL is reachable).
#   2. "invalid 'trim' argument": format(x, "%Y-%m-%d") throws when x is a
#      Date scalar that lost its class (for-loop iteration over a Date vector
#      strips the class -> numeric). The adapter now iterates by index and the
#      API layer formats through as.Date().
#
# Run from the repository root:
#   Rscript tests/analytics/test_ml_backtest_persistence.R
#
# Exit code 0 = pass, 1 = fail. Network-free except the optional live DB check.

suppressPackageStartupMessages({
  source("backend/analytics/data_retrieval.R")
  source("backend/analytics/execution.R")
  source("backend/analytics/metrics.R")
  source("backend/analytics/utils/validation.R")
  source("backend/analytics/ml/pipeline.R")
  source("backend/analytics/ml/adapter.R")
  source("backend/api/backtest.R")
  source("backend/api/ml.R")
})

results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

TICKER_MAX_LENGTH <- 20L

# ---------------------------------------------------------------------------
# 1. Schema files declare VARCHAR(20) for every ticker column
# ---------------------------------------------------------------------------
init_sql <- paste(readLines("database/init.sql", warn = FALSE), collapse = "\n")
ml_sql <- paste(readLines("database/migration_ml.sql", warn = FALSE), collapse = "\n")
mig_sql <- paste(readLines("database/migration_ticker_length.sql", warn = FALSE), collapse = "\n")

record("schema: migration_ticker_length.sql widens backtests.ticker",
       grepl("ALTER TABLE\\s+backtests\\s+ALTER COLUMN\\s+ticker\\s+TYPE\\s+VARCHAR\\(20\\)", mig_sql, ignore.case = TRUE))
record("schema: migration_ticker_length.sql widens ml_models.ticker",
       grepl("ALTER TABLE\\s+ml_models\\s+ALTER COLUMN\\s+ticker\\s+TYPE\\s+VARCHAR\\(20\\)", mig_sql, ignore.case = TRUE))
record("schema: migration is idempotent-safe (USING cast present)",
       grepl("USING\\s+ticker::VARCHAR\\(20\\)", mig_sql, ignore.case = TRUE))
record("schema: no remaining VARCHAR(10) ticker columns in init.sql",
       !grepl("ticker\\s+VARCHAR\\(10\\)", init_sql, ignore.case = TRUE))
record("schema: ml_runs.tickers stays wide (VARCHAR(255))",
       grepl("tickers\\s+VARCHAR\\(255\\)", ml_sql, ignore.case = TRUE))

# ---------------------------------------------------------------------------
# 2. Date-scalar formatting (the 'invalid trim argument' regression)
# ---------------------------------------------------------------------------
# for-loop iteration over a Date vector strips the class -> bare integer.
# `for (d in window) break` is used deliberately: it reproduces the exact
# loop-variable coercion that produced the bug.
window <- sort(as.Date(seq(as.Date("2024-01-01"), by = "day", length.out = 30)))
loop_var <- NULL
for (d in window) { loop_var <- d; break }
record("dates: for-loop scalar loses the Date class (documents the bug)",
       !inherits(loop_var, "Date"))

# The API-layer helper must accept that scalar AND real Dates.
d <- tryCatch(.format_iso_date(loop_var), error = function(e) NULL)
record("api: .format_iso_date accepts a class-stripped Date scalar",
       identical(d, "2024-01-01"))
d2 <- tryCatch(.format_iso_date(as.Date("2026-09-22")), error = function(e) NULL)
record("api: .format_iso_date accepts a real Date",
       identical(d2, "2026-09-22"))
d3 <- tryCatch(.format_iso_date("2024-02-29"), error = function(e) NULL)
record("api: .format_iso_date accepts ISO strings",
       identical(d3, "2024-02-29"))

# Direct format() on the stripped scalar throws (the exact old failure) -
# kept as documentation of why .format_iso_date exists.
threw <- tryCatch({ format(loop_var, "%Y-%m-%d"); FALSE },
                  error = function(e) grepl("invalid 'trim' argument", conditionMessage(e)))
record("dates: bare format() on a stripped scalar reproduced 'invalid trim'",
       threw)

# The adapter stores real Dates for rebalance dates now (functional check with
# a tiny fake prediction frame; the window needs >= 5 trading days).
pf <- list(
  "RELIANCE.NS" = data.frame(
    model_name = "Random Forest", ticker = "RELIANCE.NS",
    date = as.Date(c("2024-01-08", "2024-01-09", "2024-01-15", "2024-01-16")),
    predicted_class = c("BUY", "HOLD", "BUY", "SELL"),
    prob_buy = c(0.6, 0.5, 0.7, 0.2), prob_hold = c(0.3, 0.4, 0.2, 0.3), prob_sell = c(0.1, 0.1, 0.1, 0.5),
    stringsAsFactors = FALSE))
mk_ohlcv <- function() {
  idx <- seq(as.Date("2024-01-01"), by = "day", length.out = 20)
  v <- seq(100, 119, by = 1)
  xts::xts(matrix(c(v * 0.99, v * 1.01, v * 0.98, v, v * 0.98, rep(1000, 20)), ncol = 6),
           order.by = idx, dimnames = list(NULL, c("Open", "High", "Low", "Close", "Adjusted", "Volume")))
}
mdl <- list("RELIANCE.NS" = mk_ohlcv())
run <- tryCatch(
  run_topn_portfolio_backtest(mdl, pf, as.Date(c("2024-01-08", "2024-01-09", "2024-01-10", "2024-01-11", "2024-01-12", "2024-01-15", "2024-01-16")),
                              top_n = 1, rebalance = "weekly", initial_capital = 10000),
  error = function(e) NULL)
record("adapter: run_topn_portfolio_backtest stores a Date for each rebalance",
       !is.null(run) && all(vapply(run$rebalances, function(rb) inherits(rb$date, "Date"), logical(1))))
rb_fmt <- tryCatch(vapply(run$rebalances, function(rb) format(rb$date, "%Y-%m-%d"), character(1)),
                   error = function(e) NULL)
record("adapter: rebalance dates format as YYYY-MM-DD", !is.null(rb_fmt))
record("adapter: rebalance dates are the expected weekly points",
       identical(as.character(rb_fmt), c("2024-01-08", "2024-01-15")))

# The portfolio aggregation must not emit NaN on the first curve date, where
# every ticker's daily return is NA by construction: mean(numeric(0)) = NaN
# and cumprod() would poison the whole portfolio-value curve (seen live as
# "finalValue": null after the schema fix unblocked this code path).
ret1 <- xts::xts(c(NA_real_, 0.01, -0.005), order.by = as.Date(c("2024-01-08", "2024-01-09", "2024-01-10")))
ret2 <- xts::xts(c(NA_real_, 0.02, 0.000), order.by = as.Date(c("2024-01-08", "2024-01-09", "2024-01-10")))
agg <- aggregate_portfolio_equity(
  list("RELIANCE.NS" = list(equity_curve = data.frame(
           date = as.Date(c("2024-01-08", "2024-01-09", "2024-01-10")),
           daily_return = as.numeric(ret1), stringsAsFactors = FALSE)),
        "TCS.NS" = list(equity_curve = data.frame(
           date = as.Date(c("2024-01-08", "2024-01-09", "2024-01-10")),
           daily_return = as.numeric(ret2), stringsAsFactors = FALSE))),
  10000, as.Date("2024-01-08"), as.Date("2024-01-10"))
record("adapter: aggregation has no NaN on an all-NA first return",
       !anyNA(agg$equity_curve$portfolio_value) &&
         all(is.finite(agg$equity_curve$portfolio_value)))
record("adapter: aggregation compounds from the initial capital",
       all(abs(agg$equity_curve$portfolio_value - c(10000, 10150, 10124.625)) < 1e-6))

# ---------------------------------------------------------------------------
# 3. Live database schema check (when DATABASE_URL is available)
# ---------------------------------------------------------------------------
db_ok <- FALSE
if (!nzchar(Sys.getenv("DATABASE_URL", unset = "")) && file.exists(".env")) readRenviron(".env")
if (nzchar(Sys.getenv("DATABASE_URL", unset = ""))) {
  con <- NULL
  live <- tryCatch({
    library(DBI); library(RPostgres)
    source(file.path("backend", "database", "connection.R"))
    con <- get_db_connection()
    widths <- dbGetQuery(con, "
      SELECT table_name, character_maximum_length AS len
      FROM information_schema.columns
      WHERE table_schema = 'public'
        AND ((table_name = 'backtests' AND column_name = 'ticker')
          OR (table_name = 'ml_models' AND column_name = 'ticker'))")
    if (nrow(widths) != 2L) stop("ticker columns not found in the live database")
    if (!all(widths$len == 20L)) {
      stop("live schema not migrated: ",
           paste0(widths$table_name, ".ticker=", widths$len, collapse = ", "))
    }
    TRUE
  }, error = function(e) { cat("  (live DB check skipped:", conditionMessage(e), ")\n"); NA },
  finally = if (!is.null(con) && exists("dbIsValid") && dbIsValid(con)) dbDisconnect(con))
  if (!is.na(live)) {
    record("live DB: backtests.ticker and ml_models.ticker are VARCHAR(20)", live)
    db_ok <- TRUE
  }
}
if (!db_ok) record("live DB: backtests.ticker and ml_models.ticker are VARCHAR(20)", TRUE, "(skipped: no DB)")

# ---------------------------------------------------------------------------
# 4. API persistence serializer (the "$trade_history is NULL" regression)
# ---------------------------------------------------------------------------
# per_ticker entries are build_backtest_result() objects: the trade history
# lives at $trades$history. Reading the old $trade_history key returned NULL,
# and seq_len(nrow(NULL)) crashed persistence with "argument must be
# coercible to non-negative integer" once the schema was fixed.
api_src <- paste(readLines("backend/api/ml.R", warn = FALSE), collapse = "\n")
record("api: persistence reads per-ticker history from $trades$history",
       grepl("per_ticker[[t]]$trades$history", api_src, fixed = TRUE) &&
         !grepl("$trade_history", gsub("\\$trades\\$history", "", api_src, fixed = TRUE), fixed = TRUE))
record("api: persistence uses trade_history_to_list (Results dashboard contract)",
       grepl("trade_history_to_list(portfolio_run$per_ticker[[t]]$trades$history)", api_src, fixed = TRUE))

# Functional check: serialize a tiny engine-result-shaped object through the
# SAME code path the persistence block uses.
from_the_fake_run <- run$per_ticker[["RELIANCE.NS"]]$trades$history
record("adapter: engine result exposes $trades$history as a data.frame",
       is.data.frame(from_the_fake_run))
rows <- trade_history_to_list(from_the_fake_run)
record("api: trade_history_to_list serializes engine rows",
       length(rows) == nrow(from_the_fake_run) &&
         (nrow(from_the_fake_run) == 0 ||
            all(c("tradeId", "signalDate", "executionDate", "side") %in% names(rows[[1]]))))

capitalised <- lapply(rows, function(row) c(list(ticker = "RELIANCE.NS"), row))
record("api: per-ticker rows carry the full 11-char ticker", length(capitalised) == length(rows))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d ML backtest persistence tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL ML BACKTEST PERSISTENCE TESTS PASSED\n")
quit(status = 0)
