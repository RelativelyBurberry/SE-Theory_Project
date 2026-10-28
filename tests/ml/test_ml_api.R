# AlphaForge Insight — ML API integration tests
#
# Starts the REAL Plumber server (backend/plumber.R) on a test port and
# exercises the five ML endpoints end-to-end: auth (401 paths), validation
# (400 paths) and the feature/train/predict happy paths against live Yahoo
# Finance data.
#
# Requires DATABASE_URL + JWT_SECRET (backend/.Renviron or root .env) for the
# 401 tests (JWT signing) and network access for the happy paths.
#
# Run from the repository root:
#   Rscript tests/ml/test_ml_api.R
# Exit code 0 = all pass, 1 = at least one failure.

options(warn = -1)

read_renviron <- function() {
  for (f in c(file.path("backend", ".Renviron"), ".env")) {
    if (file.exists(f)) { readRenviron(f); return(invisible(TRUE)) }
  }
  invisible(FALSE)
}
read_renviron()

results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

cat("==================================================\n")
cat("ML API INTEGRATION TESTS\n")
cat("==================================================\n\n")

# ---------------------------------------------------------------------------
# Start the real API on a scratch port
# ---------------------------------------------------------------------------
port <- 8765L
rscript <- file.path(R.home("bin"), "Rscript")
log <- normalizePath(tempfile(fileext = ".log"), winslash = "/", mustWork = FALSE)
sys <- Sys.info()[["sysname"]]
server_pid <- NULL
if (sys == "Windows") {
  # cmd handles the redirect; env vars are set in the child's environment.
  server_pid <- system2("cmd", sprintf(
    '/c "set PLUMBER_HOST=127.0.0.1&& set PLUMBER_PORT=%d&& "%s" backend/plumber.R > "%s" 2>&1"',
    port, rscript, log), wait = FALSE)
} else {
  server_pid <- system2("sh", c("-c", sprintf(
    '"%s" backend/plumber.R > "%s" 2>&1 & echo $!', rscript, log)),
    env = c("PLUMBER_HOST=127.0.0.1", sprintf("PLUMBER_PORT=%d", port)),
    wait = FALSE)
}

base <- sprintf("http://127.0.0.1:%d", port)
up <- FALSE
for (i in 1:40) {
  Sys.sleep(1)
  up <- tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, timeout = 3)
    r <- curl::curl_fetch_memory(glue::glue("{base}/health"), handle = h)
    TRUE
  }, error = function(e) FALSE)
  if (up) break
}
if (!up) {
  cat("API did not come up; log:\n")
  cat(paste(readLines(log), collapse = "\n"), "\n")
  quit(status = 1)
}
record("API started for ML tests", up)

ml_request <- function(path, body = NULL, token = NULL, timeout = 180) {
  headers <- c("Content-Type" = "application/json")
  if (!is.null(token)) headers["Authorization"] <- paste("Bearer", token)
  h <- curl::new_handle()
  curl::handle_setopt(h, timeout = timeout,
                      postfields = if (is.null(body)) "{}" else
                        jsonlite::toJSON(body, auto_unbox = TRUE))
  curl::handle_setheaders(h, .list = headers)
  resp <- tryCatch(curl::curl_fetch_memory(paste0(base, path), handle = h),
                   error = function(e) list(error = conditionMessage(e)))
  if (!is.null(resp$error)) return(list(raw = NULL, status = 0L, error = resp$error))
  txt <- rawToChar(resp$content)
  list(raw = jsonlite::fromJSON(txt, simplifyVector = FALSE),
       status_code = resp$status_code)
}

# A registered test user (unique email so repeat runs never 409)
email <- sprintf("ml-api-test-%d@example.com", as.integer(Sys.time()))
register_resp <- ml_request("/auth/register", list(email = email, password = "Str0ngPass!x"))
login_resp <- ml_request("/auth/login", list(email = email, password = "Str0ngPass!x"))
token <- if (!is.null(login_resp$raw$token)) login_resp$raw$token else NULL
record("auth: login returns a token for ML tests", !is.null(token))

end_date <- format(Sys.Date() - 1, "%Y-%m-%d")
start_date <- "2023-01-01"

if (!is.null(token)) {
  # --- 401: no token -------------------------------------------------------
  r <- ml_request("/ml/features", list(ticker = "AAPL", start = start_date, end = end_date))
  record("/ml/features: 401 without a token", r$status_code == 401L)

  r <- ml_request("/ml/train", list(model = "random_forest", ticker = "AAPL",
                                    start = start_date, end = end_date))
  record("/ml/train: 401 without a token", r$status_code == 401L)

  r <- ml_request("/ml/compare", list(ticker = "AAPL", start = start_date, end = end_date))
  record("/ml/compare: 401 without a token", r$status_code == 401L)

  r <- ml_request("/ml/predict", list(model = "xgboost", ticker = "AAPL",
                                      start = start_date, end = end_date))
  record("/ml/predict: 401 without a token", r$status_code == 401L)

  r <- ml_request("/ml/backtest", list(model = "xgboost", tickers = c("AAPL"),
                                       start = start_date, end = end_date))
  record("/ml/backtest: 401 without a token", r$status_code == 401L)

  # --- 400: invalid requests ----------------------------------------------
  r <- ml_request("/ml/train",
                  list(model = "not_a_model", ticker = "AAPL",
                       start = start_date, end = end_date), token)
  record("/ml/train: 400 unknown model", r$status_code == 400L)

  r <- ml_request("/ml/train",
                  list(model = "random_forest", ticker = "AAPL",
                       start = "2024-01-01", end = "2023-01-01"), token)
  record("/ml/train: 400 inverted date range", r$status_code == 400L)

  r <- ml_request("/ml/backtest",
                  list(model = "xgboost", tickers = character(0),
                       start = start_date, end = end_date), token)
  record("/ml/backtest: 400 empty tickers", r$status_code == 400L)

  # --- happy paths (live Yahoo data) --------------------------------------
  r <- ml_request("/ml/features",
                  list(ticker = "AAPL", start = "2024-01-01", end = end_date,
                       feature_groups = c("rsi", "bollinger")), token)
  record("/ml/features: 200 on live data", r$status_code == 200L)
  record("/ml/features: groups resolved + rows returned",
         isTRUE(r$raw$success) && length(r$raw$feature_names) > 0 && r$raw$n_rows > 0)

  r <- ml_request("/ml/train",
                  list(model = "decision_tree", ticker = "AAPL",
                       start = "2023-01-01", end = end_date,
                       horizon = 5, buy_threshold = 0.02, sell_threshold = 0.02),
                  token)
  record("/ml/train: 200 decision_tree on live data", r$status_code == 200L,
         if (r$status_code != 200L) as.character(r$raw$error) else "")
  record("/ml/train: exposes model_id, metrics, importance, train_info",
         isTRUE(r$raw$success) &&
           all(c("model_id", "validation_metrics", "hyperparameters", "train_info")
               %in% names(r$raw)) &&
           !is.null(r$raw$validation_metrics$accuracy) &&
           r$raw$validation_metrics$n_eval > 0)

  r <- ml_request("/ml/predict",
                  list(model = "xgboost", ticker = "AAPL",
                       start = "2023-01-01", end = end_date), token)
  record("/ml/predict: 200 with standardized prediction record",
         r$status_code == 200L && isTRUE(r$raw$success) &&
           all(c("date", "prediction", "prob_buy", "prob_hold", "prob_sell")
               %in% names(r$raw$prediction)))

  # compare with the two fastest models to keep the test quick
  r <- ml_request("/ml/compare",
                  list(ticker = "AAPL", start = "2023-01-01", end = end_date,
                       models = c("decision_tree", "lda"),
                       hyperparameters = list(decision_tree = list(maxdepth = 4))),
                  token, timeout = 300)
  record("/ml/compare: 200 with a per-model comparison table",
         r$status_code == 200L && isTRUE(r$raw$success) &&
           length(r$raw$comparison) == 2)
  cmp_cols <- if (length(r$raw$comparison) > 0) names(r$raw$comparison[[1]]) else character(0)
  record("/ml/compare: classification + trading columns present",
         all(c("Model", "Accuracy", "F1", "ROC_AUC", "CAGR", "Sharpe",
               "MaxDrawdown", "WinRate") %in% cmp_cols))
} else {
  record("auth: token obtained (skipping authenticated paths)", FALSE, "no token")
}

# ---------------------------------------------------------------------------
# Cleanup: stop the server (best-effort; the scratch server is ephemeral)
# ---------------------------------------------------------------------------
if (sys == "Windows") {
  # taskkill by PID (cmd.exe spawns the Rscript child under the same PID on
  # Windows; if the PID vanished already the kill is a harmless no-op).
  if (!is.null(server_pid) && !is.na(server_pid)) {
    system2("taskkill", c("/F", "/PID", as.character(server_pid)),
            stdout = FALSE, stderr = FALSE)
  }
} else if (!is.null(server_pid)) {
  tools::pskill(server_pid)
}

cat(sprintf("\n%d tests, %d passed, %d failed\n",
            length(results),
            sum(vapply(results, function(x) x$passed, logical(1))),
            sum(!vapply(results, function(x) x$passed, logical(1)))))
quit(status = if (all(vapply(results, function(x) x$passed, logical(1)))) 0 else 1)
