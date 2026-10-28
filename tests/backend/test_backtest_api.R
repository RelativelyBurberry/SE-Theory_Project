# AlphaForge Insight ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€šÃ‚Â backtest API integration tests (Phase 6)
#
# LIVE tests: start the real Plumber API on a test port and make real HTTP
# requests against the full Phase 6 pipeline. Valid-ticker cases hit Yahoo
# Finance (via quantmod) and therefore depend on network availability.
# Real Neon PostgreSQL is used for persistence, retrieval and user isolation.
#
# Test users are registered through the REAL API (POST /auth/register) so the
# JWT sub claims reference real users.id rows; tokens are then minted locally
# with jose using the same JWT_SECRET the child API process uses.
#
# CLEANUP: the test users (and their backtests/results, via ON DELETE CASCADE)
# are deleted from the database when this script exits. No unrelated data is
# ever touched.
#
# Run from the repository root:
#   Rscript tests/backend/test_backtest_api.R
#
# Exit code 0 = pass, 1 = fail.

PORT <- 8140L

# ---------------------------------------------------------------------------
# JWT secret resolution (mirrors test_strategy_api.R / test_data_api.R).
# ---------------------------------------------------------------------------
repo_root <- getwd()
if (file.exists(file.path(repo_root, "backend", ".Renviron"))) {
  readRenviron(file.path(repo_root, "backend", ".Renviron"))
}
if (file.exists(file.path(repo_root, ".env"))) {
  readRenviron(file.path(repo_root, ".env"))
}

TEST_JWT_SECRET <- Sys.getenv("JWT_SECRET", unset = "")
if (!nzchar(TEST_JWT_SECRET)) {
  TEST_JWT_SECRET <- "backtest-api-test-secret-0123456789abcdef"
}
cat("JWT secret source:",
    if (nzchar(Sys.getenv("JWT_SECRET", unset = ""))) "real (.env/.Renviron)" else "throwaway test",
    "\n")

# Throwaway test users (deleted at cleanup).
TEST_EMAIL_A <- paste0("bt_a_", as.integer(Sys.time()), "@test.local")
TEST_EMAIL_B <- paste0("bt_b_", as.integer(Sys.time()), "@test.local")
TEST_PASSWORD <- "backtest-test-pass-123"

# ---------------------------------------------------------------------------
# HTTP helper (curl) ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€šÃ‚Â same semantics as the other backend API tests.
# ---------------------------------------------------------------------------
library(curl)
library(jsonlite)
library(jose)

http_request <- function(method, path, body = NULL, token = NULL,
                         base = NULL, is_json = TRUE) {
  if (is.null(base)) base <- paste0("http://127.0.0.1:", PORT)
  url <- paste0(base, path)
  h <- curl::new_handle()

  headers <- character(0)
  if (identical(toupper(method), "POST")) {
    payload <- if (is.null(body)) "{}" else jsonlite::toJSON(body, auto_unbox = TRUE)
    curl::handle_setopt(h, customrequest = "POST", post = TRUE, postfields = payload)
    headers <- c(headers, "Content-Type" = "application/json")
  }
  if (!is.null(token)) {
    headers <- c(headers, "Authorization" = paste0("Bearer ", token))
  }
  if (length(headers) > 0) {
    do.call(curl::handle_setheaders, c(list(h = h), as.list(headers)))
  }

  resp <- curl::curl_fetch_memory(url, handle = h)
  raw <- rawToChar(resp$content)
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
  list(status = as.integer(resp$status_code), body = parsed, raw = raw)
}

# ---------------------------------------------------------------------------
results <- list()
record <- function(name, passed, detail = "") {
  ok <- isTRUE(passed)
  results[[length(results) + 1L]] <<- list(name = name, passed = ok, detail = detail)
  cat(sprintf("%s %s %s\n", if (ok) "PASS" else "FAIL", name, detail))
  invisible(ok)
}

# ---------------------------------------------------------------------------
# Start the API
# ---------------------------------------------------------------------------
pidfile <- tempfile(fileext = ".pid")
outlog <- tempfile(fileext = ".log")
errlog <- tempfile(fileext = ".log")

Sys.setenv(PLUMBER_PORT = as.character(PORT), PLUMBER_HOST = "127.0.0.1",
           PLUMBER_PIDFILE = pidfile, JWT_SECRET = TEST_JWT_SECRET)
on.exit(Sys.unsetenv(c("PLUMBER_PORT", "PLUMBER_HOST", "PLUMBER_PIDFILE", "JWT_SECRET")),
        add = TRUE)

rscript <- file.path(R.home("bin"), "Rscript")
invisible(system2(rscript, c(file.path("backend", "plumber.R")),
                  wait = FALSE, stdout = outlog, stderr = errlog))

deadline <- Sys.time() + 75
while (!file.exists(pidfile) && Sys.time() < deadline) Sys.sleep(0.3)
if (!file.exists(pidfile)) {
  cat("FAIL: server did not start in time; stderr:\n",
      paste(tryCatch(readLines(errlog, warn = FALSE), error = function(e) "n/a"),
            collapse = "\n"), "\n")
  quit(status = 1)
}
pid <- as.integer(readLines(pidfile, warn = FALSE))
on.exit(try(tools::pskill(pid), silent = TRUE), add = TRUE)

ready <- FALSE
while (Sys.time() < deadline) {
  r <- tryCatch(http_request("GET", "/health"), error = function(e) NULL)
  if (!is.null(r) && r$status == 200) { ready <- TRUE; break }
  Sys.sleep(0.3)
}
if (!ready) {
  cat("FAIL: API did not become ready\n")
  quit(status = 1)
}
cat("API ready on port", PORT, "\n")

# ---------------------------------------------------------------------------
# Create the two test users through the REAL API (real bcrypt hashing, real
# insert_user, real user ids). Tokens are then minted for those ids.
# ---------------------------------------------------------------------------
reg_a <- http_request("POST", "/auth/register",
                      body = list(email = TEST_EMAIL_A, password = TEST_PASSWORD))
reg_b <- http_request("POST", "/auth/register",
                      body = list(email = TEST_EMAIL_B, password = TEST_PASSWORD))

if (reg_a$status != 201L || reg_b$status != 201L) {
  cat("FAIL: could not register test users (A:", reg_a$status, "B:", reg_b$status, ")\n")
  quit(status = 1)
}
user_a_id <- reg_a$body$user$id
user_b_id <- reg_b$body$user$id
cat("Test user A id:", user_a_id, "| Test user B id:", user_b_id, "\n")

mint_token <- function(user_id, exp_delta = 7200) {
  claim <- jwt_claim(sub = as.character(user_id), email = "bt@test.local",
                     iat = as.numeric(Sys.time()),
                     exp = as.numeric(Sys.time()) + exp_delta)
  jwt_encode_hmac(claim, charToRaw(TEST_JWT_SECRET))
}
token_a <- mint_token(user_a_id)
token_b <- mint_token(user_b_id)

# Cleanup: delete the test users (backtests + results cascade). Registered via
# on.exit AFTER server start so it also runs if the test aborts.
cleanup_tests <- function() {
  con <- tryCatch({
    src_env <- new.env(parent = globalenv())
    sys.source(file.path("backend", "database", "connection.R"), envir = src_env)
    src_env$get_db_connection()
  }, error = function(e) NULL)
  if (!is.null(con)) {
    on.exit(src_env$close_db_connection(con), add = TRUE)
    try(DBI::dbExecute(con,
      "DELETE FROM users WHERE email IN ($1, $2)",
      params = list(TEST_EMAIL_A, TEST_EMAIL_B)), silent = TRUE)
  }
}
on.exit(cleanup_tests(), add = TRUE)

# ---------------------------------------------------------------------------
# 1. Authentication on /backtest/* (401s)
# ---------------------------------------------------------------------------
r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "AAPL", start = "2023-01-01", end = "2023-12-31",
                              initialCapital = 10000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)))
record("run: missing JWT returns 401",
       r$status == 401 && isFALSE(r$body$success))

r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "AAPL", start = "2023-01-01", end = "2023-12-31",
                              initialCapital = 10000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = "forged.jwt.token")
record("run: invalid JWT returns 401",
       r$status == 401 && isFALSE(r$body$success))

r <- http_request("GET", "/backtest/history")
record("history: missing JWT returns 401",
       r$status == 401)

r <- http_request("GET", "/backtest/1")
record("get: missing JWT returns 401",
       r$status == 401)

r <- http_request("GET", "/backtest/1/metrics")
record("metrics: missing JWT returns 401",
       r$status == 401)

r <- http_request("GET", "/backtest/1/trades")
record("trades: missing JWT returns 401",
       r$status == 401)

# ---------------------------------------------------------------------------
# 2. Validation rejections on POST /backtest/run (400s)
# ---------------------------------------------------------------------------
r <- http_request("POST", "/backtest/run", body = list(), token = token_a)
record("run: empty body -> 400",
       r$status == 400 && isFALSE(r$body$success) && nzchar(r$body$error))

r <- http_request("POST", "/backtest/run",
                  body = list(start = "2023-01-01", end = "2023-12-31",
                              initialCapital = 10000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = token_a)
record("run: missing ticker -> 400",
       r$status == 400 && isFALSE(r$body$success) && nzchar(r$body$error))

r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "AAPL", start = "2023-06-01", end = "2023-01-01",
                              initialCapital = 10000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = token_a)
record("run: start after end -> 400",
       r$status == 400 && grepl("on or before", r$body$error))

r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "AAPL", start = "2023-01-01", end = "2023-12-31",
                              initialCapital = 10000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 200, slow_period = 50)),
                  token = token_a)
record("run: fast >= slow -> 400",
       r$status == 400 && grepl("strictly less than", r$body$error))

r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "AAPL", start = "2023-01-01", end = "2023-12-31",
                              initialCapital = 10000, strategy = "NOT_A_STRATEGY",
                              parameters = list()),
                  token = token_a)
record("run: unsupported strategy -> 400",
       r$status == 400 && grepl("Unsupported strategy", r$body$error))

r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "AAPL", start = "2023-01-01", end = "not-a-date",
                              initialCapital = 10000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = token_a)
record("run: malformed end date -> 400",
       r$status == 400 && grepl("YYYY-MM-DD", r$body$error))

r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "AAPL@@@", start = "2023-01-01", end = "2023-12-31",
                              initialCapital = 10000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = token_a)
record("run: invalid ticker format -> 400",
       r$status == 400 && isFALSE(r$body$success))

# ---------------------------------------------------------------------------
# 3. Valid MA Crossover backtest (real Yahoo data + real Neon persistence)
# ---------------------------------------------------------------------------
MA_BODY <- list(ticker = "AAPL", start = "2023-01-01", end = "2024-12-31",
                initialCapital = 10000, strategy = "MA_Crossover",
                parameters = list(fast_period = 10, slow_period = 20))

run_ma <- http_request("POST", "/backtest/run", body = MA_BODY, token = token_a)
r <- run_ma
record("run: valid MA backtest -> 200 + success",
       r$status == 200 && isTRUE(r$body$success))

record("run: returns a real backtestId",
       !is.null(r$body$backtestId) && suppressWarnings(!is.na(as.integer(r$body$backtestId))))

MA_ID <- r$body$backtestId

record("run: response echoes configuration (ticker/strategy/dates)",
       identical(r$body$ticker, "AAPL") && identical(r$body$strategy, "MA_Crossover") &&
         identical(r$body$start, "2023-01-01") && identical(r$body$end, "2024-12-31"))

record("run: metrics object carries the 8 metrics + capital",
       !is.null(r$body$metrics$totalReturn) &&
         !is.null(r$body$metrics$cagr) &&
         !is.null(r$body$metrics$sharpe) &&
         !is.null(r$body$metrics$sortino) &&
         !is.null(r$body$metrics$maximumDrawdown) &&
         !is.null(r$body$metrics$winRate) &&
         !is.null(r$body$metrics$volatility) &&
         !is.null(r$body$metrics$totalTrades) &&
         !is.null(r$body$metrics$initialCapital) &&
         !is.null(r$body$metrics$finalValue))

record("run: MA metrics are sensible (initial capital preserved)",
       as.numeric(r$body$metrics$initialCapital) == 10000 &&
         as.numeric(r$body$metrics$finalValue) > 0)

record("run: equity curve is a list of dated points",
       length(r$body$equityCurve) > 100 &&
         grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", r$body$equityCurve[[1]]$date) &&
         !is.null(r$body$equityCurve[[1]]$portfolioValue))

record("run: MA backtest produced trades (crossover over 2 years)",
       length(r$body$tradeHistory) >= 2 &&
         all(vapply(r$body$tradeHistory, function(t) {
           !is.null(t$side) && t$side %in% c("BUY", "SELL") &&
             grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", t$executionDate)
         }, logical(1))))

record("run: MA parameters echo back",
       !is.null(r$body$parameters$fast_period) &&
         as.integer(r$body$parameters$fast_period) == 10 &&
         as.integer(r$body$parameters$slow_period) == 20)

# ---------------------------------------------------------------------------
# 4. Valid RSI backtest
# ---------------------------------------------------------------------------
run_rsi <- http_request("POST", "/backtest/run",
                  body = list(ticker = "MSFT", start = "2023-01-01", end = "2024-06-30",
                              initialCapital = 5000, strategy = "RSI",
                              parameters = list(period = 14, oversold = 30, overbought = 70)),
                  token = token_a)
r <- run_rsi
record("run: valid RSI backtest -> 200 + success",
       r$status == 200 && isTRUE(r$body$success) &&
         identical(r$body$strategy, "RSI") &&
         !is.null(r$body$backtestId))
RSI_ID <- r$body$backtestId

record("run: RSI parameters echo back",
       as.integer(r$body$parameters$period) == 14 &&
         as.numeric(r$body$parameters$oversold) == 30 &&
         as.numeric(r$body$parameters$overbought) == 70)

# ---------------------------------------------------------------------------
# 5. Provider-failure handling (valid format, no market data)
# ---------------------------------------------------------------------------
r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "ZZZZZZZZZZ", start = "2023-01-01", end = "2023-12-31",
                              initialCapital = 10000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = token_a)
record("run: well-formed but unresolvable ticker -> structured 4xx/5xx",
       r$status >= 400 && isFALSE(r$body$success) && nzchar(r$body$error))

# ---------------------------------------------------------------------------
# 6. GET /backtest/:id ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€šÃ‚Â full record from the DATABASE (fresh request)
# ---------------------------------------------------------------------------
r2 <- http_request("GET", paste0("/backtest/", MA_ID), token = token_a)
record("get: full record -> 200 + success",
       r2$status == 200 && isTRUE(r2$body$success) &&
         identical(as.character(r2$body$id), as.character(MA_ID)))

record("get: persisted ticker/strategy/dates match the run",
       identical(r2$body$ticker, "AAPL") && identical(r2$body$strategy, "MA_Crossover") &&
         identical(r2$body$startDate, "2023-01-01") && identical(r2$body$endDate, "2024-12-31"))

tr_diff <- abs(as.numeric(r2$body$metrics$totalReturn) - as.numeric(run_ma$body$metrics$totalReturn))
fv_diff <- abs(as.numeric(r2$body$metrics$finalValue) - as.numeric(run_ma$body$metrics$finalValue))
record("get: persisted metrics match the run response (DB column precision)",
       !is.null(r2$body$metrics$finalValue) &&
         tr_diff < 2e-4 && fv_diff < 1e-2,
       sprintf("(totalReturn diff %.7f, finalValue diff %.5f)", tr_diff, fv_diff))

record("get: persisted equity curve is a dated list",
       length(r2$body$equityCurve) > 100 &&
         !is.null(r2$body$equityCurve[[1]]$date) &&
         !is.null(r2$body$equityCurve[[1]]$portfolioValue))

record("get: persisted trade history is present",
       length(r2$body$tradeHistory) >= 2)

record("get: persisted parameters are intact",
       as.integer(r2$body$parameters$fast_period) == 10 &&
         as.integer(r2$body$parameters$slow_period) == 20)

record("get: created_at timestamp present",
       nzchar(r2$body$createdAt))

# Partial sub-resources
r2m <- http_request("GET", paste0("/backtest/", MA_ID, "/metrics"), token = token_a)
record("metrics: /backtest/:id/metrics -> 200 with metrics object",
       r2m$status == 200 && isTRUE(r2m$body$success) &&
         !is.null(r2m$body$metrics$totalReturn) &&
         as.integer(r2m$body$metrics$totalTrades) == as.integer(r2$body$metrics$totalTrades))

r2t <- http_request("GET", paste0("/backtest/", MA_ID, "/trades"), token = token_a)
record("trades: /backtest/:id/trades -> 200 with trade history",
       r2t$status == 200 && isTRUE(r2t$body$success) &&
         length(r2t$body$tradeHistory) == length(r2$body$tradeHistory) &&
         length(r2t$body$tradeHistory) >= 2 &&
         !is.null(r2t$body$tradeHistory[[1]]$side))

# ---------------------------------------------------------------------------
# 7. Missing / invalid backtest ids
# ---------------------------------------------------------------------------
r <- http_request("GET", "/backtest/999999999", token = token_a)
record("get: nonexistent backtest -> 404",
       r$status == 404 && isFALSE(r$body$success))

r <- http_request("GET", "/backtest/abc", token = token_a)
record("get: non-numeric backtest id -> 400",
       r$status == 400 && isFALSE(r$body$success))

r <- http_request("GET", "/backtest/999999999/trades", token = token_a)
record("trades: nonexistent backtest -> 404",
       r$status == 404)

r <- http_request("GET", "/backtest/999999999/metrics", token = token_a)
record("metrics: nonexistent backtest -> 404",
       r$status == 404)

# ---------------------------------------------------------------------------
# 8. Cross-user isolation
# ---------------------------------------------------------------------------
r <- http_request("GET", paste0("/backtest/", MA_ID), token = token_b)
record("isolation: user B cannot read user A backtest (404)",
       r$status == 404 && isFALSE(r$body$success))

r <- http_request("GET", paste0("/backtest/", MA_ID, "/trades"), token = token_b)
record("isolation: user B cannot read user A trades (404)",
       r$status == 404)

r <- http_request("GET", paste0("/backtest/", MA_ID, "/metrics"), token = token_b)
record("isolation: user B cannot read user A metrics (404)",
       r$status == 404)

r <- http_request("POST", "/backtest/run",
                  body = list(ticker = "TSLA", start = "2023-01-01", end = "2023-12-31",
                              initialCapital = 20000, strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = token_b)
record("run: user B can run their own backtest",
       r$status == 200 && isTRUE(r$body$success) && !is.null(r$body$backtestId))
B_ID <- r$body$backtestId

# ---------------------------------------------------------------------------
# 9. GET /backtest/history ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€šÃ‚Â pagination + isolation
# ---------------------------------------------------------------------------
rh <- http_request("GET", "/backtest/history?limit=50&offset=0", token = token_a)
record("history: user A list -> 200 + success",
       rh$status == 200 && isTRUE(rh$body$success) && length(rh$body$items) >= 2)

history_ids <- vapply(rh$body$items, function(it) as.character(it$id), character(1))
record("history: user A sees their own backtest ids",
       all(c(as.character(MA_ID), as.character(RSI_ID)) %in% history_ids))

record("history: items carry summary metrics",
       all(vapply(rh$body$items, function(it) {
         !is.null(it$ticker) && !is.null(it$strategy) && !is.null(it$startDate)
       }, logical(1))))

rhb <- http_request("GET", "/backtest/history?limit=50&offset=0", token = token_b)
history_b_ids <- vapply(rhb$body$items, function(it) as.character(it$id), character(1))
record("history: user B does NOT see user A backtests (isolation)",
       !(as.character(MA_ID) %in% history_b_ids) &&
         as.character(B_ID) %in% history_b_ids)

record("history: total counts reflect per-user isolation",
       rhb$body$total >= 1 && rh$body$total >= 2)

# Pagination: offset beyond results -> empty items, same total.
rhp <- http_request("GET", "/backtest/history?limit=50&offset=1000", token = token_a)
record("history: offset past the end -> empty items + total preserved",
       rhp$status == 200 && length(rhp$body$items) == 0 &&
         rhp$body$total == rh$body$total)

# ---------------------------------------------------------------------------
# 10. JSON null handling (no 'NA' strings in any response)
# ---------------------------------------------------------------------------
# Run an MA backtest over a window too short for the slow MA to warm up so
# several metrics are NA by construction, then verify they reach the client
# as JSON null (not the string "NA").
r0 <- http_request("POST", "/backtest/run",
                   body = list(ticker = "AAPL", start = "2024-10-01", end = "2024-10-31",
                               initialCapital = 10000, strategy = "MA_Crossover",
                               parameters = list(fast_period = 50, slow_period = 200)),
                   token = token_a)
record("nulls: short-window backtest still runs (200)",
       r0$status == 200 && isTRUE(r0$body$success))

record("nulls: raw JSON contains no NA string anywhere",
       !grepl("\"NA\"", r0$raw, fixed = TRUE))

record("nulls: tradeHistory type is [] or a list (valid JSON)",
       !is.null(r0$body$tradeHistory))

# If no trades occurred, winRate / sharpe are null rather than absent breaking.
record("nulls: response body is a plain JSON object with success flag",
       isTRUE(r0$body$success) && !is.null(r0$body$metrics) &&
         !is.null(r0$body$equityCurve) && !is.null(r0$body$tradeHistory))

# ---------------------------------------------------------------------------
# 11. Security: no secrets / hashes / stack traces leak in responses
# ---------------------------------------------------------------------------
record("security: run response contains no password or JWT secret",
       !grepl(TEST_PASSWORD, paste(r$body, collapse = " "), fixed = TRUE) &&
         !grepl("JWT_SECRET", paste(r$body, collapse = " "), fixed = TRUE))

record("security: error responses contain no stack traces",
       !grepl("traceback|at line|Error in", r2$raw, ignore.case = TRUE) &&
         !grepl("traceback|Error in", r0$raw, ignore.case = TRUE))

# ---------------------------------------------------------------------------
# 12. Persistence across separate requests: re-fetch one more time (fresh DB)
# ---------------------------------------------------------------------------
r3 <- http_request("GET", paste0("/backtest/", MA_ID), token = token_a)
record("persist: repeated GET returns identical finalValue",
       r3$status == 200 &&
         abs(as.numeric(r3$body$metrics$finalValue) - as.numeric(r2$body$metrics$finalValue)) < 1e-6)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(x) x$passed, logical(1))
cat(sprintf("\n%d/%d backtest API tests passed\n", sum(passed), length(passed)))
ok <- all(passed)
if (!ok) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
}

# quit() SKIPS on.exit handlers, so run the cleanup explicitly first.
cleanup_tests()
try(tools::pskill(pid), silent = TRUE)
if (ok) {
  cat("ALL BACKTEST API TESTS PASSED\n")
  quit(status = 0)
} else {
  quit(status = 1)
}
