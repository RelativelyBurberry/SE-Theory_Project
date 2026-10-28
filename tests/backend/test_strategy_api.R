# AlphaForge Insight — strategy API integration tests (Phase 4)
#
# LIVE tests: start the real Plumber API on a test port and make real HTTP
# requests. Strategy-preview valid-ticker cases hit Yahoo Finance (via
# quantmod) and therefore depend on network availability; validation and
# signal-generation logic is covered deterministically in tests/analytics/*.
#
# JWT handling matches Phase 2/3: a token is minted locally with jose using
# the same JWT_SECRET exported to the child API process. No database is
# needed for these endpoints (require_auth validates the token signature only).
#
# Run from the repository root:
#   Rscript tests/backend/test_strategy_api.R
#
# Exit code 0 = pass, 1 = fail.

PORT <- 8130L

# ---------------------------------------------------------------------------
# JWT secret resolution (mirrors plumber.R startup).
# ---------------------------------------------------------------------------
# plumber.R loads backend/.Renviron (if present) then the root .env (if
# present) via readRenviron, which OVERRIDES existing environment variables.
# So the child process validates tokens with the *real* configured secret.
# We mirror that here: read the same files and mint test tokens with the
# resulting secret so signature validation succeeds.
repo_root <- getwd()  # tests are run from the repository root
if (file.exists(file.path(repo_root, "backend", ".Renviron"))) {
  readRenviron(file.path(repo_root, "backend", ".Renviron"))
}
if (file.exists(file.path(repo_root, ".env"))) {
  readRenviron(file.path(repo_root, ".env"))
}

TEST_JWT_SECRET <- Sys.getenv("JWT_SECRET", unset = "")
if (!nzchar(TEST_JWT_SECRET)) {
  # No real secret configured: use a throwaway test secret. The child process
  # (which also has no configured secret) will validate with the same value
  # because we export it below via Sys.setenv.
  TEST_JWT_SECRET <- "strategy-api-test-secret-0123456789abcdef"
}
cat("JWT secret source:",
    if (nzchar(Sys.getenv("JWT_SECRET", unset = ""))) "real (.env/.Renviron)" else "throwaway test",
    "\n")

# ---------------------------------------------------------------------------
# HTTP helper (curl)
# ---------------------------------------------------------------------------
library(curl)
library(jsonlite)
library(jose)

http_request <- function(method, path, body = NULL, token = NULL,
                         base = NULL, is_json = TRUE) {
  if (is.null(base)) base <- paste0("http://127.0.0.1:", PORT)
  url <- paste0(base, path)
  h <- curl::new_handle()

  # IMPORTANT: curl::handle_setheaders REPLACES all previously set headers on
  # every call, so all headers must be set in a SINGLE call. Calling it twice
  # silently drops Content-Type (or Authorization), which makes Plumber parse
  # the JSON body with the wrong parser (observed: body fields vanish and the
  # endpoint reports "ticker is required").
  headers <- character(0)
  if (identical(toupper(method), "POST")) {
    curl::handle_setopt(h, customrequest = "POST", post = TRUE,
                        postfields = jsonlite::toJSON(body, auto_unbox = TRUE))
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
  # NA happens when a test expression returns NA instead of TRUE/FALSE; treat
  # it as a failure rather than crashing the whole suite.
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
on.exit(Sys.unsetenv(c("PLUMBER_PORT", "PLUMBER_HOST", "PLUMBER_PIDFILE", "JWT_SECRET")))

rscript <- file.path(R.home("bin"), "Rscript")
invisible(system2(rscript, c(file.path("backend", "plumber.R")),
                  wait = FALSE, stdout = outlog, stderr = errlog))

deadline <- Sys.time() + 60
while (!file.exists(pidfile) && Sys.time() < deadline) Sys.sleep(0.3)
if (!file.exists(pidfile)) {
  cat("FAIL: server did not start in time\n")
  quit(status = 1)
}
pid <- as.integer(readLines(pidfile, warn = FALSE))
on.exit(tools::pskill(pid), add = TRUE)

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

# Local JWT for protected calls (same secret the child process uses).
mint_token <- function(exp_delta = 3600) {
  claim <- jwt_claim(sub = "1", email = "strategy@test.local",
                     iat = as.numeric(Sys.time()),
                     exp = as.numeric(Sys.time()) + exp_delta)
  jwt_encode_hmac(claim, charToRaw(TEST_JWT_SECRET))
}
valid_token <- mint_token()

# ---------------------------------------------------------------------------
# 1. GET /strategies/defaults (public)
# ---------------------------------------------------------------------------
r <- http_request("GET", "/strategies/defaults")
record("defaults: returns 200",
       r$status == 200 && isTRUE(r$body$success))

record("defaults: MA Crossover defaults are 50 / 200",
       r$status == 200 &&
         as.numeric(r$body$defaults$ma_crossover$fast_period) == 50 &&
         as.numeric(r$body$defaults$ma_crossover$slow_period) == 200)

record("defaults: RSI defaults are 14 / 30 / 70",
       r$status == 200 &&
         as.numeric(r$body$defaults$rsi$period) == 14 &&
         as.numeric(r$body$defaults$rsi$oversold) == 30 &&
         as.numeric(r$body$defaults$rsi$overbought) == 70)

# ---------------------------------------------------------------------------
# 2. POST /strategies/preview authentication
# ---------------------------------------------------------------------------
r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01",
                              end = "2024-03-31", strategy = "MA_Crossover",
                              parameters = list(fast_period = 50, slow_period = 200)))
record("preview: missing JWT returns 401",
       r$status == 401 && isFALSE(r$body$success))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01",
                              end = "2024-03-31", strategy = "MA_Crossover",
                              parameters = list(fast_period = 50, slow_period = 200)),
                  token = "not.a.real.jwt")
record("preview: forged JWT returns 401",
       r$status == 401)

# ---------------------------------------------------------------------------
# 3. POST /strategies/preview — MA Crossover
# ---------------------------------------------------------------------------
r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01",
                              end = "2024-03-31", strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = valid_token)
record("preview: valid MA config returns 200 with data",
       r$status == 200 && isTRUE(r$body$success) &&
         identical(r$body$ticker, "AAPL") &&
         identical(r$body$strategy, "MA_Crossover") &&
         length(r$body$signals) > 0)

record("preview: MA rows expose fast_sma / slow_sma / close / date",
       any(vapply(r$body$signals, function(row) {
         !is.null(row$fast_sma) && !is.null(row$slow_sma) &&
           !is.null(row$close) && !is.null(row$date)
       }, logical(1))))

record("preview: MA signal values are only BUY/SELL (or absent)",
       all(vapply(r$body$signals, function(row) {
         is.null(row$signal) || identical(row$signal, "BUY") ||
           identical(row$signal, "SELL")
       }, logical(1))))

record("preview: date format is YYYY-MM-DD",
       all(vapply(r$body$signals, function(row) {
         grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", row$date)
       }, logical(1))))

# ---------------------------------------------------------------------------
# 4. POST /strategies/preview — RSI
# ---------------------------------------------------------------------------
r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01",
                              end = "2024-03-31", strategy = "RSI",
                              parameters = list(period = 14,
                                                oversold = 30, overbought = 70)),
                  token = valid_token)
record("preview: valid RSI config returns 200 with data",
       r$status == 200 && isTRUE(r$body$success) &&
         identical(r$body$strategy, "RSI") &&
         length(r$body$signals) > 0)

record("preview: RSI rows expose rsi / close / date",
       any(vapply(r$body$signals, function(row) {
         !is.null(row$rsi) && !is.null(row$close) && !is.null(row$date)
       }, logical(1))))

record("preview: RSI signal values are only BUY/SELL (or absent)",
       all(vapply(r$body$signals, function(row) {
         is.null(row$signal) || identical(row$signal, "BUY") ||
           identical(row$signal, "SELL")
       }, logical(1))))

# ---------------------------------------------------------------------------
# 5. POST /strategies/preview — validation rejections
# ---------------------------------------------------------------------------
r <- http_request("POST", "/strategies/preview",
                  body = list(start = "2024-01-01", end = "2024-03-31",
                              strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = valid_token)
record("preview: missing ticker -> 400",
       r$status == 400 && isFALSE(r$body$success) && nzchar(r$body$error))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-06-01", end = "2024-01-01",
                              strategy = "MA_Crossover",
                              parameters = list(fast_period = 10, slow_period = 20)),
                  token = valid_token)
record("preview: start > end -> 400",
       r$status == 400 && isFALSE(r$body$success) &&
         grepl("on or before", r$body$error))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01", end = "2024-03-31",
                              strategy = "MA_Crossover",
                              parameters = list(fast_period = 200, slow_period = 50)),
                  token = valid_token)
record("preview: fast >= slow rejected (never swapped) -> 400",
       r$status == 400 && isFALSE(r$body$success) &&
         grepl("strictly less than", r$body$error))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01", end = "2024-03-31",
                              strategy = "MA_Crossover",
                              parameters = list(fast_period = 0, slow_period = 50)),
                  token = valid_token)
record("preview: fast_period <= 0 -> 400",
       r$status == 400 && isFALSE(r$body$success) &&
         grepl("positive integer", r$body$error))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01", end = "2024-03-31",
                              strategy = "RSI",
                              parameters = list(period = 14,
                                                oversold = 70, overbought = 30)),
                  token = valid_token)
record("preview: oversold >= overbought -> 400",
       r$status == 400 && isFALSE(r$body$success) &&
         grepl("strictly less than", r$body$error))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01", end = "2024-03-31",
                              strategy = "RSI",
                              parameters = list(period = 14,
                                                oversold = 0, overbought = 70)),
                  token = valid_token)
record("preview: oversold <= 0 -> 400",
       r$status == 400 && isFALSE(r$body$success) &&
         grepl("greater than zero", r$body$error))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01", end = "2024-03-31",
                              strategy = "RSI",
                              parameters = list(period = 14,
                                                oversold = 30, overbought = 100)),
                  token = valid_token)
record("preview: overbought >= 100 -> 400",
       r$status == 400 && isFALSE(r$body$success) &&
         grepl("less than 100", r$body$error))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01", end = "2024-03-31",
                              strategy = "RSI",
                              parameters = list(period = 0,
                                                oversold = 30, overbought = 70)),
                  token = valid_token)
record("preview: RSI period <= 0 -> 400",
       r$status == 400 && isFALSE(r$body$success) &&
         grepl("positive integer", r$body$error))

r <- http_request("POST", "/strategies/preview",
                  body = list(ticker = "AAPL", start = "2024-01-01", end = "2024-03-31",
                              strategy = "BOGUS", parameters = list()),
                  token = valid_token)
record("preview: unsupported strategy -> 400",
       r$status == 400 && isFALSE(r$body$success) &&
         grepl("Unsupported strategy", r$body$error))

# ---------------------------------------------------------------------------
# 6. Regression: /health phase banner reflects Phase 6
# ---------------------------------------------------------------------------
r <- http_request("GET", "/health")
record("health: phase banner is 6-backtest-api",
       r$status == 200 && identical(r$body$phase, "6-backtest-api"))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(x) x$passed, logical(1))
cat(sprintf("\n%d/%d strategy API tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL STRATEGY API TESTS PASSED\n")
quit(status = 0)