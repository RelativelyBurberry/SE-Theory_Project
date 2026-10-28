#!/usr/bin/env Rscript
# =============================================================================
# E2E SMOKE TEST - Phase 6 (full-stack)
# =============================================================================
# AAPL 2015-01-01 through 2025-12-31 | $10,000 | MA_Crossover 50/200 | 0% cost
#
# Prerequisites:
#   - backend/ .env with DATABASE_URL and JWT_SECRET
#   - Yahoo Finance API accessible from this machine (quantmod + library(curl))
#
# What this test covers:
#   1.  Register + login a real user through /auth/register + /auth/login
#   2.  POST /backtest/run with real Yahoo data over a decade-long window
#   3.  Validate response shape, metrics, equity curve, trade history
#   4.  GET /backtest/:id - persisted record matches
#   5.  GET /backtest/:id/metrics
#   6.  GET /backtest/:id/trades
#   7.  GET /backtest/history - record appears in the list
#   8.  Neon DB row exists with initial_capital/final_value columns
#   9.  Full cleanup of the test user (cascades via ON DELETE CASCADE)
# =============================================================================

cat("================================================================\n")
cat("  ALPHAFORGE INSIGHT - PHASE 6 E2E SMOKE TEST\n")
cat("  AAPL 2015-01-01 -> 2025-12-31 | MA 50/200 | $10,000\n")
cat("================================================================\n\n")

PORT <- 8160L
passed <- 0L
failed <- 0L
failures <- character(0)

record <- function(name, cond, detail = "") {
  if (isTRUE(cond)) {
    passed <<- passed + 1L
    cat("PASS:", name, "\n")
  } else {
    failed <<- failed + 1L
    msg <- paste0("FAIL: ", name)
    if (nzchar(detail)) msg <- paste0(msg, " - ", detail)
    cat(msg, "\n")
    failures <<- c(failures, msg)
  }
}

# --- Environment -------------------------------------------------------------
if (file.exists('backend/.Renviron')) readRenviron('backend/.Renviron')
if (file.exists('.env')) readRenviron('.env')
E2E_JWT_SECRET <- Sys.getenv('JWT_SECRET', unset = '')
if (!nzchar(E2E_JWT_SECRET)) stop('JWT_SECRET not set; cannot run E2E test')
E2E_DB_URL <- Sys.getenv('DATABASE_URL', unset = '')
if (!nzchar(E2E_DB_URL)) stop('DATABASE_URL not set; cannot run E2E test')

cat("JWT secret source: real (.env/.Renviron)\n")
cat("Database URL present:", nzchar(E2E_DB_URL), "\n\n")

# --- Start backend -----------------------------------------------------------
if (!requireNamespace('curl', quietly = TRUE)) install.packages('curl', repos = 'https://cloud.r-project.org')
library(curl)
if (!requireNamespace('jsonlite', quietly = TRUE)) install.packages('jsonlite', repos = 'https://cloud.r-project.org')
library(jsonlite)
if (!requireNamespace('openssl', quietly = TRUE)) install.packages('openssl', repos = 'https://cloud.r-project.org')
library(openssl)

http <- function(method, path, body = NULL, token = NULL) {
  url <- paste0('http://127.0.0.1:', PORT, path)
  h <- curl::new_handle()
  headers <- character(0)
  if (identical(toupper(method), 'POST')) {
    payload <- if (is.null(body)) '{}' else toJSON(body, auto_unbox = TRUE)
    curl::handle_setopt(h, customrequest = 'POST', post = TRUE, postfields = payload)
    headers <- c(headers, 'Content-Type' = 'application/json')
  } else if (identical(toupper(method), 'GET')) {
    curl::handle_setopt(h, customrequest = 'GET')
  }
  if (!is.null(token)) headers <- c(headers, 'Authorization' = paste0('Bearer ', token))
  if (length(headers) > 0) do.call(curl::handle_setheaders, c(list(h = h), as.list(headers)))
  resp <- curl::curl_fetch_memory(url, handle = h)
  raw <- rawToChar(resp$content)
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  list(status = as.integer(resp$status_code), body = parsed, raw = raw)
}

pidfile <- tempfile(fileext = '.pid')
Sys.setenv(PLUMBER_PORT = as.character(PORT), PLUMBER_HOST = '127.0.0.1',
           PLUMBER_PIDFILE = pidfile, JWT_SECRET = E2E_JWT_SECRET)
on.exit({
  Sys.unsetenv(c('PLUMBER_PORT', 'PLUMBER_HOST', 'PLUMBER_PIDFILE', 'JWT_SECRET'))
}, add = TRUE)

cat('Starting backend on port', PORT, '...\n')
invisible(system2(file.path(R.home('bin'), 'Rscript'), c('backend/plumber.R'),
                  wait = FALSE, stdout = tempfile(), stderr = tempfile()))

deadline <- Sys.time() + 120
while (!file.exists(pidfile) && Sys.time() < deadline) Sys.sleep(0.5)
pid <- as.integer(readLines(pidfile, warn = FALSE))
on.exit(try(tools::pskill(pid), silent = TRUE), add = TRUE)

ready <- FALSE
while (Sys.time() < deadline) {
  r <- tryCatch(http('GET', '/health'), error = function(e) NULL)
  if (!is.null(r) && r$status == 200L) { ready <- TRUE; break }
  Sys.sleep(1)
}
cat('Backend ready:', ready, '\n\n')
if (!ready) stop('Backend failed to start within timeout')

# --- Register + login --------------------------------------------------------
E2E_EMAIL <- paste0('e2e_phase6_', format(Sys.time(), '%H%M%S'), '@test.local')
E2E_PASSWORD <- 'e2e-phase6-strong-pass'

reg <- http('POST', '/auth/register', body = list(email = E2E_EMAIL, password = E2E_PASSWORD))
record('register: 201 + success', reg$status == 201L && isTRUE(reg$body$success))
e2e_uid <- reg$body$user$id

login <- http('POST', '/auth/login', body = list(email = E2E_EMAIL, password = E2E_PASSWORD))
record('login: 200 + JWT returned',
       login$status == 200L && isTRUE(login$body$success) &&
         is.character(login$body$token) && nzchar(login$body$token))
jwt <- login$body$token

me <- http('GET', '/auth/me', token = jwt)
record('auth/me: returns the registered user', me$status == 200L && me$body$user$email == E2E_EMAIL)

# --- Run the decade-long backtest --------------------------------------------
cat('\nRunning backtest: AAPL 2015-01-01 -> 2025-12-31, MA 50/200, $10k...\n')
t0 <- proc.time()
run <- http('POST', '/backtest/run', token = jwt, body = list(
  ticker = 'AAPL',
  start = '2015-01-01',
  end = '2025-12-31',
  initialCapital = 10000,
  strategy = 'MA_Crossover',
  parameters = list(fast_period = 50L, slow_period = 200L)
))
elapsed <- (proc.time() - t0)[['elapsed']]
cat(sprintf('  [%.1fs elapsed]\n', elapsed))

record('run: 200 + success', run$status == 200L && isTRUE(run$body$success))

bid <- run$body$backtestId
record('run: backtestId is a positive integer', !is.null(bid) && bid > 0)

m <- run$body$metrics
record('run: metrics.initialCapital == 10000', identical(as.numeric(m$initialCapital), 10000))
record('run: metrics.finalValue is a finite number',
       is.numeric(m$finalValue) && length(m$finalValue) == 1L && is.finite(m$finalValue) && m$finalValue > 0)
record('run: metrics.totalReturn is finite', is.finite(as.numeric(m$totalReturn)))
record('run: metrics.sharpe is finite', is.finite(as.numeric(m$sharpe)))
record('run: metrics.maximumDrawdown is negative', as.numeric(m$maximumDrawdown) < 0)
record('run: metrics.winRate in [0,1]', as.numeric(m$winRate) >= 0 && as.numeric(m$winRate) <= 1)
record('run: metrics.totalTrades > 0', as.integer(m$totalTrades) > 0L)
cat(sprintf('  Final value: $%.2f | Total return: %.2f%% | Sharpe: %.2f | Trades: %d\n',
            m$finalValue, m$totalReturn * 100, m$sharpe, m$totalTrades))

eq <- run$body$equityCurve
record('run: equityCurve is a non-empty list', is.list(eq) && length(eq) > 100)
record('run: first equity point has date + portfolioValue',
       is.character(eq[[1]]$date) && is.numeric(eq[[1]]$portfolioValue))
last_eq <- eq[[length(eq)]]
record('run: last equity date is the final 2025 trading day',
       last_eq$date >= '2025-12-29' && last_eq$date <= '2025-12-31')
record('run: last portfolio value matches finalValue',
       abs(as.numeric(last_eq$portfolioValue) - m$finalValue) < 1)

th <- run$body$tradeHistory
record('run: tradeHistory is a non-empty list', is.list(th) && length(th) > 0)
record('run: first trade has side/date/price', nzchar(th[[1]]$side) &&
         nzchar(th[[1]]$executionDate) && is.numeric(th[[1]]$executionPrice))

record('run: parameters echo back correctly',
       run$body$parameters$fast_period == 50L && run$body$parameters$slow_period == 200L)

# --- GET /backtest/:id -------------------------------------------------------
cat('\nFetching persisted record via GET /backtest/', bid, '...\n', sep = '')
get <- http('GET', paste0('/backtest/', bid), token = jwt)
record('get: 200 + success', get$status == 200L && isTRUE(get$body$success))
record('get: ticker/strategy match', get$body$ticker == 'AAPL' && get$body$strategy == 'MA_Crossover')
record('get: finalValue present and matches',
       is.numeric(get$body$metrics$finalValue) &&
         abs(get$body$metrics$finalValue - m$finalValue) < 1e-2)
record('get: totalReturn present and matches',
       abs(as.numeric(get$body$metrics$totalReturn) - as.numeric(m$totalReturn)) < 2e-4)

# --- GET /backtest/:id/metrics -----------------------------------------------
met <- http('GET', paste0('/backtest/', bid, '/metrics'), token = jwt)
record('metrics: 200 + success', met$status == 200L && isTRUE(met$body$success))
record('metrics: all 8 fields present',
       all(vapply(c('totalReturn','cagr','sharpe','sortino','maximumDrawdown',
                     'winRate','volatility','totalTrades'), function(k)
         !is.null(met$body$metrics[[k]]), logical(1))))

# --- GET /backtest/:id/trades ------------------------------------------------
tr <- http('GET', paste0('/backtest/', bid, '/trades'), token = jwt)
record('trades: 200 + success', tr$status == 200L && isTRUE(tr$body$success))
record('trades: tradeHistory matches',
       length(tr$body$tradeHistory) == length(th))

# --- GET /backtest/history ---------------------------------------------------
hist <- http('GET', '/backtest/history', token = jwt)
record('history: 200 + success', hist$status == 200L && isTRUE(hist$body$success))
ids_in_history <- vapply(hist$body$items, function(x) x$id, integer(1))
record('history: backtest id appears in the list', bid %in% ids_in_history)
record('history: items carry the backtestId',
       any(vapply(hist$body$items, function(x) x$id == bid, logical(1))))

# --- Neon DB verification ----------------------------------------------------
cat('\nVerifying Neon persistence (direct DB query)...\n')
suppressMessages({ library(DBI); library(RPostgres) })
src_env <- new.env(parent = globalenv())
sys.source(file.path('backend', 'database', 'connection.R'), envir = src_env)
con <- src_env$get_db_connection()
on.exit(src_env$close_db_connection(con), add = TRUE)

db_backtest <- dbGetQuery(con, 'SELECT * FROM backtests WHERE id = $1', params = list(bid))
record('db: backtest row exists', nrow(db_backtest) == 1L)
record('db: backtest ticker = AAPL', db_backtest$ticker == 'AAPL')
record('db: backtest user_id matches', db_backtest$user_id == e2e_uid)

db_result <- dbGetQuery(con, 'SELECT * FROM results WHERE backtest_id = $1', params = list(bid))
record('db: results row exists', nrow(db_result) == 1L)
record('db: initial_capital column present',
       'initial_capital' %in% names(db_result) && !is.na(db_result$initial_capital))
record('db: final_value column present',
       'final_value' %in% names(db_result) && !is.na(db_result$final_value))
record('db: results.initial_capital = 10000',
       as.numeric(db_result$initial_capital) == 10000)
record('db: results.final_value matches API',
       abs(as.numeric(db_result$final_value) - m$finalValue) < 1e-2)

# --- Security quick-check ----------------------------------------------------
no_jwt <- http('GET', paste0('/backtest/', bid))
record('security: unauthenticated GET returns 401', no_jwt$status == 401L)

run_raw <- run$raw
record('security: no password or JWT secret in run response',
       !grepl('password', run_raw, ignore.case = TRUE) &&
         !grepl(E2E_JWT_SECRET, run_raw, fixed = TRUE))

# --- Cleanup -----------------------------------------------------------------
cat('\nCleaning up test user...\n')
DBI::dbExecute(con, 'DELETE FROM users WHERE email = $1', params = list(E2E_EMAIL))
remaining <- DBI::dbGetQuery(con, 'SELECT COUNT(*) AS n FROM users WHERE email = $1',
                             params = list(E2E_EMAIL))
record('cleanup: test user deleted', remaining$n == 0L)

# --- Summary -----------------------------------------------------------------
cat('\n================================================================\n')
cat(sprintf('  E2E SMOKE TEST: %d/%d passed\n', passed, passed + failed))
cat('================================================================\n')
if (failed > 0) {
  cat('\nFailed tests:\n')
  for (f in failures) cat(' ', f, '\n')
  quit(status = 1)
}
cat('\nAll E2E smoke tests passed. Backend -> Neon -> Yahoo API -> Metrics all functional.\n')
quit(status = 0)