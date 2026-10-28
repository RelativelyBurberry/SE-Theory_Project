# AlphaForge Insight — live verification of the GET /backtest/:id and
# GET /backtest/history retrieval path with a VALID frontend JWT.
#
# Purpose: reproduce, at the HTTP level, exactly what the React app does after
# a hard browser refresh at /results/:id and /dashboard:
#   1. The refresh has NO state except the JWT persisted in localStorage by
#      AuthContext and the backtest id in the URL.
#   2. The Results page calls GET /backtest/:id with `Authorization: Bearer`
#      sourced from localStorage; the Dashboard calls GET /backtest/history.
#   3. The token is obtained HERE through the REAL POST /auth/login flow, so
#      it is byte-for-byte the same kind of JWT the SPA holds.
#
# Checks:
#   - GET /backtest/43 with valid JWT  -> authenticated (NOT 401) response.
#     id 43 belongs to another account, so the correct answer is a structured
#     404 "Backtest not found." (per-user isolation), NOT a 401. The frontend
#     renders every non-401 error via the results page's error state.
#   - POST /backtest/run (new, valid run) -> backtestId
#   - GET /backtest/<new id> with valid JWT -> 200 full persisted record
#     (proves persist -> retrieve purely from the URL id = hard-refresh path).
#   - GET /backtest/history with valid JWT -> 200 + the new id in items.
#   - Control: GET /backtest/43 WITHOUT a token -> 401 (the SPA sends one,
#     so this never happens in the app).
#
# Run from the repository root:
#   Rscript tests/backend/verify_backtest_persistence_api.R
# Exit code 0 = pass, 1 = fail. Test user is deleted on exit.

PORT <- 8768L

# ---------------------------------------------------------------------------
# JWT secret resolution (mirrors the other backend API tests).
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
  TEST_JWT_SECRET <- "persist-verify-test-secret-0123456789abcdef"
}

TEST_EMAIL <- paste0("persist_verify_", as.integer(Sys.time()), "@test.local")
TEST_PASSWORD <- "persist-verify-pass-123"

# ---------------------------------------------------------------------------
# HTTP helper.
# ---------------------------------------------------------------------------
library(curl)
library(jsonlite)

http_request <- function(method, path, body = NULL, token = NULL) {
  url <- paste0("http://127.0.0.1:", PORT, path)
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

results <- list()
record <- function(name, passed, detail = "") {
  ok <- isTRUE(passed)
  results[[length(results) + 1L]] <<- list(name = name, passed = ok, detail = detail)
  cat(sprintf("%s %s %s\n", if (ok) "PASS" else "FAIL", name, detail))
  invisible(ok)
}

# ---------------------------------------------------------------------------
# Start the API (current code on disk).
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

# Cleanup: delete the test user (backtests + results cascade).
cleanup_tests <- function() {
  con <- tryCatch({
    src_env <- new.env(parent = globalenv())
    sys.source(file.path("backend", "database", "connection.R"), envir = src_env)
    src_env$get_db_connection()
  }, error = function(e) NULL)
  if (!is.null(con)) {
    on.exit(src_env$close_db_connection(con), add = TRUE)
    try(DBI::dbExecute(con,
      "DELETE FROM users WHERE email = $1",
      params = list(TEST_EMAIL)), silent = TRUE)
  }
}
on.exit(cleanup_tests(), add = TRUE)

# ---------------------------------------------------------------------------
# Obtain a REAL frontend JWT through the exact flow the SPA uses.
# ---------------------------------------------------------------------------
reg <- http_request("POST", "/auth/register",
                    body = list(email = TEST_EMAIL, password = TEST_PASSWORD))
if (reg$status != 201L) {
  cat("FAIL: could not register test user (", reg$status, ")\n")
  quit(status = 1)
}
login <- http_request("POST", "/auth/login",
                      body = list(email = TEST_EMAIL, password = TEST_PASSWORD))
record("login: real POST /auth/login issues a JWT",
       login$status == 200L && isTRUE(login$body$success) && nzchar(login$body$token))
TOKEN <- login$body$token
cat("JWT obtained (real login), length:", nchar(TOKEN), "\n")

# ---------------------------------------------------------------------------
# 1. GET /backtest/43 with a valid JWT — the exact request the Results page
#    makes at /results/43 after a hard refresh.
# ---------------------------------------------------------------------------
r <- http_request("GET", "/backtest/43")
record("control: GET /backtest/43 with NO token -> 401 (protected)",
       r$status == 401 && isFALSE(r$body$success))

r <- http_request("GET", "/backtest/43", token = TOKEN)
cat(sprintf("GET /backtest/43 WITH valid JWT -> HTTP %d error=%s\n",
            r$status,
            if (!is.null(r$body$error)) r$body$error else "(none)"))
record("get43: GET /backtest/43 with valid JWT is AUTHENTICATED (NOT 401)",
       r$status != 401)
record("get43: id 43 is scoped to another account -> structured 404 (user isolation)",
       r$status == 404 && isFALSE(r$body$success) &&
         grepl("Backtest not found", r$body$error))

r <- http_request("GET", "/backtest/history")
record("control: GET /backtest/history with NO token -> 401 (protected)",
       r$status == 401)

# ---------------------------------------------------------------------------
# 2. Full persist -> retrieve round trip with a NEW backtest id.
# ---------------------------------------------------------------------------
run <- http_request("POST", "/backtest/run",
                    body = list(ticker = "AAPL", start = "2023-01-01", end = "2024-12-31",
                                initialCapital = 10000, strategy = "MA_Crossover",
                                parameters = list(fast_period = 10, slow_period = 20)),
                    token = TOKEN)
record("run: valid MA backtest -> 200 + real backtestId",
       run$status == 200 && isTRUE(run$body$success) &&
         !is.null(run$body$backtestId))
NEW_ID <- as.character(run$body$backtestId)
cat("New backtest id:", NEW_ID, "\n")

# Simulate the browser reopened AT /results/<id>: the ONLY inputs are the URL
# id and the localStorage JWT. No in-memory state survives a refresh.
r2 <- http_request("GET", paste0("/backtest/", NEW_ID), token = TOKEN)
record("getNew: GET /backtest/<newId> with valid JWT -> 200 full record",
       r2$status == 200 && isTRUE(r2$body$success) &&
         identical(as.character(r2$body$id), NEW_ID))
record("getNew: persisted record matches the run (ticker/strategy/dates)",
       identical(r2$body$ticker, "AAPL") &&
         identical(r2$body$strategy, "MA_Crossover") &&
         identical(r2$body$startDate, "2023-01-01") &&
         identical(r2$body$endDate, "2024-12-31"))
record("getNew: metrics + equity curve + trades persisted",
       !is.null(r2$body$metrics$totalReturn) &&
         length(r2$body$equityCurve) > 100 &&
         length(r2$body$tradeHistory) >= 2)

# ---------------------------------------------------------------------------
# 3. GET /backtest/history with the valid JWT (Dashboard after refresh).
# ---------------------------------------------------------------------------
rh <- http_request("GET", "/backtest/history?limit=20&offset=0", token = TOKEN)
record("history: GET /backtest/history with valid JWT -> 200 + items",
       rh$status == 200 && isTRUE(rh$body$success) && length(rh$body$items) >= 1)
history_ids <- vapply(rh$body$items, function(it) as.character(it$id), character(1))
record("history: the new run appears in history (persisted, visible post-refresh)",
       NEW_ID %in% history_ids)
record("history: summary fields ride along (ticker/strategy/dates)",
       all(vapply(rh$body$items, function(it) {
         !is.null(it$ticker) && !is.null(it$strategy) && !is.null(it$startDate)
       }, logical(1))))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(x) x$passed, logical(1))
cat(sprintf("\n%d/%d persistence API verification checks passed\n",
            sum(passed), length(passed)))
ok <- all(passed)
if (!ok) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
}

cleanup_tests()
try(tools::pskill(pid), silent = TRUE)
if (ok) {
  cat("BACKTEST RETRIEVAL PATH VERIFIED (valid JWT, no hardcoded id)\n")
  quit(status = 0)
} else {
  quit(status = 1)
}