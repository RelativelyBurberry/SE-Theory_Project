# AlphaForge Insight — Phase 3 market-data API integration tests
#
# LIVE tests: they start the real Plumber API and make real HTTP requests.
# The /market-data valid-ticker cases hit Yahoo Finance (via quantmod) and
# therefore depend on network availability. Deterministic indicator and
# validation logic is covered separately by tests/analytics/*.R.
#
# JWT handling matches Phase 2: a token is minted locally with jose using the
# same JWT_SECRET exported to the child API process. No database is needed
# for these endpoints (require_auth validates the token signature only).
#
# Run from the repository root:
#   Rscript tests/backend/test_data_api.R
#
# Exit code 0 = pass, 1 = fail.

PORT <- 8128L

# ---------------------------------------------------------------------------
# JWT secret resolution (mirrors plumber.R startup).
# ---------------------------------------------------------------------------
# plumber.R loads backend/.Renviron (if present) then the root .env (if
# present) via readRenviron, which OVERRIDES existing environment variables.
# If the root .env sets JWT_SECRET, the child process validates tokens with
# that secret — so tokens must be minted with it for locally-valid JWT tests.
# (Discovered during Phase 4: once a root .env with JWT_SECRET was added, the
#  previously-fixed test secret stopped matching what the server validates.)
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
  TEST_JWT_SECRET <- "market-data-test-secret-0123456789abcdef"
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

http_request <- function(method, path, body = NULL, token = NULL, base = NULL) {
  if (is.null(base)) base <- paste0("http://127.0.0.1:", PORT)
  url <- paste0(base, path)
  h <- curl::new_handle()
  if (identical(toupper(method), "POST")) {
    curl::handle_setopt(h, customrequest = "POST", post = TRUE,
                        postfields = jsonlite::toJSON(body, auto_unbox = TRUE))
    curl::handle_setheaders(h, "Content-Type" = "application/json")
  }
  if (!is.null(token)) {
    curl::handle_setheaders(h, Authorization = paste0("Bearer ", token))
  }
  resp <- curl::curl_fetch_memory(url, handle = h)
  raw <- rawToChar(resp$content)
  # simplifyVector = FALSE keeps nested "data": [...] as row-objects so the
  # tests can inspect per-row fields without data.frame flattening.
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
  list(status = as.integer(resp$status_code), body = parsed, raw = raw)
}

# ---------------------------------------------------------------------------
results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
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
  claim <- jwt_claim(sub = "1", email = "market@test.local",
                     iat = as.numeric(Sys.time()),
                     exp = as.numeric(Sys.time()) + exp_delta)
  jwt_encode_hmac(claim, charToRaw(TEST_JWT_SECRET))
}
valid_token <- mint_token()

# ---------------------------------------------------------------------------
# 1. Regression: /health
# ---------------------------------------------------------------------------
r <- http_request("GET", "/health")
record("health: returns 200 (phase 6 build)",
       r$status == 200 && identical(r$body$status, "ok") &&
         identical(r$body$phase, "6-backtest-api"))

# ---------------------------------------------------------------------------
# 2. GET /stocks/validate (public)
# ---------------------------------------------------------------------------
r <- http_request("GET", "/stocks/validate?ticker=AAPL")
record("validate: valid ticker (AAPL) -> valid:true",
       r$status == 200 && isTRUE(r$body$valid) && identical(r$body$ticker, "AAPL"))

r <- http_request("GET", "/stocks/validate?ticker=msft")
record("validate: lowercase ticker handled (case-insensitive)",
       r$status == 200 && isTRUE(r$body$valid) && identical(r$body$ticker, "MSFT"))

r <- http_request("GET", "/stocks/validate?ticker=XYZABC99")
record("validate: non-existent ticker -> valid:false (not fabricated)",
       r$status == 200 && isFALSE(r$body$valid) && nzchar(r$body$error))

r <- http_request("GET", "/stocks/validate?ticker=!!!!")
record("validate: malformed ticker -> valid:false with error",
       r$status == 200 && isFALSE(r$body$valid) && nzchar(r$body$error))

r <- http_request("GET", "/stocks/validate")
record("validate: missing ticker param -> 400 structured error",
       r$status == 400 && isFALSE(r$body$success) && nzchar(r$body$error))

# ---------------------------------------------------------------------------
# 3. GET /market-data (protected)
# ---------------------------------------------------------------------------
r <- http_request("GET", "/market-data?ticker=AAPL&start=2024-01-01&end=2024-03-01")
record("market-data: no token -> 401 (auth enforced)",
       r$status == 401)

r <- http_request("GET", "/market-data?ticker=AAPL&start=2024-01-01&end=2024-03-01",
                  token = valid_token)
r_md <- r
record("market-data: valid request -> 200 success",
       r$status == 200 && isTRUE(r$body$success))
record("market-data: returns data array with count > 0",
       is.list(r$body$data) && is.numeric(r$body$count) && r$body$count > 0)
record("market-data: each row has required OHLCV fields",
       all(vapply(r$body$data, function(row) {
         all(c("date", "open", "high", "low", "close", "adjusted_close", "volume") %in% names(row))
       }, logical(1))))
record("market-data: dates serialized as YYYY-MM-DD",
       all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", vapply(r$body$data, `[[`, "", "date"))))
record("market-data: numeric fields are numbers",
       all(vapply(r$body$data, function(row) {
         all(vapply(c("open", "high", "low", "close", "adjusted_close", "volume"),
                    function(f) is.numeric(row[[f]]), logical(1)))
       }, logical(1))))
record("market-data: raw body is valid JSON",
       jsonlite::validate(r_md$raw))
record("market-data: results are chronologically ordered",
       all(diff(as.Date(vapply(r_md$body$data, `[[`, "", "date"))) > 0))

# Invalid ticker format -> 400
r <- http_request("GET", "/market-data?ticker=!!!!&start=2024-01-01&end=2024-03-01",
                  token = valid_token)
record("market-data: invalid ticker format -> 400",
       r$status == 400 && isFALSE(r$body$success))

# Well-formed but non-existent ticker -> 400 (no data for the symbol)
r <- http_request("GET", "/market-data?ticker=XYZABC99&start=2024-01-01&end=2024-03-01",
                  token = valid_token)
record("market-data: non-existent ticker -> 400 with safe error",
       r$status == 400 && isFALSE(r$body$success) && !grepl("quantmod|getSymbols|curl", r$body$error))

# Invalid dates
r <- http_request("GET", "/market-data?ticker=AAPL&start=2024-06-01&end=2024-01-01",
                  token = valid_token)
record("market-data: start after end -> 400",
       r$status == 400 && isFALSE(r$body$success))

r <- http_request("GET", "/market-data?ticker=AAPL&start=2024-13-01&end=2024-03-01",
                  token = valid_token)
record("market-data: malformed date -> 400",
       r$status == 400 && isFALSE(r$body$success))

r <- http_request("GET", "/market-data?ticker=AAPL&start=2024-01-01&end=2099-01-01",
                  token = valid_token)
record("market-data: future end date -> 400",
       r$status == 400 && isFALSE(r$body$success))

# Missing parameters -> 400
r <- http_request("GET", "/market-data", token = valid_token)
record("market-data: missing parameters -> 400",
       r$status == 400 && isFALSE(r$body$success))

# Auth: bad / forged / expired tokens -> 401
r <- http_request("GET", "/market-data?ticker=AAPL&start=2024-01-01&end=2024-03-01",
                  token = "abc.def.ghi")
record("market-data: invalid token -> 401",
       r$status == 401)

wrong_secret <- jwt_encode_hmac(
  jwt_claim(sub = "1", email = "market@test.local",
            iat = as.numeric(Sys.time()), exp = as.numeric(Sys.time()) + 3600),
  charToRaw("wrong-secret-for-market-data-test")
)
r <- http_request("GET", "/market-data?ticker=AAPL&start=2024-01-01&end=2024-03-01",
                  token = wrong_secret)
record("market-data: wrong-secret token -> 401",
       r$status == 401)

expired_token <- mint_token(exp_delta = -3600) # jose grace period: -1h
r <- http_request("GET", "/market-data?ticker=AAPL&start=2024-01-01&end=2024-03-01",
                  token = expired_token)
record("market-data: expired token -> 401",
       r$status == 401)

# Regression: the Phase 2 JWT validation path is still enforced on /auth/me.
# The locally minted token is cryptographically VALID but references a user
# that does not exist in the database, so /auth/me must reject it with 401
# ("user no longer exists") — proving signature verification runs first.
r <- http_request("GET", "/auth/me", token = valid_token)
record("auth-regression: /auth/me still enforces JWT + user lookup (401 for unknown user)",
       r$status == 401 && isFALSE(r$body$success))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d market-data API tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL MARKET-DATA API TESTS PASSED\n")
quit(status = 0)