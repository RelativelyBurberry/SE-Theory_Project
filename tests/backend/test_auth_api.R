# AlphaForge Insight — Phase 2 auth integration tests (REAL database required)
#
# Starts the real Plumber API and exercises the full authentication flow
# against the Neon PostgreSQL database configured in DATABASE_URL:
#
#   POST /auth/register  -> 201 / 400 / 409
#   POST /auth/login     -> 200 + JWT / 401
#   GET  /auth/me        -> 200 / 401 (missing, invalid, expired tokens)
#
# Also verifies database persistence: the user row exists and the stored
# password is a bcrypt hash, never plaintext.
#
# Prerequisites:
#   1. DATABASE_URL set (backend/.Renviron or shell environment).
#   2. backend/apply_schema.R has been run once so the tables exist.
#
# Run from the repository root:
#   Rscript tests/backend/test_auth_api.R
#
# Exit code 0 = pass, 1 = fail.

PORT <- 8127L
TEST_JWT_SECRET <- "integration-test-secret-9876543210abcdef"
# Unique email per run so tests never collide with real data.
TEST_EMAIL <- paste0("auth.test.", as.integer(Sys.time()), "@example.com")
TEST_PASSWORD <- "Phase2TestPassword!"

# Load backend/.Renviron so DATABASE_URL is available to the child process.
backend_renviron <- file.path("backend", ".Renviron")
if (file.exists(backend_renviron)) {
  readRenviron(backend_renviron)
} else if (file.exists(".env")) {
  readRenviron(".env")
}

if (!nzchar(Sys.getenv("DATABASE_URL", unset = ""))) {
  cat("SKIP: DATABASE_URL is not set. Configure backend/.Renviron (copy from\n")
  cat("  backend/.Renviron.example with your Neon connection string) first.\n")
  quit(status = 1)
}

if (!nzchar(Sys.getenv("JWT_SECRET", unset = ""))) {
  Sys.setenv(JWT_SECRET = TEST_JWT_SECRET)
}

library(curl)

# ---------------------------------------------------------------------------
# HTTP helper (curl package)
# ---------------------------------------------------------------------------
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
  parsed <- tryCatch(jsonlite::fromJSON(rawToChar(resp$content)),
                     error = function(e) rawToChar(resp$content))
  list(status = as.integer(resp$status_code), body = parsed)
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
           PLUMBER_PIDFILE = pidfile, JWT_SECRET = Sys.getenv("JWT_SECRET"))
on.exit(Sys.unsetenv(c("PLUMBER_PORT", "PLUMBER_HOST", "PLUMBER_PIDFILE")))

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

# Wait for the API to accept requests.
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
# 1. Registration
# ---------------------------------------------------------------------------
r <- http_request("POST", "/auth/register",
                  list(email = TEST_EMAIL, password = TEST_PASSWORD))
record("register: successful registration returns 201",
       r$status == 201 && isTRUE(r$body$success))
record("register: response contains safe user", 
       !is.null(r$body$user$id) && identical(r$body$user$email, TEST_EMAIL))
record("register: no token in register response", is.null(r$body$token))
record("register: no password/hash in register response",
       is.null(r$body$password) && is.null(r$body$password_hash) && is.null(r$body$user$password_hash))

# --- duplicate registration (409) ---
r <- http_request("POST", "/auth/register",
                  list(email = TEST_EMAIL, password = TEST_PASSWORD))
record("register: duplicate email returns 409",
       r$status == 409 && isFALSE(r$body$success))

# --- invalid email (400) ---
r <- http_request("POST", "/auth/register",
                  list(email = "not-an-email", password = TEST_PASSWORD))
record("register: invalid email returns 400",
       r$status == 400 && isFALSE(r$body$success))

# --- missing password (400) ---
r <- http_request("POST", "/auth/register",
                  list(email = paste0("nopass.", TEST_EMAIL)))
record("register: missing password returns 400",
       r$status == 400 && isFALSE(r$body$success))

# --- short password (400) ---
r <- http_request("POST", "/auth/register",
                  list(email = paste0("short.", TEST_EMAIL), password = "short"))
record("register: short password returns 400",
       r$status == 400 && isFALSE(r$body$success))

# --- empty request body (400) ---
r <- http_request("POST", "/auth/register", list())
record("register: empty body returns 400",
       r$status == 400 && isFALSE(r$body$success))

# ---------------------------------------------------------------------------
# 2. Login
# ---------------------------------------------------------------------------
r <- http_request("POST", "/auth/login",
                  list(email = TEST_EMAIL, password = TEST_PASSWORD))
record("login: successful login returns 200",
       r$status == 200 && isTRUE(r$body$success))
record("login: response contains a JWT token",
       is.character(r$body$token) && nchar(r$body$token) > 40)
token <- r$body$token
record("login: token has three dot-separated parts",
       length(strsplit(token, "\\.")[[1]]) == 3)
record("login: response contains safe user",
       identical(r$body$user$email, TEST_EMAIL))
record("login: no password/hash in login response",
       is.null(r$body$password) && is.null(r$body$password_hash) && is.null(r$body$user$password_hash))

# Wrong password (401)
r <- http_request("POST", "/auth/login",
                  list(email = TEST_EMAIL, password = "WrongPassword!"))
record("login: wrong password returns 401",
       r$status == 401 && isFALSE(r$body$success))

# Unknown email (401)
r <- http_request("POST", "/auth/login",
                  list(email = "nobody@nowhere.invalid", password = TEST_PASSWORD))
record("login: unknown email returns 401",
       r$status == 401 && isFALSE(r$body$success))

# ---------------------------------------------------------------------------
# 3. /auth/me
# ---------------------------------------------------------------------------
# Valid JWT
r <- http_request("GET", "/auth/me", token = token)
record("me: valid token returns 200 with current user",
       r$status == 200 && isTRUE(r$body$success) && identical(r$body$user$email, TEST_EMAIL))

# Missing token (401)
r <- http_request("GET", "/auth/me")
record("me: missing token returns 401", r$status == 401)

# Random invalid token (401)
r <- http_request("GET", "/auth/me", token = "abc.def.ghi")
record("me: invalid token returns 401",
       r$status == 401 && isFALSE(r$body$success))

# Malformed header (401)
h <- curl::new_handle()
curl::handle_setheaders(h, "Authorization" = "Basic dXNlcjpwYXNz")
resp <- curl::curl_fetch_memory(paste0("http://127.0.0.1:", PORT, "/auth/me"), handle = h)
record("me: non-bearer Authorization header returns 401",
       as.integer(resp$status_code) == 401)

# Token signed with a different secret (401)
library(jose)
other_secret <- charToRaw(paste0("different-secret-", TEST_JWT_SECRET))
forged_claim <- jwt_claim(sub = "1", email = TEST_EMAIL,
                          iat = as.numeric(Sys.time()), exp = as.numeric(Sys.time()) + 3600)
forged_token <- jwt_encode_hmac(forged_claim, other_secret)
r <- http_request("GET", "/auth/me", token = forged_token)
record("me: token with wrong secret returns 401",
       r$status == 401 && isFALSE(r$body$success))

# Expired token (401) — jose uses a 60s grace period, so expire well in the past
expired_claim <- jwt_claim(sub = "1", email = TEST_EMAIL,
                           iat = as.numeric(Sys.time()) - 7200,
                           exp = as.numeric(Sys.time()) - 3600)
expired_token <- jwt_encode_hmac(expired_claim, charToRaw(Sys.getenv("JWT_SECRET")))
r <- http_request("GET", "/auth/me", token = expired_token)
record("me: expired token returns 401",
       r$status == 401 && isFALSE(r$body$success))

# ---------------------------------------------------------------------------
# 4. Database persistence checks
# ---------------------------------------------------------------------------
source(file.path("backend", "database", "connection.R"))
con <- get_db_connection()
on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

row <- DBI::dbGetQuery(
  con,
  "SELECT id, email, password_hash, created_at FROM users WHERE email = $1",
  params = list(TEST_EMAIL)
)
record("db: user row is persisted in Neon",
       nrow(row) == 1 && identical(row$email[1], TEST_EMAIL) && !is.na(row$id[1]))
record("db: stored password is a bcrypt hash, NOT plaintext",
       nrow(row) == 1 && grepl("^\\$2", row$password_hash[1]) &&
         !grepl("Phase2TestPassword", row$password_hash[1]) &&
         bcrypt::checkpw(TEST_PASSWORD, row$password_hash[1]))
record("db: created_at timestamp is populated", nrow(row) == 1 && !is.na(row$created_at[1]))

# ---------------------------------------------------------------------------
# 5. Cleanup: remove the test user (test data should not pollute Neon)
# ---------------------------------------------------------------------------
DELETED <- DBI::dbExecute(con, "DELETE FROM users WHERE email = $1", params = list(TEST_EMAIL))
record("db: test user removed after test", is.numeric(DELETED) && DELETED >= 0)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(x) x$passed, logical(1))
cat(sprintf("\n%d/%d integration tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL INTEGRATION TESTS PASSED\n")
quit(status = 0)