# AlphaForge Insight — DB connection resilience unit tests (no real database)
#
# Verifies the bounded retry + connect_timeout behaviour added to
# backend/database/connection.R:
#   - connect_timeout=4 is passed to RPostgres::dbConnect()
#   - a transient connection failure triggers retries (max 3 attempts)
#   - a successful first attempt does not retry
#   - when every attempt fails, the original connection error is surfaced
#   - retry is confined to connection establishment (SQL is never retried)
#   - the attempt bound is respected
#
# Uses a mocked dbConnect in an isolated environment, so no real database
# connection is ever attempted.
#
# Run from the repository root:
#   Rscript tests/backend/test_connection_retry.R
# Exit code 0 = pass, 1 = fail.

# Isolated environment so the mocked dbConnect never leaks into global state.
conn_env <- new.env(parent = globalenv())
source(file.path("backend", "database", "connection.R"), local = conn_env)

results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

TEST_URL <- "postgresql+psycopg://alice:s3cr3t@db.internal:5555/neondb?sslmode=require"
Sys.setenv(DATABASE_URL = TEST_URL)
on.exit(Sys.unsetenv("DATABASE_URL"), add = TRUE)

# Mock dbConnect: each element of `behaviors` is either "ok" or a character
# error message ("fail:<message>"). Behaviors are consumed in order; after the
# list is exhausted the mock succeeds.
new_mock <- function(behaviors) {
  st <- new.env(parent = emptyenv())
  st$calls <- 0L
  st$args <- list()
  f <- function(drv, ...) {
    st$calls <- st$calls + 1L
    st$args[[st$calls]] <- list(...)
    b <- if (st$calls <= length(behaviors)) behaviors[[st$calls]] else "ok"
    if (!identical(b, "ok")) stop(b)
    list(ok = TRUE)
  }
  attr(f, "state") <- st
  f
}
mock_state <- function(m) attr(m, "state")

# ---------------------------------------------------------------------------
# 1. Integration via get_db_connection(): connect_timeout + intact args +
#    no retry on first success.
# ---------------------------------------------------------------------------
m1 <- new_mock(list("ok"))
conn_env$dbConnect <- m1
con1 <- conn_env$get_db_connection()
a1 <- mock_state(m1)$args[[1L]]

record("connect_timeout=4 seconds is passed to dbConnect",
       identical(a1$connect_timeout, 4L))
record("hostname is passed intact through get_db_connection",
       identical(a1$host, "db.internal"))
record("dbname is passed intact", identical(a1$dbname, "neondb"))
record("sslmode query parameter is preserved", identical(a1$sslmode, "require"))
record("successful first connection does not retry (exactly 1 attempt)",
       mock_state(m1)$calls == 1L && is.list(con1) && isTRUE(con1$ok))

# ---------------------------------------------------------------------------
# 2. Transient failure causes retry (direct helper, zero sleep for speed).
# ---------------------------------------------------------------------------
m2 <- new_mock(list("fail:transient dns error", "fail:transient dns error", "ok"))
args2 <- conn_env$parse_db_url(conn_env$normalize_db_url(TEST_URL))
con2 <- conn_env$db_connect_with_retry(args2, max_attempts = 3L,
                                       sleep_seconds = c(0, 0), connect_fun = m2)
record("transient connection failure is retried up to 3 attempts",
       mock_state(m2)$calls == 3L && is.list(con2) && isTRUE(con2$ok))
record("connect_timeout=4 is applied on every retry attempt",
       all(vapply(mock_state(m2)$args,
                  function(x) identical(x$connect_timeout, 4L), logical(1))) &&
         mock_state(m2)$calls == 3L)

# ---------------------------------------------------------------------------
# 3. All attempts failing returns the original connection error.
# ---------------------------------------------------------------------------
m3 <- new_mock(rep(
  list('fail:could not translate host name "db.internal" to address: Name or service not known'),
  3L
))
conn_env$dbConnect <- m3
r3 <- tryCatch({ conn_env$get_db_connection(); "NO ERROR" },
               error = function(e) conditionMessage(e))
record("all attempts failing: exactly 3 connection attempts made",
       mock_state(m3)$calls == 3L)
record("all attempts failing: original libpq error preserved in stop message",
       grepl("could not translate host name", r3) &&
         grepl("Failed to connect to the database", r3) &&
         !identical(r3, "NO ERROR"))

# ---------------------------------------------------------------------------
# 4. Retry is confined to connection establishment: a query failure on an
#    established connection must NOT trigger another dbConnect call.
# ---------------------------------------------------------------------------
m4 <- new_mock(list("ok"))
conn_env$dbConnect <- m4
con4 <- conn_env$get_db_connection()
sql_err <- tryCatch({ DBI::dbGetQuery(con4, "SELECT faulty"); "NO ERROR" },
                    error = function(e) conditionMessage(e))
record("SQL failure on an established connection does NOT re-run dbConnect",
       mock_state(m4)$calls == 1L && !identical(sql_err, "NO ERROR"))

# ---------------------------------------------------------------------------
# 5. The attempt bound is respected (max_attempts=1 -> single try, then stop).
# ---------------------------------------------------------------------------
m5 <- new_mock(list("boom"))
r5 <- tryCatch({
  conn_env$db_connect_with_retry(
    conn_env$parse_db_url(conn_env$normalize_db_url(TEST_URL)),
    max_attempts = 1L, sleep_seconds = c(0), connect_fun = m5)
  "NO ERROR"
}, error = function(e) conditionMessage(e))
record("max_attempts=1 makes a single attempt then stops with the error",
       mock_state(m5)$calls == 1L && identical(r5, "boom"))

# ---------------------------------------------------------------------------
# Cleanup + summary
# ---------------------------------------------------------------------------
conn_env$dbConnect <- NULL

passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d connection retry unit tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL CONNECTION RETRY TESTS PASSED\n")
quit(status = 0)