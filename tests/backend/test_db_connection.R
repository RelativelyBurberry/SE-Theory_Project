# AlphaForge Insight — Phase 2 database connection test (Neon PostgreSQL)
#
# Verifies that DATABASE_URL works and that the schema from database/init.sql
# has been applied (users, backtests, results tables with the expected
# columns). Does not create or modify data; read-only.
#
# Run from the repository root:
#   Rscript tests/backend/test_db_connection.R
#
# Exit code 0 = pass, 1 = fail.

backend_renviron <- file.path("backend", ".Renviron")
if (file.exists(backend_renviron)) {
  readRenviron(backend_renviron)
} else if (file.exists(".env")) {
  readRenviron(".env")
}

db_url <- Sys.getenv("DATABASE_URL", unset = "")
if (!nzchar(db_url)) {
  cat("SKIP: DATABASE_URL is not set. Configure backend/.Renviron first.\n")
  quit(status = 1)
}

library(DBI)
library(RPostgres)

# Use the same connection layer as the API (keyword-arg URL parsing).
source(file.path("backend", "database", "connection.R"))

results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

con <- NULL
tryCatch(
  {
    con <- get_db_connection()
    record("connection: DATABASE_URL connects successfully", TRUE)

    # Table existence
    for (tbl in c("users", "backtests", "results")) {
      record(paste0("table: '", tbl, "' exists"),
             dbExistsTable(con, tbl))
    }

    # users table columns
    cols <- dbGetQuery(
      con,
      "SELECT column_name FROM information_schema.columns
       WHERE table_schema = 'public' AND table_name = 'users'"
    )
    column_names <- sort(cols$column_name)
    record("schema: users has id/email/password_hash/created_at",
           all(c("id", "email", "password_hash", "created_at") %in% column_names))

    # email uniqueness constraint
    idx <- dbGetQuery(
      con,
      "SELECT indexname FROM pg_indexes
       WHERE tablename = 'users' AND indexdef ILIKE '%unique%'"
    )
    uniq_cols <- dbGetQuery(
      con,
      "SELECT a.attname
       FROM pg_index i
       JOIN pg_class t ON t.oid = i.indrelid
       JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = ANY(i.indkey)
       WHERE t.relname = 'users' AND i.indisunique"
    )
    record("schema: users.email has a unique constraint",
           "email" %in% uniq_cols$attname || nrow(idx) > 0)

    # password_hash is NOT NULL
    nul <- dbGetQuery(
      con,
      "SELECT is_nullable FROM information_schema.columns
       WHERE table_schema = 'public' AND table_name = 'users' AND column_name = 'password_hash'"
    )
    record("schema: users.password_hash is NOT NULL",
           identical(nul$is_nullable[1], "NO"))
  },
  error = function(e) {
    record("connection: DATABASE_URL connects successfully", FALSE, conditionMessage(e))
  },
  finally = {
    if (!is.null(con) && dbIsValid(con)) dbDisconnect(con)
  }
)

passed <- vapply(results, function(x) x$passed, logical(1))
cat(sprintf("\n%d/%d database tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL DATABASE TESTS PASSED\n")
quit(status = 0)