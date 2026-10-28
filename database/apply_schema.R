# AlphaForge Insight — safe database schema initialisation (Phase 2)
#
# Applies database/init.sql to the Neon PostgreSQL database configured in
# DATABASE_URL. The schema file uses CREATE TABLE IF NOT EXISTS and is
# NON-DESTRUCTIVE: it never drops or alters existing tables/data, so it is
# safe to run repeatedly (idempotent).
#
# Usage:
#   1. Configure DATABASE_URL (backend/.Renviron or shell environment).
#   2. Rscript database/apply_schema.R
#
# Exit code 0 = schema applied/verified, 1 = failure.

# Ensure backend/.Renviron (git-ignored, holds DATABASE_URL) is loaded.
backend_renviron <- file.path("backend", ".Renviron")
if (file.exists(backend_renviron)) {
  readRenviron(backend_renviron)
} else {
  # Convenience: a root .env file is loaded as a fallback.
  root_env <- file.path(".env")
  if (file.exists(root_env)) {
    readRenviron(root_env)
  } else {
    home_renviron <- file.path(path.expand("~"), ".Renviron")
    if (file.exists(home_renviron)) {
      readRenviron(home_renviron)
    }
  }
}

db_url <- Sys.getenv("DATABASE_URL", unset = "")
if (!nzchar(db_url)) {
  cat("ERROR: DATABASE_URL is not set.\n")
  cat("  Copy backend/.Renviron.example to backend/.Renviron and set\n")
  cat("  DATABASE_URL to your Neon connection string, then retry.\n")
  quit(status = 1)
}

library(DBI)
library(RPostgres)

# Use the same connection layer as the API (keyword-arg URL parsing).
source(file.path("backend", "database", "connection.R"))

expected_tables <- c("users", "backtests", "results")

apply_schema <- function() {
  schema_file <- file.path("database", "init.sql")
  if (!file.exists(schema_file)) {
    stop("Schema file not found: ", schema_file)
  }

  sql_lines <- readLines(schema_file, warn = FALSE)
  # Drop full-line comments and blank lines, then split into individual
  # statements (RPostgres cannot run multiple commands in one prepared
  # statement, so each statement is executed separately).
  sql_lines <- sql_lines[!grepl("^[[:space:]]*--", sql_lines)]
  sql_clean <- paste(sql_lines, collapse = "\n")
  stmts <- strsplit(sql_clean, ";", fixed = TRUE)[[1]]
  stmts <- trimws(stmts)
  stmts <- stmts[nzchar(stmts)]
  if (length(stmts) == 0) stop("No SQL statements found in ", schema_file)

  con <- NULL
  tryCatch(
    {
      cat("Connecting to database (Neon PostgreSQL)...\n")
      con <- get_db_connection()
      cat("Connected.\n")

      cat("Applying ", schema_file, " (non-destructive; CREATE TABLE IF NOT EXISTS)...\n", sep = "")
      for (s in stmts) {
        dbExecute(con, s)
      }
      cat("Schema statements executed (", length(stmts), " statements).\n", sep = "")

      cat("Verifying tables...\n")
      present <- dbExistsTable(con, "users") &&
        dbExistsTable(con, "backtests") &&
        dbExistsTable(con, "results")
      if (!present) {
        stop("One or more expected tables are missing after applying the schema.")
      }

      for (tbl in expected_tables) {
        n_tables <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM information_schema.tables WHERE table_schema = 'public' AND table_name = $1", params = list(tbl))
        cat(sprintf("  table '%s' present: %s\n", tbl, n_tables$n[1] > 0))
      }

      # Verify the users table has the auth columns required by Phase 2.
      cols <- dbGetQuery(
        con,
        "SELECT column_name FROM information_schema.columns
         WHERE table_schema = 'public' AND table_name = 'users'"
      )
      required <- c("id", "email", "password_hash", "created_at")
      missing <- setdiff(required, cols$column_name)
      if (length(missing) > 0) {
        stop("users table is missing columns: ", paste(missing, collapse = ", "))
      }

      cat("Schema verified. All required tables exist with the expected columns.\n")
      TRUE
    },
    error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
      FALSE
    },
    finally = {
      if (!is.null(con) && dbIsValid(con)) {
        dbDisconnect(con)
      }
    }
  )
}

ok <- isTRUE(apply_schema())
if (ok) {
  cat("apply_schema: PASS\n")
  quit(status = 0)
} else {
  quit(status = 1)
}