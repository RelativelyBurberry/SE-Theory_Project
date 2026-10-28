# AlphaForge Insight — safe ML schema migration applier
#
# Applies database/migration_ml.sql (additive ml_models + ml_runs tables) to
# the Neon PostgreSQL database configured in DATABASE_URL. Non-destructive
# and idempotent: CREATE TABLE IF NOT EXISTS / CREATE INDEX IF NOT EXISTS.
#
# Usage (from the repository root):
#   Rscript database/apply_migration_ml.R

backend_renviron <- file.path("backend", ".Renviron")
if (file.exists(backend_renviron)) {
  readRenviron(backend_renviron)
} else {
  root_env <- file.path(".env")
  if (file.exists(root_env)) readRenviron(root_env)
}

db_url <- Sys.getenv("DATABASE_URL", unset = "")
if (!nzchar(db_url)) {
  cat("ERROR: DATABASE_URL is not set.\n")
  quit(status = 1)
}

library(DBI)
library(RPostgres)
source(file.path("backend", "database", "connection.R"))

schema_file <- file.path("database", "migration_ml.sql")
sql_lines <- readLines(schema_file, warn = FALSE)
sql_lines <- sql_lines[!grepl("^[[:space:]]*--", sql_lines)]
sql_clean <- paste(sql_lines, collapse = "\n")
stmts <- trimws(strsplit(sql_clean, ";", fixed = TRUE)[[1]])
stmts <- stmts[nzchar(stmts)]

con <- NULL
ok <- FALSE
tryCatch(
  {
    con <- get_db_connection()
    for (s in stmts) dbExecute(con, s)
    present <- dbExistsTable(con, "ml_models") && dbExistsTable(con, "ml_runs")
    if (!present) stop("ml_models/ml_runs tables missing after migration.")
    cat("ML schema applied and verified (ml_models, ml_runs).\n")
    ok <- TRUE
  },
  error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  },
  finally = {
    if (!is.null(con) && dbIsValid(con)) dbDisconnect(con)
  }
)
quit(status = if (ok) 0 else 1)
