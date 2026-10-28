# AlphaForge Insight — safe ticker-length migration applier
#
# Applies database/migration_ticker_length.sql (widens backtests.ticker and
# ml_models.ticker from VARCHAR(10) to VARCHAR(20)) to the PostgreSQL database
# configured in DATABASE_URL. Non-destructive and idempotent: the ALTER
# statements are no-ops when the columns are already VARCHAR(20), and no rows
# are modified or dropped.
#
# Usage (from the repository root):
#   Rscript database/apply_migration_ticker_length.R
#
# Exit code 0 = applied + verified, 1 = failure.

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

schema_file <- file.path("database", "migration_ticker_length.sql")
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

    # Post-apply verification: the ACTUAL database must now accept 20-char
    # tickers — not just the migration file. Checks information_schema so the
    # check reflects the running database, whatever its history.
    widths <- dbGetQuery(con, "
      SELECT table_name, character_maximum_length AS len
      FROM information_schema.columns
      WHERE table_schema = 'public'
        AND ((table_name = 'backtests' AND column_name = 'ticker')
          OR (table_name = 'ml_models' AND column_name = 'ticker'))
    ")
    if (nrow(widths) != 2L) {
      stop("Expected ticker columns not found after migration (backtests.ticker, ml_models.ticker).")
    }
    if (!all(widths$len == 20L)) {
      bad <- paste0(widths$table_name, ".ticker=", widths$len, collapse = ", ")
      stop("Ticker columns were not widened to 20 (got: ", bad, ").")
    }
    for (i in seq_len(nrow(widths))) {
      cat(sprintf("  verified %s.ticker = VARCHAR(%d)\n", widths$table_name[i], widths$len[i]))
    }

    # Functional proof on the real database: an 11-char NSE symbol must be
    # storable in both columns. Performed in a transaction that is ALWAYS
    # rolled back, so no test data is left behind.
    dbBegin(con)
    proof_ok <- tryCatch(
      {
        dbExecute(con, "INSERT INTO backtests (user_id, ticker, strategy, start_date, end_date, initial_capital, parameters)
                       VALUES ((SELECT MIN(id) FROM users), 'RELIANCE.NS', 'ML_Strategy', '2023-01-01', '2023-01-02', 1, '{}')")
        dbExecute(con, "INSERT INTO ml_models (user_id, model_key, model_name, ticker, start_date, end_date)
                       VALUES ((SELECT MIN(id) FROM users), 'random_forest', 'Random Forest', 'RELIANCE.NS', '2023-01-01', '2023-01-02')")
        TRUE
      },
      error = function(e) {
        cat("  functional insert check FAILED:", conditionMessage(e), "\n")
        FALSE
      }
    )
    dbRollback(con)
    if (!isTRUE(proof_ok)) stop("11-character ticker could not be inserted after migration.")
    cat("  verified live insert of 'RELIANCE.NS' (rolled back; no data left behind)\n")

    cat("Ticker-length migration applied and verified.\n")
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
