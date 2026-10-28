# Apply the Phase 6 additive migration to the configured Neon database.
# Idempotent; safe to re-run.
# Run from the repository root:
#   Rscript database/apply_migration_phase6.R

readRenviron('backend/.Renviron')
if (file.exists('.env')) readRenviron('.env')
suppressMessages({library(DBI); library(RPostgres)})

src_env <- new.env(parent = globalenv())
sys.source(file.path('backend', 'database', 'connection.R'), envir = src_env)
con <- src_env$get_db_connection()
on.exit(src_env$close_db_connection(con), add = TRUE)

lines <- readLines('database/migration_phase6_results_columns.sql', warn = FALSE)
stmt <- paste(lines[!grepl('^\\s*--', lines) & nchar(trimws(lines)) > 0], collapse = '\n')
cat("Executing:\n", stmt, "\n")
dbExecute(con, stmt)

cols <- dbGetQuery(con,
  "SELECT column_name, data_type FROM information_schema.columns
   WHERE table_name = 'results' AND column_name IN ('initial_capital','final_value')
   ORDER BY column_name")
print(cols)
stopifnot(nrow(cols) == 2L)
cat('Phase 6 migration applied and verified: initial_capital + final_value exist.\n')