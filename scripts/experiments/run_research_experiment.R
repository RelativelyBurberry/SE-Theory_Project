#!/usr/bin/env Rscript
# =============================================================================
# AlphaForge Insight — 15-run research backtesting experiment (PAPER.md §9–§10)
# =============================================================================
# Runs the exact experiment specified in PAPER.md section 9:
#   5 assets (AAPL, MSFT, GOOGL, AMZN, TSLA)  x  3 strategies
#     - MA Crossover (50/200)
#     - RSI (14, 30/70)
#     - Buy & Hold
#   Date range:   2015-01-01 .. 2025-12-31
#   Initial cap:  $10,000 | Transaction cost: 0% | Risk-free rate: 2%
#
# The experiment reuses the PRODUCTION Phase 3–6 pipeline (no reimplementation):
#   backend/analytics/data_retrieval.R  fetch_ohlcv (Yahoo via quantmod, cached)
#   backend/analytics/indicators.R      TTR SMA/RSI
#   backend/analytics/signals.R         MA crossover / RSI crossing / Buy & Hold
#   backend/analytics/execution.R       run_backtest (t+1-OPEN execution engine)
#   backend/analytics/metrics.R         Phase 5 eight-metric layer
#   backend/analytics/utils/validation.R  strategy config validation + defaults
#
# No numerical value in the outputs is fabricated: every metric comes from an
# actual engine execution against live Yahoo Finance data. If any of the 15
# runs fails (e.g. data retrieval), the script FAILS LOUDLY and writes NO
# canonical results — a failed run is never replaced by a guessed value.
#
# Usage (from the repository root):
#   Rscript scripts/experiments/run_research_experiment.R
#
# Outputs (written to scripts/experiments/output/):
#   results_15runs.csv        presented paper table (PAPER.md §10.1)
#   results_15runs_raw.csv    full-precision raw metrics + internals
#   results_aggregates.csv    presented aggregate table (PAPER.md §10.2)
#   results_table.md          human-readable Markdown report
#   experiment.json           full machine-readable record
#   experiment_metadata.json  experiment configuration metadata
# =============================================================================

# --- Resolve the repository root (works from any working directory) ---------
resolve_root <- function() {
  if (file.exists("backend/analytics/execution.R")) return(getwd())
  if (file.exists("scripts/experiments/run_research_experiment.R")) {
    return(dirname(dirname(dirname(normalizePath(
      "scripts/experiments/run_research_experiment.R")))))
  }
  args <- commandArgs(TRUE)
  script_args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    return(dirname(dirname(dirname(normalizePath(file_arg[1])))))
  }
  stop("Could not resolve the repository root; run from the repo root.")
}
root <- resolve_root()
cat("Repository root:", root, "\n")
setwd(root)

# --- Load the production analytics modules -----------------------------------
suppressPackageStartupMessages({
  source(file.path(root, "backend", "analytics", "data_retrieval.R"))
  source(file.path(root, "backend", "analytics", "indicators.R"))
  source(file.path(root, "backend", "analytics", "signals.R"))
  source(file.path(root, "backend", "analytics", "execution.R"))
  source(file.path(root, "backend", "analytics", "metrics.R"))
})
source(file.path(root, "scripts", "experiments", "experiment_lib.R"))

# --- Run the experiment -------------------------------------------------------
cat("--------------------------------------------------------------\n")
cat("ALPHAFORGE INSIGHT — 15-RUN RESEARCH EXPERIMENT (PAPER.md §9)\n")
cat("--------------------------------------------------------------\n")

out <- run_experiment(
  fetch_fn = fetch_ohlcv,          # production market-data retrieval
  out_dir = file.path(root, "scripts", "experiments", "output"),
  quiet = FALSE,
  write_outputs = TRUE
)

# --- Console summary ----------------------------------------------------------
cat("\n\n")
cat("==============================================================\n")
cat("RESULT SUMMARY — PER-ASSET METRICS (PAPER.md §10.1)\n")
cat("==============================================================\n")
print(out$presented, row.names = FALSE, right = FALSE)

cat("\n")
cat("==============================================================\n")
cat("RESULT SUMMARY — AGGREGATE STATISTICS (PAPER.md §10.2)\n")
cat("==============================================================\n")
print(out$aggregates_presented, row.names = FALSE, right = FALSE)

cat("\n")
cat("==============================================================\n")
cat("EXPERIMENT METADATA\n")
cat("==============================================================\n")
md <- out$metadata
cat(sprintf("Data source            : %s\n", md$data_source))
cat(sprintf("Date range             : %s .. %s\n", md$start_date, md$end_date))
cat(sprintf("Initial capital        : $%.2f\n", md$initial_capital))
cat(sprintf("Transaction cost       : %.0f%%\n", md$transaction_cost * 100))
cat(sprintf("Risk-free rate         : %.2f%%\n", md$risk_free_rate * 100))
cat(sprintf("Execution timestamp UTC: %s\n", md$execution_timestamp))
cat(sprintf("Commit hash            : %s\n", md$commit_hash))
cat(sprintf("Observations per ticker: %s\n",
            paste(sprintf("%s=%d", names(md$n_observations), md$n_observations),
                  collapse = ", ")))

cat("\nOutput files written:\n")
for (f in out$files) cat("  ", f, "\n")

cat("\nExperiment completed successfully: all 15 runs produced real engine results.\n")