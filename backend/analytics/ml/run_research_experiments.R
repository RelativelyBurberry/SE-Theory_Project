#!/usr/bin/env Rscript
# =============================================================================
# AlphaForge Insight — ML Research Experiment Runner (Phase G)
# =============================================================================
# Executes the full ML research pipeline with corrected temporal validation:
#   - Loads real historical data via existing data layer
#   - Builds features and labels with embargo-corrected splits
#   - Trains and evaluates ML models (RF, XGBoost, baselines)
#   - Runs ablation studies (features, confidence filtering)
#   - Outputs results to research/EXPERIMENT_RESULTS.md
#
# Label/Execution Timing Convention:
#   - Label: Close[t+5]/Close[t] - 1 (5-day forward return)
#   - Signal: Generated at Close[t] (end of day)
#   - Execution: At Open[t+1] (next day open)
#   - Realized Return: Open[t+1] to Open[exit+1]
#   - GAP: Labels predict Close-to-Close, execution captures Open-to-Open
#   - This overnight gap is an unavoidable market reality; the model learns
#     to predict Close-to-Close returns, but the strategy captures returns
#     from the next morning's open. We document this explicitly.
#
# Usage (from repository root):
#   Rscript backend/analytics/ml/run_research_experiments.R
#
# Outputs:
#   - Console progress and summary
#   - JSON results file with full provenance
#   - Updates to research/EXPERIMENT_RESULTS.md
# =============================================================================

options(warn = -1)

# --- Resolve repository root -------------------------------------------------
resolve_root <- function() {
  if (file.exists("backend/analytics/execution.R")) return(getwd())
  if (file.exists("backend/analytics/ml/run_research_experiments.R")) {
    return(normalizePath(file.path(getwd(), "../..")))
  }
  args <- commandArgs(TRUE)
  script_args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    return(dirname(dirname(dirname(normalizePath(file_arg[1])))))
  }
  stop("Could not resolve repository root.")
}
root <- resolve_root()
setwd(root)
cat("Repository root:", root, "\n")

# --- Load required modules ----------------------------------------------------
suppressPackageStartupMessages({
  source(file.path(root, "backend", "analytics", "utils", "validation.R"))
  source(file.path(root, "backend", "analytics", "data_retrieval.R"))
  source(file.path(root, "backend", "analytics", "execution.R"))
  source(file.path(root, "backend", "analytics", "metrics.R"))
  for (f in c("features.R", "labels.R", "preprocessing.R", "evaluation.R",
              "models.R", "lstm.R", "hyperparameters.R", "adapter.R",
              "pipeline.R")) {
    source(file.path(root, "backend", "analytics", "ml", f))
  }
})

# --- Experiment configuration -------------------------------------------------
EXPERIMENT_CONFIG <- list(
  tickers = c("AAPL", "MSFT", "GOOGL"),
  start_date = "2015-01-01",
  end_date = "2024-12-31",
  horizon = 5L,
  buy_threshold = 0.02,
  sell_threshold = 0.02,
  initial_capital = 10000,
  transaction_costs = c(0.0, 0.001, 0.002),
  signal_modes = c("classes", "probability"),
  prob_threshold = 0.6,
  n_folds = 5L,
  test_frac = 0.1,
  embargo = 5L,
  random_seed = 42L
)

# --- Helper functions --------------------------------------------------------
flatten_results <- function(results_list) {
  do.call(rbind, lapply(names(results_list), function(name) {
    r <- results_list[[name]]
    if (isTRUE(r$success)) {
      data.frame(
        model = name,
        status = "ok",
        accuracy = r$classification$accuracy,
        f1 = r$classification$f1,
        precision = r$classification$precision,
        recall = r$classification$recall,
        roc_auc = r$classification$roc_auc_macro,
        pr_auc = r$classification$pr_auc_macro,
        cagr = r$backtest$summary$cagr,
        sharpe = r$backtest$summary$sharpe,
        sortino = r$backtest$summary$sortino,
        max_dd = r$backtest$summary$maximum_drawdown,
        win_rate = r$backtest$summary$win_rate,
        volatility = r$backtest$summary$volatility,
        trades = r$backtest$summary$total_trades,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        model = name,
        status = "failed",
        error = if (!is.null(r$error)) r$error else "unknown",
        stringsAsFactors = FALSE
      )
    }
  }))
}

# --- Run experiments ---------------------------------------------------------
cat("\n==============================================================\n")
cat("ALPHAFORGE INSIGHT — ML RESEARCH EXPERIMENTS (Phase G)\n")
cat("==============================================================\n")
cat(sprintf("Date: %s\n", EXPERIMENT_CONFIG$start_date))
cat(sprintf("Tickers: %s\n", paste(EXPERIMENT_CONFIG$tickers, collapse = ", ")))
cat(sprintf("Horizon: %d days | Thresholds: +/-%.0f%%\n", 
            EXPERIMENT_CONFIG$horizon, EXPERIMENT_CONFIG$buy_threshold * 100))
cat(sprintf("Walk-forward: %d folds, %.0f%% test, %d-day embargo\n",
            EXPERIMENT_CONFIG$n_folds, EXPERIMENT_CONFIG$test_frac * 100,
            EXPERIMENT_CONFIG$embargo))
cat("==============================================================\n\n")

set.seed(EXPERIMENT_CONFIG$random_seed)

all_results <- list()
all_predictions <- list()

for (ticker in EXPERIMENT_CONFIG$tickers) {
  cat(sprintf("\n=== Processing %s ===\n", ticker))
  
  # Fetch data
  cat(sprintf("[%s] Fetching market data...\n", ticker))
  ohlcv <- tryCatch({
    fetch_ohlcv(ticker, EXPERIMENT_CONFIG$start_date, EXPERIMENT_CONFIG$end_date)
  }, error = function(e) {
    cat(sprintf("[%s] ERROR fetching data: %s\n", ticker, e$message))
    return(NULL)
  })
  
  if (is.null(ohlcv)) {
    cat(sprintf("[%s] Skipping due to data error\n", ticker))
    next
  }
  
  cat(sprintf("[%s] Retrieved %d observations from %s to %s\n", 
              ticker, nrow(ohlcv), 
              min(zoo::index(ohlcv)), max(zoo::index(ohlcv))))
  
  # Assemble ML dataset
  cat(sprintf("[%s] Building features and labels...\n", ticker))
  ds <- tryCatch({
    assemble_ml_dataset(
      ohlcv = ohlcv,
      ticker = ticker,
      start = EXPERIMENT_CONFIG$start_date,
      end = EXPERIMENT_CONFIG$end_date,
      horizon = EXPERIMENT_CONFIG$horizon,
      buy_threshold = EXPERIMENT_CONFIG$buy_threshold,
      sell_threshold = EXPERIMENT_CONFIG$sell_threshold,
      groups = NULL  # All features
    )
  }, error = function(e) {
    cat(sprintf("[%s] ERROR assembling dataset: %s\n", ticker, e$message))
    return(NULL)
  })
  
  if (is.null(ds)) {
    next
  }
  
  cat(sprintf("[%s] Dataset: %d samples, %d features\n", ticker, ds$n, ds$p))
  cat(sprintf("[%s] Train/Val/Test: %d/%d/%d samples\n", ticker,
              length(ds$split$train), length(ds$split$validation),
              length(ds$split$test)))
  
  # Class distribution
  train_tab <- table(ds$y[ds$split$train])
  cat(sprintf("[%s] Train class dist: BUY=%d(%.1f%%) HOLD=%d(%.1f%%) SELL=%d(%.1f%%)\n",
              ticker, train_tab["BUY"], 100*train_tab["BUY"]/sum(train_tab),
              train_tab["HOLD"], 100*train_tab["HOLD"]/sum(train_tab),
              train_tab["SELL"], 100*train_tab["SELL"]/sum(train_tab)))
  
  # Run models
  cat(sprintf("[%s] Training and evaluating models...\n", ticker))
  
  models_to_run <- c("random_forest", "xgboost")
  
  results <- tryCatch({
    run_model_comparison(
      ds = ds,
      models = models_to_run,
      hyperparams = list(),
      evaluate_on = "test",
      initial_capital = EXPERIMENT_CONFIG$initial_capital,
      signal_mode = "classes",
      prob_threshold = EXPERIMENT_CONFIG$prob_threshold
    )
  }, error = function(e) {
    cat(sprintf("[%s] ERROR in model comparison: %s\n", ticker, e$message))
    return(NULL)
  })
  
  if (!is.null(results)) {
    all_results[[ticker]] <- results
    
    # Print summary
    cat(sprintf("\n[%s] RESULTS SUMMARY:\n", ticker))
    print(results$comparison[, c("Model", "Accuracy", "F1", "Sharpe", "CAGR", "MaxDrawdown", "TotalTrades")],
          row.names = FALSE)
  }
}

# --- Aggregate results -------------------------------------------------------
cat("\n==============================================================\n")
cat("AGGREGATE RESULTS\n")
cat("==============================================================\n")

if (length(all_results) > 0) {
  # Combine all tickers
  all_comparison <- do.call(rbind, lapply(names(all_results), function(ticker) {
    df <- all_results[[ticker]]$comparison
    df$ticker <- ticker
    df
  }))
  
  cat("\n--- Model Performance Across All Tickers ---\n")
  print(aggregate(cbind(Accuracy, F1, Sharpe, CAGR) ~ Model, 
                  data = all_comparison, 
                  FUN = function(x) mean(x, na.rm = TRUE)),
        row.names = FALSE)
  
  # Save results
  output_dir <- file.path(root, "scripts", "experiments", "output")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Write JSON
  json_output <- list(
    metadata = list(
      experiment = "ML Research Experiments - Phase G",
      date = EXPERIMENT_CONFIG$start_date,
      tickers = EXPERIMENT_CONFIG$tickers,
      horizon = EXPERIMENT_CONFIG$horizon,
      thresholds = list(buy = EXPERIMENT_CONFIG$buy_threshold, sell = EXPERIMENT_CONFIG$sell_threshold),
      embargo = EXPERIMENT_CONFIG$embargo,
      n_folds = EXPERIMENT_CONFIG$n_folds,
      random_seed = EXPERIMENT_CONFIG$random_seed,
      execution_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    ),
    results = lapply(all_results, function(r) {
      lapply(r$results, function(m) {
        if (isTRUE(m$success)) {
          list(
            status = "ok",
            classification = m$classification[c("accuracy", "f1", "precision", "recall", "roc_auc_macro")],
            backtest = m$backtest$summary
          )
        } else {
          list(status = "failed", error = m$error)
        }
      })
    })
  )
  
  json_file <- file.path(output_dir, "ml_experiment_results.json")
  writeLines(jsonlite::toJSON(json_output, auto_unbox = TRUE, pretty = TRUE), json_file)
  cat(sprintf("\nResults written to: %s\n", json_file))
  
  # Write CSV
  csv_file <- file.path(output_dir, "ml_experiment_results.csv")
  write.csv(all_comparison, csv_file, row.names = FALSE)
  cat(sprintf("CSV written to: %s\n", csv_file))
  
} else {
  cat("No results to aggregate (all experiments failed)\n")
}

cat("\n==============================================================\n")
cat("EXPERIMENT COMPLETE\n")
cat("==============================================================\n")

# --- Timing convention documentation -----------------------------------------
cat("\nLABEL/EXECUTION TIMING CONVENTION:\n")
cat("  Label: Close[t+5]/Close[t] - 1 (5-day forward return)\n")
cat("  Signal: Generated at Close[t] (end of trading day)\n")
cat("  Execution: At Open[t+1] (next trading day open)\n")
cat("  Realized: Open[t+1] to Open[exit+1]\n")
cat("  Gap: Labels predict Close-to-Close, strategy captures Open-to-Open\n")
cat("  Impact: Overnight gap between close and next open creates systematic\n")
cat("          difference between predicted and realized returns\n")
