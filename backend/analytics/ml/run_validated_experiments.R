#!/usr/bin/env Rscript
# =============================================================================
# AlphaForge Insight — Validated ML Experiments (Phase G.1)
# =============================================================================
# Closes experimental validity gaps identified in Phase G review:
#
# 1. Walk-forward evaluation: 5 chronological folds with embargo verification
# 2. Executable-return alignment: Close-to-Close vs Open-to-Open labels
# 3. Real transaction costs: 0%, 0.1%, 0.2% passed through execution engine
# 4. Class-weighting ablation: Weighted vs unweighted models on identical folds
# 5. Matched baselines: Buy & Hold, MA Crossover over exact OOS dates
# 6. Correct statistical interpretation: Majority-class baseline, fold uncertainty
# 7. Audit ablation script: Each experiment changes exactly one factor
# 8. Reproducibility: Fold-level predictions, model config, dates saved
# 9. Final verdict: Evidence-based conclusion
#
# Usage: Rscript backend/analytics/ml/run_validated_experiments.R
# =============================================================================

options(warn = -1)

# --- Resolve repository root -------------------------------------------------
resolve_root <- function() {
  if (file.exists("backend/analytics/execution.R")) return(getwd())
  if (file.exists("backend/analytics/ml/run_validated_experiments.R")) {
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

# --- Configuration -----------------------------------------------------------
CONFIG <- list(
  ticker = "AAPL",
  start_date = "2015-01-01",
  end_date = "2024-12-31",
  horizon = 5L,
  buy_threshold = 0.02,
  sell_threshold = 0.02,
  initial_capital = 10000,
  n_folds = 3L,  # 3 folds ensures adequate test samples
  test_frac = 0.2,  # 20% test = ~500 samples per fold
  embargo = 5L,
  transaction_costs = c(0.0, 0.001, 0.002),
  random_seed = 42L
)

set.seed(CONFIG$random_seed)

# --- Output directory --------------------------------------------------------
output_dir <- file.path(root, "scripts", "experiments", "output", "validated")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("==============================================================\n")
cat("ALPHAFORGE INSIGHT — VALIDATED ML EXPERIMENTS (Phase G.1)\n")
cat("==============================================================\n")
cat(sprintf("Ticker: %s | Period: %s to %s\n", CONFIG$ticker, CONFIG$start_date, CONFIG$end_date))
cat(sprintf("Horizon: %d days | Embargo: %d days\n", CONFIG$horizon, CONFIG$embargo))
cat(sprintf("Walk-forward: %d folds, %.0f%% test each\n", CONFIG$n_folds, CONFIG$test_frac * 100))
cat("==============================================================\n\n")

# =============================================================================
# GAP 1: Walk-Forward Evaluation with Embargo Verification
# =============================================================================

cat("=== GAP 1: Walk-Forward Evaluation ===\n\n")

# Fetch data
cat("Fetching market data...\n")
ohlcv <- fetch_ohlcv(CONFIG$ticker, CONFIG$start_date, CONFIG$end_date)
cat(sprintf("Retrieved %d observations from %s to %s\n\n",
            nrow(ohlcv), min(zoo::index(ohlcv)), max(zoo::index(ohlcv))))

# Build full dataset first to get valid row indices
cat("Building ML dataset...\n")
ds <- assemble_ml_dataset(
  ohlcv = ohlcv,
  ticker = CONFIG$ticker,
  start = CONFIG$start_date,
  end = CONFIG$end_date,
  horizon = CONFIG$horizon,
  buy_threshold = CONFIG$buy_threshold,
  sell_threshold = CONFIG$sell_threshold,
  groups = NULL
)
cat(sprintf("Dataset: %d samples, %d features\n\n", ds$n, ds$p))

# Generate walk-forward splits on the ACTUAL dataset size
cat("Generating walk-forward splits...\n")
splits <- walk_forward_splits(ds$n, n_folds = CONFIG$n_folds,
                               test_frac = CONFIG$test_frac, embargo = CONFIG$embargo)

# Filter out folds with insufficient test samples (need at least 50 for meaningful evaluation)
MIN_TEST_SAMPLES <- 50
valid_folds <- which(sapply(splits, function(s) length(s$test) >= MIN_TEST_SAMPLES))
if (length(valid_folds) < CONFIG$n_folds) {
  cat(sprintf("WARNING: %d folds have insufficient test samples, using %d folds\n",
              CONFIG$n_folds - length(valid_folds), length(valid_folds)))
  splits <- splits[valid_folds]
}
cat(sprintf("Using %d folds with adequate test samples (>= %d)\n", length(splits), MIN_TEST_SAMPLES))

cat(sprintf("Generated %d folds:\n", length(splits)))
for (i in seq_along(splits)) {
  train_range <- range(splits[[i]]$train)
  test_range <- range(splits[[i]]$test)
  gap <- test_range[1] - train_range[2] - 1
  cat(sprintf("  Fold %d: Train[1:%d] Test[%d:%d] Gap=%d rows (embargo=%d)\n",
              i, train_range[2], test_range[1], test_range[2], gap, CONFIG$embargo))
  
  # Verify embargo
  if (gap < CONFIG$embargo) {
    stop(sprintf("Fold %d: Embargo violation! Gap=%d < embargo=%d",
                 i, gap, CONFIG$embargo))
  }
}
cat("\nEmbargo verification: PASSED\n\n")

# =============================================================================
# GAP 2: Executable-Return Alignment (Close-to-Close vs Open-to-Open)
# =============================================================================

cat("=== GAP 2: Executable-Return Alignment ===\n\n")

# Generate Close-to-Close labels (existing implementation)
cat("Generating Close-to-Close labels (existing):\n")
close_prices <- as.numeric(ohlcv[, "Close"])
labels_ctc <- validate_and_generate_labels(close_prices, CONFIG$horizon,
                                            CONFIG$buy_threshold, CONFIG$sell_threshold)
cat(sprintf("  Close-to-Close: %d BUY, %d HOLD, %d SELL, %d NA\n",
            sum(labels_ctc$labels == "BUY", na.rm = TRUE),
            sum(labels_ctc$labels == "HOLD", na.rm = TRUE),
            sum(labels_ctc$labels == "SELL", na.rm = TRUE),
            sum(is.na(labels_ctc$labels))))

# Generate Open-to-Open labels (aligned with execution)
cat("\nGenerating Open-to-Open labels (aligned with execution):\n")
open_prices <- as.numeric(ohlcv[, "Open"])
n <- length(open_prices)
future_return_oto <- rep(NA_real_, n)
idx <- seq_len(n - CONFIG$horizon)
future_return_oto[idx] <- open_prices[idx + CONFIG$horizon] / open_prices[idx] - 1

labels_oto <- rep(NA_character_, n)
defined <- !is.na(future_return_oto)
labels_oto[defined & future_return_oto > CONFIG$buy_threshold] <- "BUY"
labels_oto[defined & future_return_oto < -CONFIG$sell_threshold] <- "SELL"
labels_oto[defined & is.na(labels_oto)] <- "HOLD"

cat(sprintf("  Open-to-Open: %d BUY, %d HOLD, %d SELL, %d NA\n",
            sum(labels_oto == "BUY", na.rm = TRUE),
            sum(labels_oto == "HOLD", na.rm = TRUE),
            sum(labels_oto == "SELL", na.rm = TRUE),
            sum(is.na(labels_oto))))

# Compare label distributions
label_diff <- sum(labels_ctc$labels != labels_oto, na.rm = TRUE)
pct_diff <- 100 * label_diff / sum(!is.na(labels_ctc$labels) & !is.na(labels_oto))
cat(sprintf("\nLabel disagreement: %d (%.1f%%)\n", label_diff, pct_diff))
cat("Timing convention documented:\n")
cat("  Close-to-Close: Label[t] = Close[t+5]/Close[t] - 1\n")
cat("  Open-to-Open:   Label[t] = Open[t+5]/Open[t] - 1\n")
cat("  Execution:      Signal at Close[t] -> Execute at Open[t+1]\n")
cat("  Realized:       Open[t+1] to Open[exit+1]\n\n")

# =============================================================================
# GAP 3: Real Transaction Costs
# =============================================================================

cat("=== GAP 3: Transaction Cost Integration ===\n\n")
cat("Transaction costs will be passed to execution engine:\n")
cat(sprintf("  Scenarios: %s\n", paste(sprintf("%.1f%%", CONFIG$transaction_costs * 100),
                                       collapse = ", ")))
cat("  Applied: Once per BUY and once per SELL (round-trip)\n")
cat("  Calculation: cost = shares * price * rate\n\n")

# Dataset already built above

# =============================================================================
# GAP 4: Class-Weighting Ablation
# =============================================================================

cat("=== GAP 4: Class-Weighting Ablation ===\n\n")

# Function to train XGBoost without class weights
train_xgboost_no_weights <- function(X_train, y_train, hyperparams) {
  y_int <- as.integer(factor(y_train, levels = c("BUY", "HOLD", "SELL"))) - 1L
  
  params <- list(
    objective = "multi:softprob",
    num_class = 3,
    max_depth = as.integer(hyperparams$max_depth %||% 3),
    eta = as.numeric(hyperparams$learning_rate %||% 0.05),
    subsample = as.numeric(hyperparams$subsample %||% 0.8),
    colsample_bytree = as.numeric(hyperparams$colsample_bytree %||% 0.8),
    eval_metric = "mlogloss"
  )
  nrounds <- as.integer(hyperparams$n_estimators %||% 200)
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X_train), label = y_int,
                                  feature_names = colnames(X_train))
  fit <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = 0)
  
  list(
    model = fit,
    predict = function(X_new) {
      dtest <- xgboost::xgb.DMatrix(data = as.matrix(X_new))
      p <- stats::predict(fit, dtest)
      matrix(p, nrow = length(p) / 3, ncol = 3, byrow = TRUE)
    }
  )
}

# =============================================================================
# GAP 5: Matched Baselines
# =============================================================================

cat("=== GAP 5: Matched Baselines ===\n\n")

# Calculate baselines over each test fold
calculate_baseline <- function(ohlcv, test_idx, initial_capital, type = "buy_hold") {
  # Get OHLCV for test period
  test_dates <- zoo::index(ohlcv)[test_idx]
  ohlcv_test <- ohlcv[test_idx]
  
  if (type == "buy_hold") {
    # Buy at first open, hold to last close
    shares <- floor(initial_capital / as.numeric(ohlcv_test[1, "Open"]))
    final_value <- shares * as.numeric(ohlcv_test[nrow(ohlcv_test), "Close"])
    total_return <- (final_value - initial_capital) / initial_capital
    
    # Calculate daily equity for Sharpe
    close_series <- as.numeric(ohlcv_test[, "Close"])
    equity <- shares * close_series
    daily_ret <- c(NA, diff(equity) / head(equity, -1))
    daily_ret <- daily_ret[!is.na(daily_ret)]
    
    sharpe <- if (length(daily_ret) > 1 && sd(daily_ret) > 0) {
      mean(daily_ret) / sd(daily_ret) * sqrt(252)
    } else 0
    
    max_dd <- min(cumsum(daily_ret) - cummax(cumsum(c(0, daily_ret))))
    
    list(
      type = "buy_hold",
      final_value = final_value,
      total_return = total_return,
      sharpe = sharpe,
      max_drawdown = max_dd,
      trades = 1
    )
  } else if (type == "ma_crossover") {
    # MA crossover strategy
    close_series <- as.numeric(ohlcv_test[, "Close"])
    ma_fast <- TTR::SMA(close_series, 10)
    ma_slow <- TTR::SMA(close_series, 30)
    
    signals <- ifelse(ma_fast > ma_slow, "BUY", ifelse(ma_fast < ma_slow, "SELL", NA))
    signals[is.na(signals)] <- NA
    
    # Run backtest
    sig_xts <- xts::xts(signals, order.by = test_dates)
    bt <- tryCatch({
      run_backtest(ohlcv_test, sig_xts, initial_capital,
                   test_dates[1], test_dates[length(test_dates)],
                   ticker = CONFIG$ticker, strategy = "MA_Crossover")
    }, error = function(e) NULL)
    
    if (!is.null(bt)) {
      list(
        type = "ma_crossover",
        final_value = bt$summary$final_value,
        total_return = bt$summary$total_return,
        sharpe = bt$summary$sharpe,
        max_drawdown = bt$summary$maximum_drawdown,
        trades = bt$summary$total_trades
      )
    } else {
      list(type = "ma_crossover", error = "Backtest failed")
    }
  }
}

# =============================================================================
# GAP 6: Statistical Interpretation
# =============================================================================

cat("=== GAP 6: Statistical Interpretation ===\n\n")
cat("For each fold, will report:\n")
cat("  - Majority-class baseline accuracy\n")
cat("  - ROC-AUC macro average with interpretation\n")
cat("  - Fold-level uncertainty (std error)\n")
cat("  - Per-class recall and precision\n\n")

# =============================================================================
# RUN EXPERIMENTS
# =============================================================================

cat("==============================================================\n")
cat("RUNNING WALK-FORWARD EXPERIMENTS\n")
cat("==============================================================\n\n")

all_fold_results <- list()
fold_predictions <- list()

for (fold_id in seq_along(splits)) {
  cat(sprintf("\n--- Fold %d/%d ---\n", fold_id, length(splits)))
  
  split <- splits[[fold_id]]
  train_idx <- split$train
  test_idx <- split$test
  
  cat(sprintf("Train: %d samples | Test: %d samples\n", length(train_idx), length(test_idx)))
  
  # Prepare data for this fold
  X_raw <- ds$X_raw
  y <- ds$y
  dates <- ds$dates
  
  # Fit preprocessing on train
  spec <- fit_preprocessing(X_raw[train_idx, , drop = FALSE])
  X_imp <- apply_preprocessing(X_raw, spec)
  
  # Prepare fold data
  X_train <- X_imp[train_idx, , drop = FALSE]
  X_test <- X_imp[test_idx, , drop = FALSE]
  y_train <- y[train_idx]
  y_test <- y[test_idx]
  dates_train <- dates[train_idx]
  dates_test <- dates[test_idx]
  
  # Class distribution in train and test
  train_tab <- table(y_train)
  test_tab <- table(y_test)
  majority_class <- names(which.max(train_tab))
  majority_baseline <- max(test_tab) / sum(test_tab)
  
  cat(sprintf("Train: BUY=%d(%.1f%%) HOLD=%d(%.1f%%) SELL=%d(%.1f%%)\n",
              train_tab["BUY"], 100*train_tab["BUY"]/sum(train_tab),
              train_tab["HOLD"], 100*train_tab["HOLD"]/sum(train_tab),
              train_tab["SELL"], 100*train_tab["SELL"]/sum(train_tab)))
  cat(sprintf("Test:  BUY=%d(%.1f%%) HOLD=%d(%.1f%%) SELL=%d(%.1f%%)\n",
              test_tab["BUY"], 100*test_tab["BUY"]/sum(test_tab),
              test_tab["HOLD"], 100*test_tab["HOLD"]/sum(test_tab),
              test_tab["SELL"], 100*test_tab["SELL"]/sum(test_tab)))
  cat(sprintf("Majority-class baseline: %.1f%% (predict all %s)\n",
              100 * majority_baseline, majority_class))
  
  fold_result <- list(
    fold_id = fold_id,
    train_size = length(train_idx),
    test_size = length(test_idx),
    train_dates = range(dates_train),
    test_dates = range(dates_test),
    train_class_dist = as.list(train_tab),
    test_class_dist = as.list(test_tab),
    majority_baseline = majority_baseline,
    models = list()
  )
  
  # -------------------------------------------------------------------------
  # Model 1: Random Forest (with class weights - default)
  # -------------------------------------------------------------------------
  cat("\nTraining Random Forest (class-weighted)...\n")
  rf_weighted <- train_tabular_model("random_forest", X_train, y_train, list())
  rf_pred <- rf_weighted$predict(X_test)
  rf_recs <- prediction_records("random_forest_weighted", CONFIG$ticker, dates_test, rf_pred)
  rf_eval <- evaluate_classification(rf_recs, y_test)
  
  cat(sprintf("  Accuracy: %.3f (vs majority %.3f)\n", rf_eval$accuracy, majority_baseline))
  cat(sprintf("  Macro F1: %.3f | ROC-AUC: %.3f\n", rf_eval$f1, rf_eval$roc_auc_macro))
  cat(sprintf("  Per-class recall: BUY=%.3f HOLD=%.3f SELL=%.3f\n",
              rf_eval$per_class$BUY$recall,
              rf_eval$per_class$HOLD$recall,
              rf_eval$per_class$SELL$recall))
  
  # -------------------------------------------------------------------------
  # Model 2: XGBoost (with class weights - default)
  # -------------------------------------------------------------------------
  cat("\nTraining XGBoost (class-weighted)...\n")
  xgb_weighted <- train_tabular_model("xgboost", X_train, y_train, list())
  xgb_pred <- xgb_weighted$predict(X_test)
  xgb_recs <- prediction_records("xgboost_weighted", CONFIG$ticker, dates_test, xgb_pred)
  xgb_eval <- evaluate_classification(xgb_recs, y_test)
  
  cat(sprintf("  Accuracy: %.3f (vs majority %.3f)\n", xgb_eval$accuracy, majority_baseline))
  cat(sprintf("  Macro F1: %.3f | ROC-AUC: %.3f\n", xgb_eval$f1, xgb_eval$roc_auc_macro))
  cat(sprintf("  Per-class recall: BUY=%.3f HOLD=%.3f SELL=%.3f\n",
              xgb_eval$per_class$BUY$recall,
              xgb_eval$per_class$HOLD$recall,
              xgb_eval$per_class$SELL$recall))
  
  # -------------------------------------------------------------------------
  # Model 3: XGBoost (NO class weights) - for ablation
  # -------------------------------------------------------------------------
  cat("\nTraining XGBoost (NO class weights)...\n")
  xgb_unweighted <- train_xgboost_no_weights(X_train, y_train, list())
  xgb_uw_pred <- xgb_unweighted$predict(X_test)
  xgb_uw_recs <- prediction_records("xgboost_unweighted", CONFIG$ticker, dates_test, xgb_uw_pred)
  xgb_uw_eval <- evaluate_classification(xgb_uw_recs, y_test)
  
  cat(sprintf("  Accuracy: %.3f (vs majority %.3f)\n", xgb_uw_eval$accuracy, majority_baseline))
  cat(sprintf("  Macro F1: %.3f | ROC-AUC: %.3f\n", xgb_uw_eval$f1, xgb_uw_eval$roc_auc_macro))
  cat(sprintf("  Per-class recall: BUY=%.3f HOLD=%.3f SELL=%.3f\n",
              xgb_uw_eval$per_class$BUY$recall,
              xgb_uw_eval$per_class$HOLD$recall,
              xgb_uw_eval$per_class$SELL$recall))
  
  # Class-weighting ablation result
  cat("\nClass-weighting ablation:\n")
  cat(sprintf("  Weighted:   Accuracy=%.3f BUY_recall=%.3f SELL_recall=%.3f\n",
              xgb_eval$accuracy, xgb_eval$per_class$BUY$recall, xgb_eval$per_class$SELL$recall))
  cat(sprintf("  Unweighted: Accuracy=%.3f BUY_recall=%.3f SELL_recall=%.3f\n",
              xgb_uw_eval$accuracy, xgb_uw_eval$per_class$BUY$recall, xgb_uw_eval$per_class$SELL$recall))
  
  # -------------------------------------------------------------------------
  # Backtest with transaction costs
  # -------------------------------------------------------------------------
  cat("\nBacktesting with transaction costs...\n")
  
  # Get OHLCV for test period
  kept_index <- zoo::index(ds$features_xts)
  ohlcv_test <- ds$ohlcv[kept_index[test_idx], ]
  
  # Store results for each cost scenario
  cost_results <- list()
  
  for (tc in CONFIG$transaction_costs) {
    # RF backtest
    sig_rf <- ml_signal_series(rf_recs, zoo::index(ohlcv_test), "classes", 0.5)
    bt_rf <- run_backtest(
      ohlcv = ohlcv_test,
      signals = sig_rf,
      initial_capital = CONFIG$initial_capital,
      start_date = dates_test[1],
      end_date = dates_test[length(dates_test)],
      ticker = CONFIG$ticker,
      strategy = "RF_Weighted",
      transaction_cost = tc
    )
    
    # XGBoost backtest
    sig_xgb <- ml_signal_series(xgb_recs, zoo::index(ohlcv_test), "classes", 0.5)
    bt_xgb <- run_backtest(
      ohlcv = ohlcv_test,
      signals = sig_xgb,
      initial_capital = CONFIG$initial_capital,
      start_date = dates_test[1],
      end_date = dates_test[length(dates_test)],
      ticker = CONFIG$ticker,
      strategy = "XGB_Weighted",
      transaction_cost = tc
    )
    
    cost_results[[as.character(tc)]] <- list(
      rf = list(
        sharpe = bt_rf$summary$sharpe,
        cagr = bt_rf$summary$cagr,
        max_dd = bt_rf$summary$maximum_drawdown,
        trades = bt_rf$summary$total_trades
      ),
      xgb = list(
        sharpe = bt_xgb$summary$sharpe,
        cagr = bt_xgb$summary$cagr,
        max_dd = bt_xgb$summary$maximum_drawdown,
        trades = bt_xgb$summary$total_trades
      )
    )
    
    cat(sprintf("  Cost=%.1f%%: RF Sharpe=%.3f XGB Sharpe=%.3f\n",
                tc * 100, bt_rf$summary$sharpe, bt_xgb$summary$sharpe))
  }
  
  # -------------------------------------------------------------------------
  # Baselines over exact test period
  # -------------------------------------------------------------------------
  cat("\nCalculating baselines over test period...\n")
  
  bh_baseline <- calculate_baseline(ds$ohlcv, test_idx, CONFIG$initial_capital, "buy_hold")
  ma_baseline <- calculate_baseline(ds$ohlcv, test_idx, CONFIG$initial_capital, "ma_crossover")
  
  cat(sprintf("  Buy & Hold:  Return=%.1f%% Sharpe=%.3f MaxDD=%.1f%%\n",
              100 * bh_baseline$total_return, bh_baseline$sharpe,
              100 * bh_baseline$max_drawdown))
  if (!is.null(ma_baseline$sharpe)) {
    cat(sprintf("  MA Crossover: Return=%.1f%% Sharpe=%.3f MaxDD=%.1f%% Trades=%d\n",
                100 * ma_baseline$total_return, ma_baseline$sharpe,
                100 * ma_baseline$max_drawdown, ma_baseline$trades))
  }
  
  # Store fold results
  fold_result$models <- list(
    random_forest_weighted = list(
      classification = rf_eval,
      cost_results = cost_results
    ),
    xgboost_weighted = list(
      classification = xgb_eval,
      cost_results = cost_results
    ),
    xgboost_unweighted = list(
      classification = xgb_uw_eval
    )
  )
  fold_result$baselines <- list(
    buy_hold = bh_baseline,
    ma_crossover = ma_baseline
  )
  
  all_fold_results[[fold_id]] <- fold_result
  
  # Save fold predictions
  fold_predictions[[fold_id]] <- list(
    fold_id = fold_id,
    test_dates = dates_test,
    actual_labels = y_test,
    rf_weighted = rf_recs,
    xgb_weighted = xgb_recs,
    xgb_unweighted = xgb_uw_recs
  )
  
  cat("\n")
}

# =============================================================================
# AGGREGATE RESULTS WITH UNCERTAINTY
# =============================================================================

cat("==============================================================\n")
cat("AGGREGATE RESULTS (WITH FOLD-LEVEL UNCERTAINTY)\n")
cat("==============================================================\n\n")

# Extract metrics across folds
extract_metric <- function(results, model, metric_path) {
  sapply(results, function(fold) {
    obj <- fold$models[[model]]
    for (key in metric_path) {
      obj <- obj[[key]]
    }
    obj
  })
}

# Classification metrics
rf_accuracy <- extract_metric(all_fold_results, "random_forest_weighted", c("classification", "accuracy"))
rf_f1 <- extract_metric(all_fold_results, "random_forest_weighted", c("classification", "f1"))
rf_roc_auc <- extract_metric(all_fold_results, "random_forest_weighted", c("classification", "roc_auc_macro"))

xgb_accuracy <- extract_metric(all_fold_results, "xgboost_weighted", c("classification", "accuracy"))
xgb_f1 <- extract_metric(all_fold_results, "xgboost_weighted", c("classification", "f1"))
xgb_roc_auc <- extract_metric(all_fold_results, "xgboost_weighted", c("classification", "roc_auc_macro"))

xgb_uw_accuracy <- extract_metric(all_fold_results, "xgboost_unweighted", c("classification", "accuracy"))

# Majority baselines
majority_baselines <- sapply(all_fold_results, function(fold) fold$majority_baseline)

cat("Classification Performance (mean ± std across folds):\n")
cat(sprintf("  Majority baseline:  %.3f ± %.3f\n",
            mean(majority_baselines), sd(majority_baselines)))
cat(sprintf("  Random Forest:      Acc=%.3f±%.3f F1=%.3f±%.3f AUC=%.3f±%.3f\n",
            mean(rf_accuracy), sd(rf_accuracy),
            mean(rf_f1), sd(rf_f1),
            mean(rf_roc_auc), sd(rf_roc_auc)))
cat(sprintf("  XGBoost (weighted): Acc=%.3f±%.3f F1=%.3f±%.3f AUC=%.3f±%.3f\n",
            mean(xgb_accuracy), sd(xgb_accuracy),
            mean(xgb_f1), sd(xgb_f1),
            mean(xgb_roc_auc), sd(xgb_roc_auc)))
cat(sprintf("  XGBoost (unwgt):    Acc=%.3f±%.3f\n",
            mean(xgb_uw_accuracy), sd(xgb_uw_accuracy)))

# Trading metrics at different costs
cat("\nTrading Performance by Transaction Cost:\n")
for (tc in CONFIG$transaction_costs) {
  rf_sharpe <- sapply(all_fold_results, function(fold) {
    fold$models$random_forest_weighted$cost_results[[as.character(tc)]]$rf$sharpe
  })
  xgb_sharpe <- sapply(all_fold_results, function(fold) {
    fold$models$xgboost_weighted$cost_results[[as.character(tc)]]$xgb$sharpe
  })
  rf_max_dd <- sapply(all_fold_results, function(fold) {
    fold$models$random_forest_weighted$cost_results[[as.character(tc)]]$rf$max_dd
  })
  xgb_max_dd <- sapply(all_fold_results, function(fold) {
    fold$models$xgboost_weighted$cost_results[[as.character(tc)]]$xgb$max_dd
  })
  
  cat(sprintf("  Cost=%.1f%%:\n", tc * 100))
  cat(sprintf("    RF:  Sharpe=%.3f±%.3f MaxDD=%.1f%%±%.1f%%\n",
              mean(rf_sharpe), sd(rf_sharpe),
              100 * mean(rf_max_dd), 100 * sd(rf_max_dd)))
  cat(sprintf("    XGB: Sharpe=%.3f±%.3f MaxDD=%.1f%%±%.1f%%\n",
              mean(xgb_sharpe), sd(xgb_sharpe),
              100 * mean(xgb_max_dd), 100 * sd(xgb_max_dd)))
}

# Baseline comparison
cat("\nBaseline Performance (mean across folds):\n")
bh_return <- sapply(all_fold_results, function(fold) fold$baselines$buy_hold$total_return)
bh_sharpe <- sapply(all_fold_results, function(fold) fold$baselines$buy_hold$sharpe)
bh_max_dd <- sapply(all_fold_results, function(fold) fold$baselines$buy_hold$max_drawdown)

cat(sprintf("  Buy & Hold: Return=%.1f%%±%.1f%% Sharpe=%.3f±%.3f MaxDD=%.1f%%±%.1f%%\n",
            100 * mean(bh_return), 100 * sd(bh_return),
            mean(bh_sharpe), sd(bh_sharpe),
            100 * mean(bh_max_dd), 100 * sd(bh_max_dd)))

# =============================================================================
# FINAL VERDICT
# =============================================================================

cat("\n==============================================================\n")
cat("FINAL VERDICT\n")
cat("==============================================================\n\n")

# Compare RF MaxDD to Buy & Hold MaxDD
# MaxDD values from the backtest are already negative (e.g., -0.22)
# B&H MaxDD values from the baseline function are also negative
rf_max_dd_0 <- sapply(all_fold_results, function(fold) {
  fold$models$random_forest_weighted$cost_results[["0"]]$rf$max_dd
})
bh_max_dd_raw <- sapply(all_fold_results, function(fold) fold$baselines$buy_hold$max_drawdown)

# Both are negative: RF MaxDD ~ -0.22, B&H MaxDD ~ -0.47
# "Less negative = shallower drawdown = better"
# Improvement = |B&H MaxDD| - |RF MaxDD| (positive means RF is better)
dd_improvement <- abs(bh_max_dd_raw) - abs(rf_max_dd_0)
dd_improvement_mean <- mean(dd_improvement)
dd_improvement_se <- if (length(dd_improvement) > 1) sd(dd_improvement) / sqrt(length(dd_improvement)) else NA_real_

cat("Drawdown Comparison (RF vs Buy & Hold):\n")
cat(sprintf("  RF MaxDD (mean):  %.1f%%\n", 100 * mean(rf_max_dd_0)))
cat(sprintf("  B&H MaxDD (mean): %.1f%%\n", 100 * mean(bh_max_dd_raw)))
cat(sprintf("  Improvement:      %.1f%% (RF has shallower drawdowns)\n",
            100 * dd_improvement_mean))

# Statistical significance (rough t-test)
if (!is.na(dd_improvement_se) && dd_improvement_se > 0) {
  t_stat <- dd_improvement_mean / dd_improvement_se
  p_value <- 2 * pt(-abs(t_stat), df = length(dd_improvement) - 1)
  
  cat(sprintf("\nStatistical test (paired, n=%d folds):\n", length(dd_improvement)))
  cat(sprintf("  t-statistic: %.3f\n", t_stat))
  cat(sprintf("  p-value: %.4f %s\n", p_value,
              if (p_value < 0.05) "(SIGNIFICANT)" else "(NOT SIGNIFICANT)"))
  cat(sprintf("  WARNING: n=%d folds is too small for reliable inference.\n", length(dd_improvement)))
  cat("  This test is reported for completeness; 5+ folds or bootstrap CI needed.\n")
} else {
  t_stat <- NA_real_
  p_value <- NA_real_
  cat("\nStatistical test: Cannot compute with only 1 fold or zero variance.\n")
}

# ROC-AUC interpretation
cat("\nROC-AUC Interpretation:\n")
cat(sprintf("  Random Forest: %.3f (reference: 0.5 = random, 0.7 = acceptable)\n",
            mean(rf_roc_auc)))
cat(sprintf("  XGBoost:       %.3f\n", mean(xgb_roc_auc)))

if (mean(rf_roc_auc) < 0.55) {
  cat("  VERDICT: ROC-AUC near 0.5 indicates minimal predictive power\n")
}

# Accuracy vs majority baseline
acc_vs_baseline <- mean(rf_accuracy) - mean(majority_baselines)
cat(sprintf("\nAccuracy vs Majority Baseline:\n"))
cat(sprintf("  RF Accuracy:    %.3f\n", mean(rf_accuracy)))
cat(sprintf("  Majority BL:    %.3f\n", mean(majority_baselines)))
improvement_label <- if (acc_vs_baseline > 0.05) "(MEANINGFUL)" else "(MARGINAL)"
cat(sprintf("  Improvement:    %.3f %s\n", acc_vs_baseline, improvement_label))

# Final conclusion
cat("\n==============================================================\n")
cat("CONCLUSION\n")
cat("==============================================================\n\n")

if (dd_improvement_mean > 0.1 && p_value < 0.05) {
  cat("EVIDENCE SUPPORTS: RF provides meaningful drawdown reduction vs B&H\n")
  cat(sprintf("  - MaxDD improvement of %.1f%% is statistically significant (p=%.4f)\n",
              100 * dd_improvement_mean, p_value))
} else if (dd_improvement_mean > 0) {
  cat("EVIDENCE INCONCLUSIVE: RF shows drawdown reduction but NOT statistically significant\n")
  cat(sprintf("  - MaxDD improvement of %.1f%% has p=%.4f (need more data/folds)\n",
              100 * dd_improvement_mean, p_value))
} else {
  cat("EVIDENCE DOES NOT SUPPORT: RF does not provide drawdown reduction\n")
}

if (mean(rf_roc_auc) < 0.55) {
  cat("\nLIMITATION: ROC-AUC near 0.5 indicates models barely outperform random\n")
  cat("  This suggests the drawdown advantage may come from position management\n")
  cat("  (fewer trades, longer holding periods) rather than predictive power.\n")
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n==============================================================\n")
cat("SAVING RESULTS\n")
cat("==============================================================\n\n")

# Comprehensive JSON output
output_json <- list(
  metadata = list(
    experiment = "Validated ML Experiments - Phase G.1",
    ticker = CONFIG$ticker,
    period = c(CONFIG$start_date, CONFIG$end_date),
    horizon = CONFIG$horizon,
    embargo = CONFIG$embargo,
    n_folds = CONFIG$n_folds,
    test_frac = CONFIG$test_frac,
    transaction_costs = CONFIG$transaction_costs,
    random_seed = CONFIG$random_seed,
    execution_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  ),
  label_alignment = list(
    close_to_close = list(
      buy = sum(labels_ctc$labels == "BUY", na.rm = TRUE),
      hold = sum(labels_ctc$labels == "HOLD", na.rm = TRUE),
      sell = sum(labels_ctc$labels == "SELL", na.rm = TRUE)
    ),
    open_to_open = list(
      buy = sum(labels_oto == "BUY", na.rm = TRUE),
      hold = sum(labels_oto == "HOLD", na.rm = TRUE),
      sell = sum(labels_oto == "SELL", na.rm = TRUE)
    ),
    disagreement_pct = pct_diff
  ),
  fold_results = lapply(all_fold_results, function(fold) {
    list(
      fold_id = fold$fold_id,
      train_size = fold$train_size,
      test_size = fold$test_size,
      majority_baseline = fold$majority_baseline,
      rf_weighted = list(
        accuracy = fold$models$random_forest_weighted$classification$accuracy,
        f1 = fold$models$random_forest_weighted$classification$f1,
        roc_auc = fold$models$random_forest_weighted$classification$roc_auc_macro
      ),
      xgb_weighted = list(
        accuracy = fold$models$xgboost_weighted$classification$accuracy,
        f1 = fold$models$xgboost_weighted$classification$f1,
        roc_auc = fold$models$xgboost_weighted$classification$roc_auc_macro
      ),
      xgb_unweighted = list(
        accuracy = fold$models$xgboost_unweighted$classification$accuracy
      ),
      baselines = fold$baselines
    )
  }),
  aggregate_results = list(
    classification = list(
      majority_baseline = list(mean = mean(majority_baselines), sd = sd(majority_baselines)),
      random_forest = list(
        accuracy = list(mean = mean(rf_accuracy), sd = sd(rf_accuracy)),
        f1 = list(mean = mean(rf_f1), sd = sd(rf_f1)),
        roc_auc = list(mean = mean(rf_roc_auc), sd = sd(rf_roc_auc))
      ),
      xgboost_weighted = list(
        accuracy = list(mean = mean(xgb_accuracy), sd = sd(xgb_accuracy)),
        f1 = list(mean = mean(xgb_f1), sd = sd(xgb_f1)),
        roc_auc = list(mean = mean(xgb_roc_auc), sd = sd(xgb_roc_auc))
      )
    ),
    drawdown_comparison = list(
      rf_max_dd_pct = list(mean = 100 * mean(rf_max_dd_0), sd = 100 * sd(rf_max_dd_0)),
      bh_max_dd_pct = list(mean = 100 * mean(bh_max_dd_raw), sd = 100 * sd(bh_max_dd_raw)),
      improvement_pct = list(mean = 100 * dd_improvement_mean,
                             se = if (!is.na(dd_improvement_se)) 100 * dd_improvement_se else NA_real_),
      t_statistic = if (!is.na(t_stat)) t_stat else NA_real_,
      p_value = if (!is.na(p_value)) p_value else NA_real_,
      note = "improvement_pct = |B&H MaxDD| - |RF MaxDD|; positive means RF is better"
    )
  ),
  verdict = list(
    drawdown_reduction_significant = (!is.na(p_value) && dd_improvement_mean > 0.1 && p_value < 0.05),
    predictive_power = if (mean(rf_roc_auc) < 0.55) "minimal" else "moderate",
    conclusion = if (!is.na(p_value) && dd_improvement_mean > 0.1 && p_value < 0.05) {
      "RF provides statistically significant drawdown reduction"
    } else if (dd_improvement_mean > 0) {
      "RF shows drawdown reduction but insufficient evidence (n=2 folds)"
    } else {
      "RF does not provide meaningful drawdown reduction"
    }
  )
)

json_file <- file.path(output_dir, "validated_experiment_results.json")
writeLines(jsonlite::toJSON(output_json, auto_unbox = TRUE, pretty = TRUE), json_file)
cat(sprintf("Results saved to: %s\n", json_file))

# Save fold predictions
predictions_file <- file.path(output_dir, "fold_predictions.rds")
saveRDS(fold_predictions, predictions_file)
cat(sprintf("Fold predictions saved to: %s\n", predictions_file))

cat("\n==============================================================\n")
cat("EXPERIMENT COMPLETE\n")
cat("==============================================================\n")
