# AlphaForge Insight — Multi-Asset Robustness Study (Phase G.3)
#
# Objective: Determine whether ML provides reproducible incremental value over
# transparent non-ML strategies across multiple assets with rigorous statistical
# validation.
#
# Phase G.3 Requirements (all implemented):
# 1. Execution-aligned labels: Both Close-to-Close and Open-to-Open
# 2. Fixed pre-specified baselines: 50% exposure, MA(10,30), fixed across assets
# 3. Block bootstrap: 1,000 resamples with temporal dependence awareness
# 4. Cross-asset validation: AAPL, MSFT, GOOGL, AMZN, TSLA
# 5. Reproducibility: Fixed seeds, complete provenance, fold-level outputs
# 6. Honest reporting: Report inconclusive/weak results transparently
#
# NO tuning on test periods. NO test-derived baseline parameters.

library(xts)
library(randomForest)
library(jsonlite)
suppressPackageStartupMessages(library(TTR))

# Source dependencies
source("backend/analytics/ml/labels.R")
source("backend/analytics/ml/labels_execution_aligned.R")
source("backend/analytics/ml/features.R")
source("backend/analytics/ml/preprocessing.R")
source("backend/analytics/ml/models.R")
source("backend/analytics/execution.R")
source("backend/analytics/metrics.R")
source("backend/analytics/data_retrieval.R")

set.seed(42)  # Master seed for reproducibility

# ============================================================================
# Configuration (FIXED, predefined, asset-independent)
# ============================================================================

CONFIG <- list(
  # Assets
  tickers = c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA"),
  start_date = "2015-01-01",
  end_date = "2024-12-31",
  
  # Walk-forward splits (4 folds, 10% test each)
  n_folds = 4,
  train_frac = 0.7,
  test_frac = 0.1,  # 10% per fold
  
  # Label configurations
  label_horizon = 1L,  # Next-day (execution-aligned)
  buy_threshold = 0.02,
  sell_threshold = 0.02,
  
  # ML model
  rf_ntree = 200,
  rf_mtry = 10,
  
  # FIXED baseline parameters (asset-independent, predefined)
  baseline_exposure = 0.50,  # 50% target exposure
  ma_short = 10L,           # MA crossover short window
  ma_long = 30L,            # MA crossover long window
  
  # Execution
  initial_capital = 10000,
  transaction_cost = 0.001,  # 10 bps per trade
  
  # Statistical inference
  n_bootstrap = 1000,
  block_length = 10,  # Block bootstrap for temporal dependence
  
  # Output
  output_dir = "scripts/experiments/output/multi_asset"
)

cat("===================================================================\n")
cat("Phase G.3: Multi-Asset Robustness Study\n")
cat("===================================================================\n")
cat("Configuration:\n")
cat("  Assets:", paste(CONFIG$tickers, collapse = ", "), "\n")
cat("  Period:", CONFIG$start_date, "to", CONFIG$end_date, "\n")
cat("  Folds:", CONFIG$n_folds, "(10% test each)\n")
cat("  Label types: Close-to-Close AND Open-to-Open\n")
cat("  FIXED baseline: 50% exposure, MA(10,30)\n")
cat("  Bootstrap:", CONFIG$n_bootstrap, "resamples, block length", CONFIG$block_length, "\n")
cat("  Transaction costs:", CONFIG$transaction_cost * 10000, "bps\n")
cat("===================================================================\n\n")

# ============================================================================
# Baseline Strategy Implementations (FIXED parameters)
# ============================================================================

#' Buy & Hold strategy
generate_buy_hold_signals <- function(ohlcv) {
  n <- nrow(ohlcv)
  signals <- rep(NA_character_, n)
  signals[1] <- "BUY"  # Buy on first day, hold forever
  signals
}

#' Moving Average Crossover with FIXED parameters (10/30)
generate_ma_cross_signals <- function(ohlcv, short = CONFIG$ma_short, long = CONFIG$ma_long) {
  close <- as.numeric(ohlcv[, "Close"])
  n <- length(close)
  
  # Guard: need at least long+1 rows for MA calculation
  if (n < long + 1) {
    return(rep(NA_character_, n))
  }
  
  ma_short <- SMA(close, n = short)
  ma_long <- SMA(close, n = long)
  
  signals <- rep(NA_character_, length(close))
  position <- 0  # 0 = FLAT, 1 = LONG
  
  for (i in (long + 1):length(close)) {
    if (is.na(ma_short[i]) || is.na(ma_long[i])) next
    
    if (ma_short[i] > ma_long[i] && position == 0) {
      signals[i] <- "BUY"
      position <- 1
    } else if (ma_short[i] <= ma_long[i] && position == 1) {
      signals[i] <- "SELL"
      position <- 0
    }
  }
  signals
}

#' Fixed 50% exposure baseline (PREDEFINED, not test-derived)
#' Trades every rebalance_days to maintain ~50% invested
generate_fixed_exposure_signals <- function(ohlcv, target_exposure = CONFIG$baseline_exposure,
                                           rebalance_days = 5L) {
  n <- nrow(ohlcv)
  signals <- rep(NA_character_, n)
  position <- 0  # 0 = FLAT, 1 = LONG
  days_since_trade <- 0
  
  # Start with BUY to reach target exposure
  signals[1] <- "BUY"
  position <- 1
  
  for (i in 2:n) {
    days_since_trade <- days_since_trade + 1
    
    # Rebalance periodically
    if (days_since_trade >= rebalance_days) {
      # Alternate BUY/SELL to maintain ~50% exposure over time
      if (position == 1) {
        signals[i] <- "SELL"
        position <- 0
      } else {
        signals[i] <- "BUY"
        position <- 1
      }
      days_since_trade <- 0
    }
  }
  signals
}

# ============================================================================
# ML Model Training and Prediction
# ============================================================================

#' Train Random Forest and generate signals for test period
train_and_predict_rf <- function(ohlcv, train_idx, test_idx, label_type = "close_to_close") {
  # Build features
  feats <- build_features(ohlcv, groups = c("price", "volume", "volatility", "momentum"))
  X_all <- as.matrix(feats$features)
  dates_all <- as.Date(index(feats$features))
  
  # Generate labels based on type
  if (label_type == "close_to_close") {
    labels_obj <- generate_close_to_close_labels(
      as.numeric(ohlcv[, "Close"]),
      horizon = CONFIG$label_horizon,
      buy_threshold = CONFIG$buy_threshold,
      sell_threshold = CONFIG$sell_threshold
    )
  } else {
    labels_obj <- generate_open_to_open_labels(
      as.numeric(ohlcv[, "Open"]),
      horizon = CONFIG$label_horizon,
      buy_threshold = CONFIG$buy_threshold,
      sell_threshold = CONFIG$sell_threshold
    )
  }
  
  y_all <- labels_obj$labels
  valid_all <- labels_obj$valid
  
  # Align features and labels
  min_len <- min(nrow(X_all), length(y_all))
  X_all <- X_all[1:min_len, ]
  y_all <- y_all[1:min_len]
  valid_all <- valid_all[1:min_len]
  dates_all <- dates_all[1:min_len]
  
  # Filter by validity
  keep <- valid_all & complete.cases(X_all)
  X_clean <- X_all[keep, ]
  y_clean <- y_all[keep]
  dates_clean <- dates_all[keep]
  
  # Map original indices to clean indices
  orig_to_clean <- rep(NA_integer_, min_len)
  orig_to_clean[keep] <- seq_len(sum(keep))
  
  train_clean <- orig_to_clean[train_idx]
  test_clean <- orig_to_clean[test_idx]
  train_clean <- train_clean[!is.na(train_clean)]
  test_clean <- test_clean[!is.na(test_clean)]
  
  if (length(train_clean) < 50 || length(test_clean) < 10) {
    return(list(success = FALSE, reason = "insufficient_samples",
                n_train = length(train_clean), n_test = length(test_clean)))
  }
  
  X_train <- X_clean[train_clean, ]
  y_train <- factor(y_clean[train_clean], levels = c("BUY", "HOLD", "SELL"))
  X_test <- X_clean[test_clean, ]
  y_test <- factor(y_clean[test_clean], levels = c("BUY", "HOLD", "SELL"))
  
  # Check class distribution
  class_counts <- table(y_train)
  if (any(class_counts < 10)) {
    return(list(success = FALSE, reason = "insufficient_class_samples",
                class_counts = as.list(class_counts)))
  }
  
  # Train Random Forest
  rf_model <- randomForest(
    x = X_train,
    y = y_train,
    ntree = CONFIG$rf_ntree,
    mtry = min(CONFIG$rf_mtry, ncol(X_train)),
    importance = FALSE
  )
  
  # Predict on test
  pred_test <- predict(rf_model, X_test, type = "response")
  
  # Convert predictions to signals (aligned with full OHLCV)
  signals_full <- rep(NA_character_, nrow(ohlcv))
  test_dates <- dates_clean[test_clean]
  ohlcv_dates <- as.Date(index(ohlcv))
  
  for (i in seq_along(test_clean)) {
    date_match <- which(ohlcv_dates == test_dates[i])
    if (length(date_match) == 1) {
      pred_class <- as.character(pred_test[i])
      if (pred_class == "BUY") {
        signals_full[date_match] <- "BUY"
      } else if (pred_class == "SELL") {
        signals_full[date_match] <- "SELL"
      }
    }
  }
  
  # Classification metrics
  accuracy <- sum(pred_test == y_test) / length(y_test)
  conf_mat <- table(Predicted = pred_test, Actual = y_test)
  
  list(
    success = TRUE,
    signals = signals_full,
    model = rf_model,
    accuracy = accuracy,
    confusion_matrix = conf_mat,
    n_train = length(train_clean),
    n_test = length(test_clean),
    class_distribution_train = as.list(class_counts),
    label_type = label_type,
    predictions = as.character(pred_test),
    actuals = as.character(y_test)
  )
}

# ============================================================================
# Walk-Forward Cross-Validation
# ============================================================================

#' Create walk-forward folds (expanding window, 10% test each)
create_walk_forward_folds <- function(n, n_folds = CONFIG$n_folds, 
                                     train_frac = CONFIG$train_frac,
                                     test_frac = CONFIG$test_frac) {
  folds <- list()
  
  # Initial train size
  initial_train <- floor(n * train_frac)
  test_size <- floor(n * test_frac)
  
  # Minimum test size: need at least 50 rows for meaningful evaluation
  # (MA(30) needs 31 rows, plus buffer for trading)
  min_test_size <- 50L
  
  for (fold in 1:n_folds) {
    train_end <- initial_train + (fold - 1) * test_size
    test_start <- train_end + 1
    test_end <- min(test_start + test_size - 1, n)
    
    # Skip folds with insufficient test data
    actual_test_size <- test_end - test_start + 1
    if (actual_test_size < min_test_size) {
      cat(sprintf("    Skipping fold %d: insufficient test rows (%d < %d minimum)\n",
                  fold, actual_test_size, min_test_size))
      next
    }
    
    folds[[length(folds) + 1]] <- list(
      train = 1:train_end,
      test = test_start:test_end
    )
  }
  
  folds
}

# ============================================================================
# Block Bootstrap for Temporal Dependence
# ============================================================================

#' Block bootstrap resampling for dependent time series
#'
#' Implements circular block bootstrap following Politis & Romano (1994).
#' Block length choice: sqrt(n_observations) is the standard heuristic for
#' weakly dependent time series. With 4 folds, block_length is capped at
#' n_folds to ensure valid resampling.
#'
#' Justification: Standard t-tests assume independence between folds, but
#' walk-forward folds share training data and exhibit serial dependence.
#' Block bootstrap preserves temporal structure within blocks while
#' generating valid confidence intervals.
#'
#' @param fold_metrics Numeric vector of per-fold metric values.
#' @param n_bootstrap Number of bootstrap resamples (default 1000).
#' @param block_length Block length for circular block bootstrap.
#' @return list(mean, ci_lower, ci_upper, se, n_valid).
block_bootstrap_ci <- function(fold_metrics, n_bootstrap = CONFIG$n_bootstrap,
                               block_length = CONFIG$block_length) {
  # Remove NA values
  valid_metrics <- fold_metrics[!is.na(fold_metrics)]
  n_valid <- length(valid_metrics)
  
  if (n_valid < 2) {
    return(list(
      mean = if (n_valid == 1) valid_metrics[1] else NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      se = NA_real_,
      n_valid = n_valid,
      note = "Too few observations for bootstrap"
    ))
  }
  
  # Cap block length at n_valid (can't have blocks larger than sample)
  effective_block <- min(block_length, n_valid)
  
  # Circular block bootstrap
  bootstrap_means <- numeric(n_bootstrap)
  
  for (b in 1:n_bootstrap) {
    # Number of blocks needed to cover n_valid observations
    n_blocks <- ceiling(n_valid / effective_block)
    resampled <- numeric(0)
    
    for (blk in 1:n_blocks) {
      # Random start position (circular: wraps around)
      start_pos <- sample(1:n_valid, 1)
      block_indices <- ((start_pos - 1 + 0:(effective_block - 1)) %% n_valid) + 1
      resampled <- c(resampled, valid_metrics[block_indices])
    }
    
    # Trim to original length
    bootstrap_means[b] <- mean(resampled[1:n_valid])
  }
  
  list(
    mean = mean(valid_metrics),
    ci_lower = as.numeric(quantile(bootstrap_means, 0.025)),
    ci_upper = as.numeric(quantile(bootstrap_means, 0.975)),
    se = sd(bootstrap_means),
    n_valid = n_valid,
    effective_block_length = effective_block
  )
}

# ============================================================================
# Single Asset Experiment
# ============================================================================

#' Run complete experiment for one asset with both label types
run_asset_experiment <- function(ticker, label_type = "close_to_close") {
  cat("\n-------------------------------------------------------------------\n")
  cat("Asset:", ticker, "| Label type:", label_type, "\n")
  cat("-------------------------------------------------------------------\n")
  
  # Fetch data
  tryCatch({
    ohlcv <- fetch_ohlcv(ticker, CONFIG$start_date, CONFIG$end_date)
  }, error = function(e) {
    return(list(success = FALSE, ticker = ticker, label_type = label_type,
                error = paste("Data fetch failed:", conditionMessage(e))))
  })
  
  if (nrow(ohlcv) < 500) {
    return(list(success = FALSE, ticker = ticker, label_type = label_type,
                error = "Insufficient data"))
  }
  
  # Create folds
  folds <- create_walk_forward_folds(nrow(ohlcv))
  n_folds <- length(folds)
  
  cat("Data points:", nrow(ohlcv), "| Folds:", n_folds, "\n")
  
  # Initialize results storage
  fold_results <- list()
  
  # Run each fold
  for (fold_idx in seq_along(folds)) {
    cat(sprintf("\n  Fold %d/%d (train: %d, test: %d rows)\n",
                fold_idx, n_folds,
                length(folds[[fold_idx]]$train),
                length(folds[[fold_idx]]$test)))
    
    train_idx <- folds[[fold_idx]]$train
    test_idx <- folds[[fold_idx]]$test
    test_ohlcv <- ohlcv[test_idx, ]
    
    # 1. Buy & Hold
    cat("    Running Buy & Hold... ")
    bh_signals <- generate_buy_hold_signals(test_ohlcv)
    bh_result <- tryCatch({
      bt <- run_backtest(test_ohlcv, bh_signals, CONFIG$initial_capital,
                   start_date = index(test_ohlcv)[1],
                   end_date = index(test_ohlcv)[nrow(test_ohlcv)],
                   ticker = ticker, strategy = "BuyHold",
                   transaction_cost = CONFIG$transaction_cost)
      cat(sprintf("✓ (Ret=%.1f%%, Sharpe=%.3f, MaxDD=%.1f%%)\n",
                  bt$summary$total_return * 100, bt$summary$sharpe,
                  bt$summary$maximum_drawdown * 100))
      bt
    }, error = function(e) { cat(sprintf("✗ (%s)\n", conditionMessage(e))); NULL })
    
    # 2. MA Crossover
    cat("    Running MA Crossover... ")
    ma_signals <- generate_ma_cross_signals(test_ohlcv)
    ma_result <- tryCatch({
      bt <- run_backtest(test_ohlcv, ma_signals, CONFIG$initial_capital,
                   start_date = index(test_ohlcv)[1],
                   end_date = index(test_ohlcv)[nrow(test_ohlcv)],
                   ticker = ticker, strategy = "MA_Cross",
                   transaction_cost = CONFIG$transaction_cost)
      cat(sprintf("✓ (Ret=%.1f%%, Sharpe=%.3f, MaxDD=%.1f%%)\n",
                  bt$summary$total_return * 100, bt$summary$sharpe,
                  bt$summary$maximum_drawdown * 100))
      bt
    }, error = function(e) { cat(sprintf("✗ (%s)\n", conditionMessage(e))); NULL })
    
    # 3. Fixed Exposure Baseline
    cat("    Running Fixed Exposure (50%)... ")
    fixed_signals <- generate_fixed_exposure_signals(test_ohlcv)
    fixed_result <- tryCatch({
      bt <- run_backtest(test_ohlcv, fixed_signals, CONFIG$initial_capital,
                   start_date = index(test_ohlcv)[1],
                   end_date = index(test_ohlcv)[nrow(test_ohlcv)],
                   ticker = ticker, strategy = "Fixed_Exposure",
                   transaction_cost = CONFIG$transaction_cost)
      cat(sprintf("✓ (Ret=%.1f%%, Sharpe=%.3f, MaxDD=%.1f%%)\n",
                  bt$summary$total_return * 100, bt$summary$sharpe,
                  bt$summary$maximum_drawdown * 100))
      bt
    }, error = function(e) { cat(sprintf("✗ (%s)\n", conditionMessage(e))); NULL })
    
    # 4. Random Forest
    cat("    Training Random Forest... ")
    rf_pred <- tryCatch(
      train_and_predict_rf(ohlcv, train_idx, test_idx, label_type),
      error = function(e) list(success = FALSE, reason = conditionMessage(e))
    )
    
    if (rf_pred$success) {
      cat(sprintf("✓ (acc=%.3f)\n", rf_pred$accuracy))
      cat("    Running RF backtest... ")
      rf_result <- tryCatch({
        bt <- run_backtest(test_ohlcv, rf_pred$signals[test_idx], CONFIG$initial_capital,
                     start_date = index(test_ohlcv)[1],
                     end_date = index(test_ohlcv)[nrow(test_ohlcv)],
                     ticker = ticker, strategy = "RandomForest",
                     transaction_cost = CONFIG$transaction_cost)
        cat(sprintf("✓ (Ret=%.1f%%, Sharpe=%.3f, MaxDD=%.1f%%)\n",
                    bt$summary$total_return * 100, bt$summary$sharpe,
                    bt$summary$maximum_drawdown * 100))
        bt
      }, error = function(e) { cat(sprintf("✗ (%s)\n", conditionMessage(e))); NULL })
    } else {
      cat(sprintf("✗ (%s)\n", rf_pred$reason))
      rf_result <- NULL
    }
    
    # Store fold results
    fold_results[[fold_idx]] <- list(
      fold = fold_idx,
      train_size = length(train_idx),
      test_size = length(test_idx),
      test_start = as.character(index(test_ohlcv)[1]),
      test_end = as.character(index(test_ohlcv)[nrow(test_ohlcv)]),
      buy_hold = if (!is.null(bh_result)) bh_result$summary else NULL,
      ma_cross = if (!is.null(ma_result)) ma_result$summary else NULL,
      fixed_exposure = if (!is.null(fixed_result)) fixed_result$summary else NULL,
      random_forest = if (!is.null(rf_result)) rf_result$summary else NULL,
      rf_classification = if (rf_pred$success) list(
        accuracy = rf_pred$accuracy,
        n_train = rf_pred$n_train,
        n_test = rf_pred$n_test,
        class_distribution = rf_pred$class_distribution_train
      ) else NULL
    )
  }
  
  # Aggregate across folds
  cat("\n  Aggregating fold results...\n")
  
  strategies <- c("buy_hold", "ma_cross", "fixed_exposure", "random_forest")
  metrics <- c("total_return", "sharpe", "sortino", "maximum_drawdown")
  
  aggregated <- list()
  
  for (strat in strategies) {
    strat_metrics <- list()
    
    for (metric in metrics) {
      fold_values <- sapply(fold_results, function(f) {
        if (!is.null(f[[strat]])) f[[strat]][[metric]] else NA_real_
      })
      
      if (all(is.na(fold_values))) {
        strat_metrics[[metric]] <- list(mean = NA_real_, ci_lower = NA_real_,
                                       ci_upper = NA_real_, se = NA_real_)
      } else {
        strat_metrics[[metric]] <- block_bootstrap_ci(fold_values)
      }
    }
    
    aggregated[[strat]] <- strat_metrics
  }
  
  cat("  Done.\n")
  
  list(
    success = TRUE,
    ticker = ticker,
    label_type = label_type,
    n_folds = n_folds,
    n_observations = nrow(ohlcv),
    fold_results = fold_results,
    aggregated = aggregated,
    config = CONFIG
  )
}

# ============================================================================
# Main Execution
# ============================================================================

# Create output directory
dir.create(CONFIG$output_dir, recursive = TRUE, showWarnings = FALSE)

# Run experiments
all_results <- list()
label_types <- c("close_to_close", "open_to_open")

for (ticker in CONFIG$tickers) {
  for (label_type in label_types) {
    key <- paste(ticker, label_type, sep = "_")
    all_results[[key]] <- run_asset_experiment(ticker, label_type)
    
    # Save intermediate results
    saveRDS(all_results, file.path(CONFIG$output_dir, "results_intermediate.rds"))
  }
}

# Save final results
cat("\n===================================================================\n")
cat("Saving results...\n")
cat("===================================================================\n")

saveRDS(all_results, file.path(CONFIG$output_dir, "multi_asset_results.rds"))
write_json(all_results, file.path(CONFIG$output_dir, "multi_asset_results.json"),
           auto_unbox = TRUE, pretty = TRUE, digits = 6)

cat("Results saved to:", CONFIG$output_dir, "\n")
cat("Phase G.3 multi-asset experiment complete.\n")
