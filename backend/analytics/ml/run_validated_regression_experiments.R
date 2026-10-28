# AlphaForge Insight — Leakage-Free Validated Regression Experiments (Phase H.2)
#
# Implements rigorous out-of-sample validation:
#   1. Threshold determined from training fold only (no test leakage)
#   2. Block bootstrap for statistical uncertainty
#   3. Matched models and baselines on identical dates
#   4. Multi-asset robustness testing
#   5. Complete reproducibility with fold metadata
#
# Addresses Phase H.2 audit findings:
#   - Adaptive threshold was computed on test predictions (leakage)
#   - Trading metrics were not valid out-of-sample
#   - Statistical uncertainty was not quantified

library(xts)

# Set ML directory globally
.ML_DIR <- file.path(getwd(), "backend", "analytics", "ml")

# =============================================================================
# CONFIGURATION
# =============================================================================

PHASE_H2_CONFIG <- list(
  # Experiment settings
  tickers = c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA"),
  start_date = "2020-01-01",
  end_date = NULL,  # Will use Sys.Date() - 30
  horizon = 5,
  n_folds = 3,
  test_frac = 0.1,
  seed = 42,
  
  # Threshold settings (LEAKAGE-FREE)
  threshold_method = "percentile",  # "fixed", "percentile", "std"
  fixed_threshold = 0.005,  # 0.5% for 5-day horizon
  train_percentile = 0.20,  # Top/bottom 20% from TRAINING predictions
  
  # Trading settings
  initial_capital = 10000,
  transaction_cost = 0.001,  # 10 bps
  
  # Statistical settings
  bootstrap_samples = 1000,
  block_length = 20,  # For block bootstrap (preserve temporal dependence)
  
  # Output
  output_dir = "research/results"
)

# =============================================================================
# LEAKAGE-FREE THRESHOLD SELECTION
# =============================================================================

#' Compute threshold from TRAINING data only.
#'
#' This is the corrected version of compute_adaptive_threshold that prevents
#' test-set leakage by computing the threshold from training fold predictions.
#'
#' @param train_predictions Predictions on training data (from cross-validation or OOF).
#' @param train_actuals Actual values for training data.
#' @param method One of: "percentile", "std", "fixed".
#' @param percentile Percentile for percentile method (default 0.20).
#' @param std_multiple Number of standard deviations for std method (default 1.0).
#' @param fixed_value Fixed threshold value (default 0.005).
#' @return list(threshold_buy, threshold_sell, method, provenance).
compute_train_threshold <- function(train_predictions, train_actuals,
                                    method = "percentile",
                                    percentile = 0.20,
                                    std_multiple = 1.0,
                                    fixed_value = 0.005) {
  
  train_predictions <- as.numeric(train_predictions)
  train_actuals <- as.numeric(train_actuals)
  
  # Remove NA pairs
  ok <- !is.na(train_predictions) & !is.na(train_actuals)
  train_pred <- train_predictions[ok]
  train_act <- train_actuals[ok]
  
  if (length(train_pred) < 20) {
    warning("Insufficient training predictions for threshold estimation, using fixed value")
    return(list(
      threshold_buy = fixed_value,
      threshold_sell = -fixed_value,
      method = "fixed_fallback",
      provenance = "insufficient_train_data",
      n_train_obs = length(train_pred)
    ))
  }
  
  if (method == "fixed") {
    # Fixed pre-specified threshold
    list(
      threshold_buy = fixed_value,
      threshold_sell = -fixed_value,
      method = "fixed",
      provenance = "pre_specified",
      n_train_obs = length(train_pred)
    )
    
  } else if (method == "percentile") {
    # Use percentiles of TRAINING prediction distribution
    threshold_buy <- quantile(train_pred, probs = 1 - percentile, na.rm = TRUE)
    threshold_sell <- quantile(train_pred, probs = percentile, na.rm = TRUE)
    
    list(
      threshold_buy = as.numeric(threshold_buy),
      threshold_sell = as.numeric(threshold_sell),
      method = "train_percentile",
      provenance = "training_predictions_only",
      percentile = percentile,
      n_train_obs = length(train_pred)
    )
    
  } else if (method == "std") {
    # Use standard deviations from training mean
    train_mean <- mean(train_pred, na.rm = TRUE)
    train_sd <- sd(train_pred, na.rm = TRUE)
    
    threshold_buy <- train_mean + std_multiple * train_sd
    threshold_sell <- train_mean - std_multiple * train_sd
    
    list(
      threshold_buy = as.numeric(threshold_buy),
      threshold_sell = as.numeric(threshold_sell),
      method = "train_std",
      provenance = "training_predictions_only",
      std_multiple = std_multiple,
      n_train_obs = length(train_pred)
    )
    
  } else {
    stop(sprintf("Unknown threshold method: %s", method))
  }
}

#' Apply pre-computed threshold to generate signals.
#'
#' @param predictions Test predictions.
#' @param threshold Pre-computed threshold from compute_train_threshold().
#' @return Character vector of signals ("BUY", "SELL", NA).
apply_threshold <- function(predictions, threshold) {
  predictions <- as.numeric(predictions)
  signals <- rep(NA_character_, length(predictions))
  
  signals[predictions > threshold$threshold_buy] <- "BUY"
  signals[predictions < threshold$threshold_sell] <- "SELL"
  
  signals
}

# =============================================================================
# WALK-FORWARD EVALUATION WITH LEAKAGE-FREE THRESHOLD
# =============================================================================

#' Run walk-forward evaluation with leakage-free threshold selection.
#'
#' This is the corrected version that computes thresholds from training data only.
#'
#' @param ds Dataset from assemble_regression_dataset().
#' @param model_key Model to evaluate.
#' @param config Configuration list.
#' @return list with fold results, predictions, and threshold provenance.
evaluate_model_leakage_free <- function(ds, model_key, config = PHASE_H2_CONFIG) {
  
  ml_dir <- .ML_DIR
  source(file.path(ml_dir, "preprocessing.R"), local = TRUE)
  source(file.path(ml_dir, "regression_models.R"), local = TRUE)
  source(file.path(ml_dir, "regression_evaluation.R"), local = TRUE)
  source(file.path(ml_dir, "regression_trading.R"), local = TRUE)
  
  # Generate walk-forward splits
  splits <- walk_forward_splits(ds$n, n_folds = config$n_folds, 
                                test_frac = config$test_frac,
                                embargo = ds$horizon)
  
  folds <- list()
  all_preds <- c()
  all_actuals <- c()
  all_dates <- c()
  all_fold_ids <- c()
  all_thresholds <- list()
  
  for (i in seq_along(splits)) {
    fold_split <- splits[[i]]
    train_idx <- fold_split$train
    test_idx <- fold_split$test
    
    cat(sprintf("  Fold %d: train=%d, test=%d\n", i, length(train_idx), length(test_idx)))
    
    # Fit preprocessing on training data only
    spec <- fit_preprocessing(ds$X_raw[train_idx, , drop = FALSE])
    X_imp <- apply_preprocessing(ds$X_raw, spec)
    
    # Scaling for LSTM
    needs_scaling <- isTRUE(REGRESSION_MODELS[[model_key]]$needs_scaling)
    scaler <- if (needs_scaling) fit_scaler(X_imp[train_idx, , drop = FALSE]) else NULL
    
    mk_scaled <- function(idx) {
      M <- X_imp[idx, , drop = FALSE]
      if (!is.null(scaler)) apply_scaler(M, scaler) else M
    }
    
    X_train <- mk_scaled(train_idx)
    X_test <- mk_scaled(test_idx)
    y_train <- ds$y[train_idx]
    y_test <- ds$y[test_idx]
    dates_train <- ds$dates[train_idx]
    dates_test <- ds$dates[test_idx]
    
    # Train model
    model_result <- tryCatch({
      if (model_key == "zero_return") {
        handle <- train_zero_return_baseline()
      } else if (model_key == "historical_mean") {
        handle <- train_historical_mean_baseline(y_train, use_expanding = FALSE)
      } else if (model_key == "random_forest") {
        handle <- train_random_forest_regression(X_train, y_train, 
                                                 REGRESSION_MODELS[[model_key]]$defaults)
      } else if (model_key == "xgboost") {
        handle <- train_xgboost_regression(X_train, y_train,
                                           REGRESSION_MODELS[[model_key]]$defaults)
      } else if (model_key == "lstm") {
        handle <- train_lstm_regression(X_train, y_train,
                                        REGRESSION_MODELS[[model_key]]$defaults)
      } else {
        stop(sprintf("Unknown model key: %s", model_key))
      }
      list(success = TRUE, handle = handle)
    }, error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    })
    
    if (!model_result$success) {
      folds[[i]] <- list(
        fold = i,
        success = FALSE,
        error = model_result$error,
        n_train = length(train_idx),
        n_test = length(test_idx)
      )
      next
    }
    
    handle <- model_result$handle
    
    # Get predictions on BOTH train and test
    train_preds <- tryCatch(handle$predict(X_train), error = function(e) rep(NA_real_, length(train_idx)))
    test_preds <- tryCatch(handle$predict(X_test), error = function(e) rep(NA_real_, length(test_idx)))
    
    # === LEAKAGE-FREE THRESHOLD SELECTION ===
    # Compute threshold from TRAINING predictions only
    threshold <- compute_train_threshold(
      train_predictions = train_preds,
      train_actuals = y_train,
      method = config$threshold_method,
      percentile = config$train_percentile,
      fixed_value = config$fixed_threshold
    )
    
    all_thresholds[[i]] <- threshold
    
    # Generate signals using TRAINING-derived threshold
    signals <- apply_threshold(test_preds, threshold)
    
    # Compute forecast metrics
    fm <- forecast_metrics(test_preds, y_test)
    
    # Store fold results
    folds[[i]] <- list(
      fold = i,
      success = TRUE,
      n_train = length(train_idx),
      n_test = length(test_idx),
      dates_train = as.character(range(dates_train)),
      dates_test = as.character(range(dates_test)),
      train_dates = as.character(dates_train),
      test_dates = as.character(dates_test),
      predictions = as.numeric(test_preds),
      actuals = as.numeric(y_test),
      signals = signals,
      threshold = threshold,
      metrics = fm
    )
    
    # Aggregate
    all_preds <- c(all_preds, test_preds)
    all_actuals <- c(all_actuals, y_test)
    all_dates <- c(all_dates, as.character(dates_test))
    all_fold_ids <- c(all_fold_ids, rep(i, length(test_idx)))
  }
  
  # Aggregate metrics
  aggregate_metrics <- forecast_metrics(all_preds, all_actuals)
  
  list(
    model_key = model_key,
    model_label = REGRESSION_MODELS[[model_key]]$label,
    folds = folds,
    aggregate_metrics = aggregate_metrics,
    all_predictions = data.frame(
      date = all_dates,
      fold = all_fold_ids,
      predicted = all_preds,
      actual = all_actuals,
      stringsAsFactors = FALSE
    ),
    threshold_provenance = all_thresholds,
    config = config
  )
}

# =============================================================================
# BLOCK BOOTSTRAP FOR STATISTICAL UNCERTAINTY
# =============================================================================

#' Block bootstrap for time-series performance metrics.
#'
#' Preserves temporal dependence by resampling blocks of consecutive observations.
#'
#' @param predictions Vector of predictions.
#' @param actuals Vector of actual values.
#' @param n_samples Number of bootstrap samples (default 1000).
#' @param block_length Length of blocks (default 20).
#' @param seed Random seed.
#' @return list with CI for MAE, RMSE, IC, directional accuracy.
block_bootstrap_metrics <- function(predictions, actuals, 
                                    n_samples = 1000, 
                                    block_length = 20,
                                    seed = 42) {
  
  set.seed(seed)
  
  predictions <- as.numeric(predictions)
  actuals <- as.numeric(actuals)
  
  # Remove NA pairs
  ok <- !is.na(predictions) & !is.na(actuals)
  pred <- predictions[ok]
  act <- actuals[ok]
  n <- length(pred)
  
  if (n < block_length * 2) {
    warning("Insufficient data for block bootstrap")
    return(list(
      mae_ci = c(NA, NA),
      rmse_ci = c(NA, NA),
      ic_ci = c(NA, NA),
      dir_acc_ci = c(NA, NA),
      n_samples = 0,
      error = "insufficient_data"
    ))
  }
  
  # Compute number of blocks
  n_blocks <- ceiling(n / block_length)
  
  # Bootstrap
  mae_boot <- numeric(n_samples)
  rmse_boot <- numeric(n_samples)
  ic_boot <- numeric(n_samples)
  dir_acc_boot <- numeric(n_samples)
  
  for (b in seq_len(n_samples)) {
    # Sample block starting positions with replacement
    block_starts <- sample(1:(n - block_length + 1), n_blocks, replace = TRUE)
    
    # Construct bootstrap sample
    indices <- unlist(lapply(block_starts, function(s) s:(s + block_length - 1)))
    indices <- indices[indices <= n]  # Trim to length n
    
    pred_b <- pred[indices]
    act_b <- act[indices]
    
    # Compute metrics
    mae_boot[b] <- mean(abs(pred_b - act_b))
    rmse_boot[b] <- sqrt(mean((pred_b - act_b)^2))
    
    # IC (handle constant predictions)
    if (var(pred_b) > .Machine$double.eps && var(act_b) > .Machine$double.eps) {
      ic_boot[b] <- cor(pred_b, act_b, method = "pearson")
    } else {
      ic_boot[b] <- NA
    }
    
    # Directional accuracy
    correct <- sign(pred_b) == sign(act_b)
    correct[act_b == 0] <- TRUE
    dir_acc_boot[b] <- mean(correct)
  }
  
  # Compute 95% confidence intervals
  list(
    mae_ci = quantile(mae_boot, probs = c(0.025, 0.975), na.rm = TRUE),
    rmse_ci = quantile(rmse_boot, probs = c(0.025, 0.975), na.rm = TRUE),
    ic_ci = quantile(ic_boot, probs = c(0.025, 0.975), na.rm = TRUE),
    dir_acc_ci = quantile(dir_acc_boot, probs = c(0.025, 0.975), na.rm = TRUE),
    mae_mean = mean(mae_boot),
    rmse_mean = mean(rmse_boot),
    ic_mean = mean(ic_boot, na.rm = TRUE),
    dir_acc_mean = mean(dir_acc_boot),
    n_samples = n_samples,
    block_length = block_length,
    n_obs = n
  )
}

# =============================================================================
# TRADING EVALUATION WITH FIXED THRESHOLD
# =============================================================================

#' Evaluate trading performance with pre-specified threshold.
#'
#' @param ohlcv OHLCV data.
#' @param predictions_df Data frame with date, predicted, actual columns.
#' @param threshold Threshold from compute_train_threshold().
#' @param initial_capital Starting capital.
#' @param ticker Ticker symbol.
#' @return list with backtest results and metrics.
evaluate_trading_leakage_free <- function(ohlcv, predictions_df, threshold,
                                          initial_capital = 10000, ticker = "UNKNOWN") {
  
  ml_dir <- .ML_DIR
  analytics_dir <- dirname(ml_dir)
  source(file.path(analytics_dir, "metrics.R"), local = TRUE)
  source(file.path(analytics_dir, "execution.R"), local = TRUE)
  
  # Align predictions to OHLCV
  ohlcv_dates <- as.Date(zoo::index(ohlcv))
  pred_dates <- as.Date(predictions_df$date)
  
  overlap_idx <- match(pred_dates, ohlcv_dates)
  valid <- !is.na(overlap_idx)
  
  if (sum(valid) < 5) {
    return(list(
      success = FALSE,
      error = "Fewer than 5 overlapping dates",
      n_trades = 0
    ))
  }
  
  pred_valid <- predictions_df$predicted[valid]
  dates_valid <- pred_dates[valid]
  
  # Generate signals using PRE-COMPUTED threshold (from training data)
  signals_char <- apply_threshold(pred_valid, threshold)
  
  n_buy <- sum(signals_char == "BUY", na.rm = TRUE)
  n_sell <- sum(signals_char == "SELL", na.rm = TRUE)
  
  # Create signal xts
  min_date <- min(dates_valid)
  max_date <- max(dates_valid)
  ohlcv_in_range <- ohlcv[ohlcv_dates >= min_date & ohlcv_dates <= max_date, ]
  ohlcv_range_dates <- as.Date(zoo::index(ohlcv_in_range))
  
  signal_vec <- rep(NA_character_, length(ohlcv_range_dates))
  signal_idx <- match(dates_valid, ohlcv_range_dates)
  signal_vec[signal_idx[!is.na(signal_idx)]] <- signals_char[!is.na(signal_idx)]
  
  signal_xts <- xts::xts(signal_vec, order.by = ohlcv_range_dates)
  
  # Run backtest
  bt_result <- tryCatch({
    run_backtest(
      ohlcv = ohlcv_in_range,
      signals = signal_xts,
      initial_capital = initial_capital,
      start_date = min(ohlcv_range_dates),
      end_date = max(ohlcv_range_dates),
      ticker = ticker,
      strategy = "Regression_Forecast_H2",
      parameters = list(threshold = threshold),
      transaction_cost = 0.001
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
  
  n_trades <- if (!is.null(bt_result$completed_trades)) nrow(bt_result$completed_trades) else 0
  
  list(
    success = TRUE,
    backtest_result = bt_result,
    signal_summary = list(
      n_buy = n_buy,
      n_sell = n_sell,
      threshold_used = threshold
    ),
    n_trades = n_trades
  )
}

# =============================================================================
# COMPLETE VALIDATED EXPERIMENT
# =============================================================================

#' Run complete leakage-free validated experiment for one ticker.
#'
#' @param ticker Ticker symbol.
#' @param config Configuration list.
#' @return list with all results, fold metadata, and statistical uncertainty.
run_validated_experiment <- function(ticker, config = PHASE_H2_CONFIG) {
  
  cat(sprintf("\n=== VALIDATED EXPERIMENT: %s ===\n", ticker))
  cat(sprintf("Method: %s | Threshold: %s\n", 
              config$threshold_method,
              if (config$threshold_method == "fixed") as.character(config$fixed_threshold) else "from_train"))
  
  set.seed(config$seed)
  
  ml_dir <- .ML_DIR
  source(file.path(ml_dir, "preprocessing.R"), local = TRUE)
  source(file.path(ml_dir, "features.R"), local = TRUE)
  source(file.path(ml_dir, "regression_target.R"), local = TRUE)
  
  # Fetch data
  source(file.path(dirname(ml_dir), "data_retrieval.R"), local = TRUE)
  
  end_date <- if (is.null(config$end_date)) Sys.Date() - 30 else config$end_date
  
  ohlcv <- tryCatch({
    fetch_ohlcv(ticker, config$start_date, end_date)
  }, error = function(e) {
    stop(sprintf("Failed to fetch data for %s: %s", ticker, conditionMessage(e)))
  })
  
  cat(sprintf("Fetched %d rows from %s to %s\n",
              nrow(ohlcv), min(zoo::index(ohlcv)), max(zoo::index(ohlcv))))
  
  # Build features
  feats <- build_features(ohlcv, groups = NULL)
  targets <- generate_forward_return_target(as.numeric(ohlcv[, "Close"]), config$horizon)
  
  X_raw <- feats$features
  y <- targets$targets
  valid <- targets$valid & !is.na(y)
  
  non_na_frac <- 1 - rowSums(is.na(as.matrix(X_raw))) / ncol(X_raw)
  keep <- valid & (non_na_frac >= 0.7)
  
  X_raw <- X_raw[keep, ]
  y <- y[keep]
  dates <- as.Date(zoo::index(X_raw))
  
  ds <- list(
    ohlcv = ohlcv,
    ticker = ticker,
    features_xts = feats$features,
    dates = dates,
    X_raw = as.matrix(X_raw),
    y = y,
    n = nrow(X_raw),
    p = ncol(X_raw),
    feature_names = feats$feature_names,
    horizon = config$horizon
  )
  
  cat(sprintf("Usable rows: %d | Features: %d\n", ds$n, ds$p))
  
  # Check LSTM availability
  lstm_available <- tryCatch({
    requireNamespace("keras", quietly = TRUE) && keras::is_keras_available()
  }, error = function(e) FALSE)
  
  models_to_run <- c("zero_return", "historical_mean", "random_forest", "xgboost")
  if (lstm_available) {
    models_to_run <- c(models_to_run, "lstm")
    cat("LSTM: Available\n")
  } else {
    cat("LSTM: Not available (skipping)\n")
  }
  
  # Evaluate each model
  results <- list()
  
  for (model_key in models_to_run) {
    cat(sprintf("\n[%s] Evaluating...\n", model_key))
    
    result <- tryCatch({
      evaluate_model_leakage_free(ds, model_key, config)
    }, error = function(e) {
      cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
      list(model_key = model_key, error = conditionMessage(e), success = FALSE)
    })
    
    results[[model_key]] <- result
  }
  
  # Build forecast comparison
  forecast_rows <- lapply(names(results), function(key) {
    r <- results[[key]]
    if (!is.null(r$error)) {
      data.frame(Model = key, MAE = NA, RMSE = NA, IC = NA, DirAcc = NA, N_Obs = NA,
                 stringsAsFactors = FALSE)
    } else {
      m <- r$aggregate_metrics
      data.frame(Model = key, MAE = m$mae, RMSE = m$rmse, IC = m$ic, 
                 DirAcc = m$directional_accuracy, N_Obs = m$n_obs,
                 stringsAsFactors = FALSE)
    }
  })
  
  forecast_comparison <- do.call(rbind, forecast_rows)
  forecast_comparison <- forecast_comparison[order(forecast_comparison$RMSE), ]
  
  cat("\n=== FORECAST METRICS (LEAKAGE-FREE) ===\n")
  print(forecast_comparison)
  
  # Compute statistical uncertainty for best model
  best_model <- forecast_comparison$Model[1]
  if (!is.na(best_model) && is.null(results[[best_model]]$error)) {
    cat(sprintf("\nComputing bootstrap uncertainty for %s...\n", best_model))
    
    best_preds <- results[[best_model]]$all_predictions
    bootstrap_result <- block_bootstrap_metrics(
      best_preds$predicted,
      best_preds$actual,
      n_samples = config$bootstrap_samples,
      block_length = config$block_length,
      seed = config$seed
    )
    
    cat(sprintf("  MAE: %.4f [%.4f, %.4f]\n", 
                bootstrap_result$mae_mean,
                bootstrap_result$mae_ci[1],
                bootstrap_result$mae_ci[2]))
    cat(sprintf("  RMSE: %.4f [%.4f, %.4f]\n",
                bootstrap_result$rmse_mean,
                bootstrap_result$rmse_ci[1],
                bootstrap_result$rmse_ci[2]))
    cat(sprintf("  IC: %.4f [%.4f, %.4f]\n",
                bootstrap_result$ic_mean,
                bootstrap_result$ic_ci[1],
                bootstrap_result$ic_ci[2]))
  } else {
    bootstrap_result <- NULL
  }
  
  # Save results
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(config$output_dir, 
                           sprintf("validated_experiment_%s_%s.rds", ticker, timestamp))
  
  final_result <- list(
    config = config,
    ticker = ticker,
    forecast_results = results,
    forecast_comparison = forecast_comparison,
    bootstrap_uncertainty = bootstrap_result,
    timestamp = Sys.time()
  )
  
  saveRDS(final_result, output_file)
  cat(sprintf("\nResults saved to: %s\n", output_file))
  
  invisible(final_result)
}

# =============================================================================
# MULTI-ASSET EXPERIMENT
# =============================================================================

#' Run validated experiment across multiple assets.
#'
#' @param config Configuration list.
#' @return list with results for all tickers.
run_multi_asset_experiment <- function(config = PHASE_H2_CONFIG) {
  
  cat("\n=== MULTI-ASSET VALIDATED EXPERIMENT ===\n")
  cat(sprintf("Tickers: %s\n", paste(config$tickers, collapse = ", ")))
  cat(sprintf("Threshold method: %s\n", config$threshold_method))
  
  results <- list()
  
  for (ticker in config$tickers) {
    tryCatch({
      results[[ticker]] <- run_validated_experiment(ticker, config)
    }, error = function(e) {
      cat(sprintf("\nERROR for %s: %s\n", ticker, conditionMessage(e)))
      results[[ticker]] <- list(error = conditionMessage(e), success = FALSE)
    })
  }
  
  # Aggregate results
  cat("\n=== AGGREGATE RESULTS ===\n")
  
  for (ticker in names(results)) {
    if (!is.null(results[[ticker]]$forecast_comparison)) {
      cat(sprintf("\n%s:\n", ticker))
      print(results[[ticker]]$forecast_comparison)
    }
  }
  
  # Save aggregate
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(config$output_dir, sprintf("multi_asset_results_%s.rds", timestamp))
  
  saveRDS(results, output_file)
  cat(sprintf("\nMulti-asset results saved to: %s\n", output_file))
  
  invisible(results)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

if (!interactive() && sys.nframe() == 0) {
  # Run single-asset experiment when executed directly
  result <- run_validated_experiment("AAPL", PHASE_H2_CONFIG)
}
