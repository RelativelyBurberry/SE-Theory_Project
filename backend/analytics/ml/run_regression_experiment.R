# AlphaForge Insight — LSTM vs Baselines Experiment Runner (Phase H)
#
# End-to-end experiment:
#   1. Five-day forward return regression
#   2. Zero-return and historical-mean baselines
#   3. LSTM, RF, XGBoost regression on identical test dates
#   4. Walk-forward evaluation with leakage-safe preprocessing
#   5. Forecast metrics + trading metrics
#   6. Results saved to JSON/RDS
#
# Usage:
#   source("backend/analytics/ml/run_regression_experiment.R")
#   result <- run_lstm_experiment(ticker = "AAPL", n_folds = 3)

library(xts)

# Set ML directory globally for all sourced files
.ML_DIR <- file.path(getwd(), "backend", "analytics", "ml")

#' Run the complete LSTM regression experiment.
#'
#' @param ticker Ticker symbol (default "AAPL").
#' @param start_date Start date for data (default "2020-01-01").
#' @param end_date End date for data (default Sys.Date() - 30).
#' @param horizon Forward return horizon in trading days (default 5).
#' @param n_folds Number of walk-forward folds (default 3).
#' @param test_frac Fraction of data per test fold (default 0.1).
#' @param seed Random seed (default 42).
#' @param output_dir Directory to save results (default "research/results").
#' @param skip_lstm Skip LSTM model if TensorFlow unavailable (default FALSE).
#' @return list(config, forecast_results, trading_results, comparison, errors).
run_lstm_experiment <- function(ticker = "AAPL",
                                start_date = "2020-01-01",
                                end_date = NULL,
                                horizon = 5,
                                n_folds = 3,
                                test_frac = 0.1,
                                seed = 42,
                                output_dir = "research/results",
                                skip_lstm = FALSE) {
  
  cat("=== LSTM REGRESSION EXPERIMENT ===\n")
  cat(sprintf("Ticker: %s | Horizon: %d days | Folds: %d\n\n", ticker, horizon, n_folds))
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Set ML directory globally
  ml_dir <- .ML_DIR
  if (!dir.exists(ml_dir)) {
    stop(sprintf("ML directory not found: %s", ml_dir))
  }
  
  # -------------------------------------------------------------------------
  # Step 1: Fetch Data
  # -------------------------------------------------------------------------
  cat("[Step 1/6] Fetching OHLCV data...\n")
  
  if (is.null(end_date)) {
    end_date <- Sys.Date() - 30  # Leave buffer for horizon
  }
  
  # Source data retrieval
  source(file.path(dirname(ml_dir), "data_retrieval.R"), local = TRUE)
  
  ohlcv <- tryCatch({
    fetch_ohlcv(ticker, start_date, end_date)
  }, error = function(e) {
    stop(sprintf("Failed to fetch data for %s: %s", ticker, conditionMessage(e)))
  })
  
  cat(sprintf("  Fetched %d rows from %s to %s\n\n", 
              nrow(ohlcv), 
              min(zoo::index(ohlcv)), 
              max(zoo::index(ohlcv))))
  
  # -------------------------------------------------------------------------
  # Step 2: Assemble Dataset
  # -------------------------------------------------------------------------
  cat("[Step 2/6] Assembling regression dataset...\n")
  
  # Source ML modules
  source(file.path(ml_dir, "preprocessing.R"), local = TRUE)
  source(file.path(ml_dir, "features.R"), local = TRUE)
  source(file.path(ml_dir, "regression_target.R"), local = TRUE)
  
  # Build features
  feats <- build_features(ohlcv, groups = NULL)
  
  # Generate targets
  close_prices <- as.numeric(ohlcv[, "Close"])
  targets <- generate_forward_return_target(close_prices, horizon)
  
  # Assemble dataset
  X_raw <- feats$features
  y <- targets$targets
  valid <- targets$valid & !is.na(y)
  
  # Drop warm-up rows
  non_na_frac <- 1 - rowSums(is.na(as.matrix(X_raw))) / ncol(X_raw)
  keep <- valid & (non_na_frac >= 0.7)
  
  X_raw <- X_raw[keep, ]
  y <- y[keep]
  dates <- as.Date(zoo::index(X_raw))
  
  # Split
  split <- chronological_split(nrow(X_raw), 
                               fractions = list(train = 0.6, validation = 0.2, test = 0.2),
                               embargo = horizon)
  
  ds <- list(
    ohlcv = ohlcv,
    ticker = ticker,
    features_xts = feats$features,
    dates = dates,
    X_raw = as.matrix(X_raw),
    y = y,
    split = split,
    n = nrow(X_raw),
    p = ncol(X_raw),
    feature_names = feats$feature_names,
    horizon = horizon
  )
  
  cat(sprintf("  Usable rows: %d | Features: %d\n\n", ds$n, ds$p))
  
  # -------------------------------------------------------------------------
  # Step 3: Check TensorFlow/Keras Availability
  # -------------------------------------------------------------------------
  cat("[Step 3/6] Checking TensorFlow/Keras availability...\n")
  
  lstm_available <- FALSE
  if (!skip_lstm) {
    lstm_available <- tryCatch({
      ok <- requireNamespace("keras", quietly = TRUE)
      if (!ok) {
        cat("  Keras R package not installed. Skipping LSTM.\n")
        FALSE
      } else {
        avail <- keras::is_keras_available()
        if (isTRUE(avail)) {
          cat("  TensorFlow/Keras runtime AVAILABLE.\n")
          TRUE
        } else {
          cat("  TensorFlow/Keras runtime NOT available.\n")
          cat("  Run keras::install_keras() to install.\n")
          FALSE
        }
      }
    }, error = function(e) {
      cat(sprintf("  Error checking Keras: %s\n", e$message))
      FALSE
    })
  } else {
    cat("  LSTM skipped by user request.\n")
  }
  cat("\n")
  
  # -------------------------------------------------------------------------
  # Step 4: Run Walk-Forward Forecast Evaluation
  # -------------------------------------------------------------------------
  cat("[Step 4/6] Running walk-forward forecast evaluation...\n")
  
  # Source regression modules
  source(file.path(ml_dir, "regression_models.R"), local = TRUE)
  source(file.path(ml_dir, "regression_evaluation.R"), local = TRUE)
  
  models_to_run <- c("zero_return", "historical_mean", "random_forest", "xgboost")
  if (lstm_available) {
    models_to_run <- c(models_to_run, "lstm")
  }
  
  forecast_result <- tryCatch({
    run_regression_experiment(
      ds = ds,
      models = models_to_run,
      n_folds = n_folds,
      test_frac = test_frac,
      seed = seed
    )
  }, error = function(e) {
    stop(sprintf("Forecast evaluation failed: %s", conditionMessage(e)))
  })
  
  cat("\n  Forecast Metrics Comparison:\n")
  print(forecast_result$comparison)
  cat("\n")
  
  # -------------------------------------------------------------------------
  # Step 5: Run Trading Evaluation
  # -------------------------------------------------------------------------
  cat("[Step 5/6] Running trading evaluation...\n")
  
  # Source trading module
  source(file.path(ml_dir, "regression_trading.R"), local = TRUE)
  
  # Extract predictions for each model
  prediction_results <- list()
  for (model_key in names(forecast_result$results)) {
    r <- forecast_result$results[[model_key]]
    if (isTRUE(r$aggregate_metrics$n_obs > 0)) {
      prediction_results[[model_key]] <- r$all_predictions
    }
  }
  
  trading_comparison <- tryCatch({
    compare_trading_performance(
      ohlcv = ohlcv,
      prediction_results = prediction_results,
      threshold = 0.02,
      initial_capital = 10000
    )
  }, error = function(e) {
    cat(sprintf("  Trading evaluation error: %s\n", conditionMessage(e)))
    data.frame(
      Model = names(prediction_results),
      Type = "ML",
      CAGR = NA_real_,
      Sharpe = NA_real_,
      MaxDD = NA_real_,
      WinRate = NA_real_,
      Trades = NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  
  cat("\n  Trading Metrics Comparison:\n")
  print(trading_comparison)
  cat("\n")
  
  # -------------------------------------------------------------------------
  # Step 6: Save Results
  # -------------------------------------------------------------------------
  cat("[Step 6/6] Saving results...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(output_dir, sprintf("lstm_experiment_%s_%s.rds", ticker, timestamp))
  
  final_result <- list(
    config = list(
      ticker = ticker,
      start_date = start_date,
      end_date = end_date,
      horizon = horizon,
      n_folds = n_folds,
      test_frac = test_frac,
      seed = seed,
      timestamp = Sys.time(),
      lstm_available = lstm_available,
      models_run = models_to_run,
      n_total_rows = ds$n,
      n_features = ds$p
    ),
    forecast_results = forecast_result,
    trading_comparison = trading_comparison,
    errors = list()
  )
  
  # Collect any model errors
  for (model_key in names(forecast_result$results)) {
    r <- forecast_result$results[[model_key]]
    failed_folds <- Filter(function(f) !isTRUE(f$success), r$folds)
    if (length(failed_folds) > 0) {
      final_result$errors[[model_key]] <- lapply(failed_folds, function(f) f$error)
    }
  }
  
  saveRDS(final_result, output_file)
  cat(sprintf("  Results saved to: %s\n\n", output_file))
  
  # -------------------------------------------------------------------------
  # Summary
  # -------------------------------------------------------------------------
  cat("=== EXPERIMENT COMPLETE ===\n")
  cat(sprintf("Ticker: %s | Models: %d | Folds: %d\n", ticker, length(models_to_run), n_folds))
  cat("\nForecast Metrics (sorted by RMSE):\n")
  print(forecast_result$comparison)
  cat("\nTrading Metrics:\n")
  print(trading_comparison)
  
  if (length(final_result$errors) > 0) {
    cat("\nErrors encountered:\n")
    for (model_name in names(final_result$errors)) {
      cat(sprintf("  %s: %d fold(s) failed\n", model_name, length(final_result$errors[[model_name]])))
    }
  }
  
  invisible(final_result)
}

#' Run a minimal smoke test to verify the pipeline works.
#'
#' Uses a small dataset and skips LSTM to quickly validate alignment.
#'
#' @return TRUE if smoke test passes, FALSE otherwise.
run_smoke_test <- function() {
  cat("=== REGRESSION SMOKE TEST ===\n\n")
  
  tryCatch({
    # Run with just baselines and RF (fast)
    result <- run_lstm_experiment(
      ticker = "AAPL",
      start_date = "2023-01-01",
      end_date = "2023-06-30",  # Small date range
      horizon = 5,
      n_folds = 2,
      test_frac = 0.1,
      seed = 42,
      skip_lstm = TRUE  # Skip LSTM for speed
    )
    
    # Verify results
    if (is.null(result$forecast_results) || 
        is.null(result$forecast_results$comparison) ||
        nrow(result$forecast_results$comparison) == 0) {
      cat("SMOKE TEST FAILED: No forecast results\n")
      return(FALSE)
    }
    
    # Check that we have predictions
    total_preds <- sum(sapply(result$forecast_results$results, function(r) {
      r$aggregate_metrics$n_obs
    }))
    
    if (total_preds == 0) {
      cat("SMOKE TEST FAILED: No predictions generated\n")
      return(FALSE)
    }
    
    cat("\n=== SMOKE TEST PASSED ===\n")
    return(TRUE)
    
  }, error = function(e) {
    cat(sprintf("\nSMOKE TEST FAILED: %s\n", conditionMessage(e)))
    return(FALSE)
  })
}

# Run smoke test if executed directly
if (!interactive() && sys.nframe() == 0) {
  success <- run_smoke_test()
  if (!success) {
    stop("Smoke test failed")
  }
}
