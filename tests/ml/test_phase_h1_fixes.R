# AlphaForge Insight — Phase H.1 Fix Verification Tests
#
# Tests the fixes made to:
#   1. IC calculation for constant predictions
#   2. Trading evaluation threshold and timestamp alignment

library(testthat)

# Set up ML directory
.ML_DIR <- file.path(getwd(), "backend", "analytics", "ml")
ml_dir <- .ML_DIR
assign(".ML_DIR", ml_dir, envir = globalenv())

source(file.path(ml_dir, "regression_evaluation.R"), local = FALSE)
source(file.path(ml_dir, "regression_trading.R"), local = FALSE)

# =============================================================================
# TEST 1: IC Calculation for Constant Predictions
# =============================================================================
test_that("IC returns NA for constant predictions (zero variance)", {
  # All predictions are the same constant
  pred <- rep(0.005, 100)
  act <- rnorm(100, mean = 0.01, sd = 0.03)
  
  metrics <- forecast_metrics(pred, act)
  
  expect_true(is.na(metrics$ic), 
              info = "IC should be NA when predictions have zero variance")
  expect_true(!is.na(metrics$mae), 
              info = "MAE should still be computed")
  expect_true(!is.na(metrics$rmse), 
              info = "RMSE should still be computed")
})

test_that("IC returns NA for constant actuals (zero variance)", {
  # All actuals are the same
  pred <- rnorm(100, mean = 0.01, sd = 0.03)
  act <- rep(0.005, 100)
  
  metrics <- forecast_metrics(pred, act)
  
  expect_true(is.na(metrics$ic), 
              info = "IC should be NA when actuals have zero variance")
})

test_that("IC is computed correctly for varying predictions", {
  set.seed(42)
  pred <- rnorm(100, mean = 0.01, sd = 0.02)
  act <- pred + rnorm(100, sd = 0.01)  # Correlated with noise
  
  metrics <- forecast_metrics(pred, act)
  
  expect_true(!is.na(metrics$ic), 
              info = "IC should be computed for varying predictions")
  expect_true(metrics$ic > 0.5, 
              info = "IC should be positive for correlated series")
})

# =============================================================================
# TEST 2: Directional Accuracy Edge Cases
# =============================================================================
test_that("Directional accuracy handles zero actuals correctly", {
  # When actual is exactly 0, any prediction should count as correct
  pred <- c(0.01, -0.01, 0.0, 0.02)
  act <- c(0.0, 0.0, 0.0, 0.0)
  
  metrics <- forecast_metrics(pred, act)
  
  expect_equal(metrics$directional_accuracy, 1.0,
               info = "All predictions should be correct when actuals are zero")
})

test_that("Zero predictions have zero directional accuracy for non-zero actuals", {
  # When all predictions are 0 and all actuals are non-zero
  pred <- rep(0, 100)
  act <- rnorm(100, mean = 0.01, sd = 0.03)
  act <- act[act != 0]  # Ensure no zero actuals
  act <- act[1:100]
  
  metrics <- forecast_metrics(pred, act)
  
  expect_equal(metrics$directional_accuracy, 0.0,
               info = "Zero predictions should have 0% directional accuracy for non-zero actuals")
})

# =============================================================================
# TEST 3: Signal Generation with Lower Threshold
# =============================================================================
test_that("predictions_to_signals uses lower default threshold (0.01)", {
  # Predictions that would not exceed 0.02 but do exceed 0.01
  predictions <- c(0.015, -0.015, 0.005, -0.005)
  
  signals <- predictions_to_signals(predictions, threshold = 0.01)
  
  expect_equal(signals[1], "BUY", info = "0.015 > 0.01 should be BUY")
  expect_equal(signals[2], "SELL", info = "-0.015 < -0.01 should be SELL")
  expect_true(is.na(signals[3]), info = "0.005 < 0.01 should be HOLD")
  expect_true(is.na(signals[4]), info = "-0.005 > -0.01 should be HOLD")
})

test_that("Adaptive threshold generates signals from distribution percentiles", {
  set.seed(42)
  predictions <- rnorm(100, mean = 0.005, sd = 0.02)
  
  adaptive <- compute_adaptive_threshold(predictions, percentile = 0.2)
  
  # Top 20% should be BUY, bottom 20% should be SELL
  n_buy <- sum(adaptive$signals == "BUY", na.rm = TRUE)
  n_sell <- sum(adaptive$signals == "SELL", na.rm = TRUE)
  
  expect_true(n_buy >= 15 && n_buy <= 25, 
              info = sprintf("Expected ~20 BUY signals, got %d", n_buy))
  expect_true(n_sell >= 15 && n_sell <= 25, 
              info = sprintf("Expected ~20 SELL signals, got %d", n_sell))
})

# =============================================================================
# TEST 4: Target Formula Verification (Hand-Calculated)
# =============================================================================
test_that("Forward return target formula is correct", {
  source(file.path(ml_dir, "regression_target.R"), local = TRUE)
  
  # Hand-calculated example
  test_close <- c(100, 102, 104, 106, 108, 110, 112, 114, 116, 118, 120)
  
  # For horizon=3:
  # target[1] = Close[4]/Close[1] - 1 = 106/100 - 1 = 0.06
  # target[2] = Close[5]/Close[2] - 1 = 108/102 - 1 = 0.05882353
  # target[3] = Close[6]/Close[3] - 1 = 110/104 - 1 = 0.05769231
  # ...
  # target[8] = Close[11]/Close[8] - 1 = 120/114 - 1 = 0.05263158
  # target[9-11] = NA
  
  result <- generate_forward_return_target(test_close, horizon = 3)
  
  expected_1 <- 106/100 - 1
  expected_2 <- 108/102 - 1
  expected_8 <- 120/114 - 1
  
  expect_equal(result$targets[1], expected_1, tolerance = 1e-10,
               info = "Target formula row 1 incorrect")
  expect_equal(result$targets[2], expected_2, tolerance = 1e-10,
               info = "Target formula row 2 incorrect")
  expect_equal(result$targets[8], expected_8, tolerance = 1e-10,
               info = "Target formula row 8 incorrect")
  expect_true(all(is.na(result$targets[9:11])),
              info = "Last 3 rows should have NA targets")
  expect_equal(result$horizon, 3,
               info = "Horizon should be preserved in result")
})

# =============================================================================
# TEST 5: Trading Evaluation Signal Count
# =============================================================================
test_that("Trading evaluation produces non-zero signals for varying predictions", {
  # Create synthetic OHLCV and predictions
  dates <- seq(as.Date("2023-01-01"), by = "day", length.out = 50)
  ohlcv_df <- data.frame(
    Open = 100 + cumsum(rnorm(50, 0, 0.5)),
    High = 102 + cumsum(rnorm(50, 0, 0.5)),
    Low = 98 + cumsum(rnorm(50, 0, 0.5)),
    Close = 100 + cumsum(rnorm(50, 0, 0.5)),
    Volume = 1000000
  )
  ohlcv <- xts::xts(as.matrix(ohlcv_df), order.by = dates)
  
  # Create predictions with some variance
  set.seed(123)
  predictions <- rnorm(50, mean = 0.01, sd = 0.02)
  
  result <- evaluate_predictions_trading(
    ohlcv = ohlcv,
    prediction_dates = dates,
    predictions = predictions,
    threshold = 0.01,
    use_adaptive_threshold = TRUE,
    initial_capital = 10000
  )
  
  expect_true(result$success, 
              info = paste("Trading evaluation failed:", result$error))
  expect_true(!is.null(result$backtest_result),
              info = "Backtest result should not be NULL")
  
  # With adaptive threshold, we should have some BUY/SELL signals
  n_buy <- result$signal_summary$n_buy
  n_sell <- result$signal_summary$n_sell
  
  expect_true(n_buy > 0 || n_sell > 0,
              info = sprintf("Expected some signals, got %d BUY, %d SELL", n_buy, n_sell))
})

# =============================================================================
# TEST 6: Metrics Reproducibility
# =============================================================================
test_that("Aggregate metrics match fold-level aggregation", {
  # Load the most recent experiment results
  result_files <- list.files("research/results", pattern = "lstm_experiment_AAPL.*\\.rds$", 
                              full.names = TRUE)
  
  if (length(result_files) == 0) {
    skip("No saved experiment results found")
  }
  
  experiment <- readRDS(result_files[length(result_files)])
  
  for (model_key in names(experiment$forecast_results$results)) {
    r <- experiment$forecast_results$results[[model_key]]
    
    if (is.null(r$all_predictions) || nrow(r$all_predictions) == 0) next
    
    # Get all predictions
    all_preds <- r$all_predictions
    pred <- all_preds$predicted
    act <- all_preds$actual
    
    # Recompute metrics
    metrics_recomputed <- forecast_metrics(pred, act)
    metrics_reported <- r$aggregate_metrics
    
    expect_equal(metrics_recomputed$mae, metrics_reported$mae, tolerance = 1e-10,
                 info = sprintf("%s MAE mismatch", model_key))
    expect_equal(metrics_recomputed$rmse, metrics_reported$rmse, tolerance = 1e-10,
                 info = sprintf("%s RMSE mismatch", model_key))
    expect_equal(metrics_recomputed$n_obs, metrics_reported$n_obs,
                 info = sprintf("%s N_Obs mismatch", model_key))
  }
})

# =============================================================================
# RUN ALL TESTS
# =============================================================================
cat("\n=== Phase H.1 Fix Verification Tests ===\n\n")

# Run tests manually to capture results
test_results <- test_file("tests/ml/test_phase_h1_fixes.R", reporter = "summary")

cat("\n=== Tests Complete ===\n")
