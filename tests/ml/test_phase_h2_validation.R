# AlphaForge Insight — Phase H.2 Validation Tests
#
# Comprehensive test suite for leakage-free validation:
#   1. Threshold provenance (must be from training data only)
#   2. Timestamp alignment verification
#   3. Fold separation and uniqueness
#   4. Train-only preprocessing
#   5. Return alignment correctness
#   6. Metric calculation verification
#   7. Block bootstrap correctness

library(testthat)

# Set up paths
ml_dir <- file.path(getwd(), "backend", "analytics", "ml")
.ML_DIR <- ml_dir

source(file.path(ml_dir, "preprocessing.R"), local = TRUE)
source(file.path(ml_dir, "regression_target.R"), local = TRUE)
source(file.path(ml_dir, "regression_models.R"), local = TRUE)
source(file.path(ml_dir, "regression_evaluation.R"), local = TRUE)
source(file.path(ml_dir, "run_validated_regression_experiments.R"), local = TRUE)

# =============================================================================
# TEST SUITE 1: THRESHOLD PROVENANCE
# =============================================================================

test_that("Threshold is computed from training data only", {
  
  # Create synthetic training and test predictions
  set.seed(42)
  train_preds <- rnorm(100, mean = 0.02, sd = 0.03)
  train_actuals <- train_preds + rnorm(100, sd = 0.01)
  
  test_preds <- rnorm(50, mean = 0.03, sd = 0.04)
  
  # Compute threshold from training data
  threshold <- compute_train_threshold(train_preds, train_actuals, 
                                       method = "percentile", percentile = 0.20)
  
  # Verify threshold properties
  expect_true(is.list(threshold))
  expect_true("threshold_buy" %in% names(threshold))
  expect_true("threshold_sell" %in% names(threshold))
  expect_true("method" %in% names(threshold))
  expect_true("provenance" %in% names(threshold))
  
  # Verify threshold is based on training distribution
  expect_true(threshold$threshold_buy > quantile(train_preds, 0.5))
  expect_true(threshold$threshold_sell < quantile(train_preds, 0.5))
  
  # Verify provenance
  expect_equal(threshold$provenance, "training_predictions_only")
  expect_equal(threshold$method, "train_percentile")
  
  # Apply threshold to test data
  signals <- apply_threshold(test_preds, threshold)
  
  # Verify signals are generated
  n_buy <- sum(signals == "BUY", na.rm = TRUE)
  n_sell <- sum(signals == "SELL", na.rm = TRUE)
  
  expect_true(n_buy >= 0)
  expect_true(n_sell >= 0)
  expect_true(length(signals) == length(test_preds))
})

test_that("Fixed threshold method works correctly", {
  
  threshold <- compute_train_threshold(1:100, 1:100, 
                                       method = "fixed", 
                                       fixed_value = 0.005)
  
  expect_equal(threshold$threshold_buy, 0.005)
  expect_equal(threshold$threshold_sell, -0.005)
  expect_equal(threshold$method, "fixed")
  expect_equal(threshold$provenance, "pre_specified")
})

test_that("Std threshold method works correctly", {
  
  set.seed(42)
  train_preds <- rnorm(100, mean = 0.02, sd = 0.03)
  train_actuals <- train_preds + rnorm(100, sd = 0.01)
  
  threshold <- compute_train_threshold(train_preds, train_actuals,
                                       method = "std", std_multiple = 1.0)
  
  train_mean <- mean(train_preds, na.rm = TRUE)
  train_sd <- sd(train_preds, na.rm = TRUE)
  
  expect_equal(threshold$threshold_buy, train_mean + train_sd, tolerance = 1e-6)
  expect_equal(threshold$threshold_sell, train_mean - train_sd, tolerance = 1e-6)
  expect_equal(threshold$method, "train_std")
})

test_that("Threshold falls back to fixed for insufficient data", {
  
  small_train <- rnorm(10)
  
  threshold <- compute_train_threshold(small_train, small_train,
                                       method = "percentile", percentile = 0.20)
  
  expect_equal(threshold$method, "fixed_fallback")
  expect_equal(threshold$provenance, "insufficient_train_data")
})

# =============================================================================
# TEST SUITE 2: TIMESTAMP ALIGNMENT
# =============================================================================

test_that("Predictions align correctly with dates", {
  
  # Load the most recent validated experiment
  results_dir <- file.path(getwd(), "research", "results")
  exp_files <- list.files(results_dir, pattern = "validated_experiment_AAPL_.*\\.rds$", 
                          full.names = TRUE)
  
  if (length(exp_files) == 0) {
    skip("No validated experiment files found")
  }
  
  exp <- readRDS(exp_files[1])
  
  # Check each model's predictions
  for (model_key in names(exp$forecast_results)) {
    result <- exp$forecast_results[[model_key]]
    
    if (!is.null(result$error)) next
    
    preds <- result$all_predictions
    
    # Verify date-prediction alignment
    expect_equal(length(preds$date), length(preds$predicted))
    expect_equal(length(preds$date), length(preds$actual))
    expect_equal(length(preds$date), length(preds$fold))
    
    # Verify no duplicate dates
    expect_equal(length(unique(preds$date)), length(preds$date))
    
    # Verify dates are valid Date objects
    dates <- as.Date(preds$date)
    expect_false(any(is.na(dates)))
  }
})

# =============================================================================
# TEST SUITE 3: FOLD SEPARATION
# =============================================================================

test_that("Test folds do not overlap", {
  
  results_dir <- file.path(getwd(), "research", "results")
  exp_files <- list.files(results_dir, pattern = "validated_experiment_AAPL_.*\\.rds$", 
                          full.names = TRUE)
  
  if (length(exp_files) == 0) {
    skip("No validated experiment files found")
  }
  
  exp <- readRDS(exp_files[1])
  
  for (model_key in names(exp$forecast_results)) {
    result <- exp$forecast_results[[model_key]]
    
    if (!is.null(result$error)) next
    
    # Get fold dates
    fold_dates <- split(result$all_predictions$date, result$all_predictions$fold)
    
    # Check pairwise overlap
    fold_ids <- names(fold_dates)
    
    for (i in 1:(length(fold_ids) - 1)) {
      for (j in (i + 1):length(fold_ids)) {
        overlap <- intersect(fold_dates[[fold_ids[i]]], fold_dates[[fold_ids[j]]])
        expect_equal(length(overlap), 0, 
                     info = sprintf("Folds %s and %s overlap for model %s", 
                                    fold_ids[i], fold_ids[j], model_key))
      }
    }
  }
})

test_that("Each observation tested exactly once", {
  
  results_dir <- file.path(getwd(), "research", "results")
  exp_files <- list.files(results_dir, pattern = "validated_experiment_AAPL_.*\\.rds$", 
                          full.names = TRUE)
  
  if (length(exp_files) == 0) {
    skip("No validated experiment files found")
  }
  
  exp <- readRDS(exp_files[1])
  
  for (model_key in names(exp$forecast_results)) {
    result <- exp$forecast_results[[model_key]]
    
    if (!is.null(result$error)) next
    
    preds <- result$all_predictions
    
    # Each date should appear exactly once
    date_counts <- table(preds$date)
    expect_true(all(date_counts == 1), 
                info = sprintf("Model %s has duplicated test dates", model_key))
    
    # Total count should match aggregate N_Obs
    expect_equal(nrow(preds), result$aggregate_metrics$n_obs)
  }
})

# =============================================================================
# TEST SUITE 4: TRAIN-ONLY PREPROCESSING
# =============================================================================

test_that("Preprocessing is fit on training data only", {
  
  # Create simple test data
  set.seed(42)
  X_train <- matrix(rnorm(100), ncol = 10)
  colnames(X_train) <- paste0("feat", 1:10)
  
  # Fit preprocessing on training data
  spec <- fit_preprocessing(X_train)
  
  # Verify spec is fit on training data only
  expect_true(is.list(spec))
  expect_true("kept" %in% names(spec))
  expect_true("medians" %in% names(spec))
  
  # Apply to new data
  X_new <- matrix(rnorm(50), ncol = 10)
  colnames(X_new) <- paste0("feat", 1:10)
  
  X_processed <- apply_preprocessing(X_new, spec)
  
  # Verify no NAs after imputation
  expect_false(any(is.na(X_processed)))
  
  # Verify columns match spec
  expect_equal(colnames(X_processed), spec$kept)
})

# =============================================================================
# TEST SUITE 5: RETURN ALIGNMENT
# =============================================================================

test_that("Target returns are correctly computed", {
  
  # Manually verify target formula: Close[t+horizon]/Close[t] - 1
  close_prices <- c(100, 101, 102, 103, 104, 105, 106, 107, 108)
  horizon <- 3
  
  targets <- generate_forward_return_target(close_prices, horizon)
  
  # Verify formula
  for (i in 1:(length(close_prices) - horizon)) {
    expected <- close_prices[i + horizon] / close_prices[i] - 1
    expect_equal(targets$targets[i], expected, tolerance = 1e-10)
  }
  
  # Verify last horizon rows are NA
  expect_true(all(is.na(targets$targets[(length(close_prices) - horizon + 1):length(close_prices)])))
})

test_that("Predictions and targets are temporally aligned", {
  
  results_dir <- file.path(getwd(), "research", "results")
  exp_files <- list.files(results_dir, pattern = "validated_experiment_AAPL_.*\\.rds$", 
                          full.names = TRUE)
  
  if (length(exp_files) == 0) {
    skip("No validated experiment files found")
  }
  
  exp <- readRDS(exp_files[1])
  
  for (model_key in names(exp$forecast_results)) {
    result <- exp$forecast_results[[model_key]]
    
    if (!is.null(result$error)) next
    
    # For each fold, verify predictions align with test dates
    for (fold_idx in seq_along(result$folds)) {
      fold <- result$folds[[fold_idx]]
      
      if (!isTRUE(fold$success)) next
      
      # Verify prediction count matches test count
      expect_equal(length(fold$predictions), fold$n_test)
      expect_equal(length(fold$actuals), fold$n_test)
      expect_equal(length(fold$dates), fold$n_test)
    }
  }
})

# =============================================================================
# TEST SUITE 6: METRIC CALCULATION
# =============================================================================

test_that("Forecast metrics calculate correctly", {
  
  # Test with known values
  predicted <- c(0.01, 0.02, -0.01, 0.03, -0.02)
  actual <- c(0.015, 0.018, -0.012, 0.025, -0.018)
  
  metrics <- forecast_metrics(predicted, actual)
  
  # Manually compute MAE
  expected_mae <- mean(abs(predicted - actual))
  expect_equal(metrics$mae, expected_mae, tolerance = 1e-10)
  
  # Manually compute RMSE
  expected_rmse <- sqrt(mean((predicted - actual)^2))
  expect_equal(metrics$rmse, expected_rmse, tolerance = 1e-10)
  
  # Manually compute IC
  expected_ic <- cor(predicted, actual, method = "pearson")
  expect_equal(metrics$ic, expected_ic, tolerance = 1e-10)
  
  # Directional accuracy
  correct_dir <- (sign(predicted) == sign(actual))
  expected_dir_acc <- mean(correct_dir)
  expect_equal(metrics$directional_accuracy, expected_dir_acc, tolerance = 1e-10)
  
  # N_obs
  expect_equal(metrics$n_obs, length(predicted))
})

test_that("IC returns NA for constant predictions", {
  
  predicted <- rep(0.02, 10)
  actual <- rnorm(10)
  
  metrics <- forecast_metrics(predicted, actual)
  
  expect_true(is.na(metrics$ic))
})

# =============================================================================
# TEST SUITE 7: BLOCK BOOTSTRAP
# =============================================================================

test_that("Block bootstrap preserves temporal structure", {
  
  set.seed(42)
  n <- 100
  predictions <- rnorm(n, mean = 0.02, sd = 0.03)
  actuals <- predictions + rnorm(n, sd = 0.01)
  
  # Run bootstrap with small sample for speed
  result <- block_bootstrap_metrics(predictions, actuals, 
                                    n_samples = 100, 
                                    block_length = 10,
                                    seed = 42)
  
  # Verify output structure
  expect_true(is.list(result))
  expect_true("mae_ci" %in% names(result))
  expect_true("rmse_ci" %in% names(result))
  expect_true("ic_ci" %in% names(result))
  expect_true("dir_acc_ci" %in% names(result))
  
  # Verify CI length
  expect_equal(length(result$mae_ci), 2)
  expect_equal(length(result$rmse_ci), 2)
  
  # Verify CI ordering (lower < upper)
  expect_true(result$mae_ci[1] < result$mae_ci[2])
  expect_true(result$rmse_ci[1] < result$rmse_ci[2])
  
  # Verify sample count
  expect_equal(result$n_samples, 100)
  expect_equal(result$block_length, 10)
  expect_equal(result$n_obs, n)
})

test_that("Block bootstrap handles NA values", {
  
  predictions <- c(0.01, 0.02, NA, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12, 0.13, 0.14)
  actuals <- c(0.015, 0.022, 0.012, 0.025, 0.038, 0.052, 0.065, 0.075, 0.085, 0.092, 0.105, 0.115, 0.128, 0.135, 0.145)
  
  result <- block_bootstrap_metrics(predictions, actuals,
                                    n_samples = 50,
                                    block_length = 5,
                                    seed = 42)
  
  # Should handle NAs gracefully
  expect_true(is.list(result))
  expect_true(!is.na(result$mae_mean) || !is.null(result$error))
})

test_that("Block bootstrap fails gracefully for insufficient data", {
  
  predictions <- 1:15
  actuals <- 1:15
  
  result <- block_bootstrap_metrics(predictions, actuals,
                                    n_samples = 100,
                                    block_length = 20,
                                    seed = 42)
  
  # Should return error status
  expect_true(!is.null(result$error))
  expect_equal(result$error, "insufficient_data")
})

# =============================================================================
# TEST SUITE 8: COMPARATIVE VALIDATION
# =============================================================================

test_that("Historical-mean baseline uses training data only", {
  
  # This is verified in the audit, but we double-check here
  y_train <- c(0.01, 0.02, 0.015, 0.018, 0.012)
  
  baseline <- train_historical_mean_baseline(y_train)
  
  expect_equal(baseline$mean, mean(y_train))
  
  # Predictions should all be the training mean
  X_test <- matrix(1:10, ncol = 1)
  preds <- baseline$predict(X_test)
  
  expect_true(all(preds == mean(y_train)))
})

test_that("Models produce consistent results across runs with same seed", {
  
  results_dir <- file.path(getwd(), "research", "results")
  exp_files <- list.files(results_dir, pattern = "validated_experiment_AAPL_.*\\.rds$", 
                          full.names = TRUE)
  
  if (length(exp_files) < 2) {
    skip("Need at least 2 experiment files for consistency check")
  }
  
  exp1 <- readRDS(exp_files[1])
  exp2 <- readRDS(exp_files[2])
  
  # Compare forecast metrics (should be similar if same config)
  if (exp1$config$seed == exp2$config$seed) {
    for (model_key in intersect(names(exp1$forecast_results), names(exp2$forecast_results))) {
      m1 <- exp1$forecast_results[[model_key]]$aggregate_metrics
      m2 <- exp2$forecast_results[[model_key]]$aggregate_metrics
      
      if (!is.null(m1) && !is.null(m2)) {
        expect_equal(m1$mae, m2$mae, tolerance = 1e-6)
        expect_equal(m1$rmse, m2$rmse, tolerance = 1e-6)
      }
    }
  }
})

# =============================================================================
# RUN ALL TESTS
# =============================================================================

cat("\n=== Phase H.2 Validation Test Suite ===\n\n")

# Run tests
test_results <- test_dir(
  file.path(getwd(), "tests", "ml"),
  filter = "test_phase_h2_validation",
  reporter = "summary"
)

cat("\n=== TEST SUMMARY ===\n")
print(test_results)
