# AlphaForge Insight — Tests for Python Time-Series Integration
#
# Run with: testthat::test_file("test_python_ts.R")
# Or: source("test_python_ts.R")

library(testthat)
library(httr)
library(jsonlite)

source("analytics/ml/python_ts.R")

context("Python Time-Series Integration")

test_that("check_python_ts_health returns NULL when service unavailable", {
  result <- check_python_ts_health()
  expect_true(is.null(result) || is.list(result))
})

test_that("get_python_ts_models returns NULL when service unavailable", {
  result <- get_python_ts_models()
  expect_true(is.null(result) || is.list(result))
})

test_that("TS_MODELS has 12 models registered", {
  expect_equal(length(TS_MODELS), 12)
  expect_true("zero_return" %in% TS_MODEL_KEYS)
  expect_true("historical_mean" %in% TS_MODEL_KEYS)
  expect_true("linear_regression" %in% TS_MODEL_KEYS)
  expect_true("arima" %in% TS_MODEL_KEYS)
  expect_true("garch" %in% TS_MODEL_KEYS)
  expect_true("random_forest" %in% TS_MODEL_KEYS)
  expect_true("xgboost" %in% TS_MODEL_KEYS)
  expect_true("lstm" %in% TS_MODEL_KEYS)
  expect_true("gru" %in% TS_MODEL_KEYS)
})

test_that("TS_MODELS have correct metadata", {
  for (key in TS_MODEL_KEYS) {
    model <- TS_MODELS[[key]]
    expect_true(!is.null(model$label), info = sprintf("%s missing label", key))
    expect_true(!is.null(model$family), info = sprintf("%s missing family", key))
    expect_true(!is.null(model$is_regression), info = sprintf("%s missing is_regression", key))
    expect_true(!is.null(model$is_volatility), info = sprintf("%s missing is_volatility", key))
  }
})

test_that("GARCH is marked as volatility model", {
  expect_true(TS_MODELS$garch$is_volatility)
  expect_false(TS_MODELS$garch$is_regression)
})

test_that("ts_predictions_to_records converts predictions correctly", {
  predictions <- c(0.03, 0.01, -0.01, -0.03, 0.0)
  dates <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05"))
  ticker <- "AAPL"
  
  recs <- ts_predictions_to_records(predictions, dates, ticker)
  
  expect_equal(nrow(recs), 5)
  expect_true(all(recs$ticker == ticker))
  expect_equal(recs$predicted_class, c("BUY", "BUY", "HOLD", "SELL", "HOLD"))
  expect_true(all(recs$prob_buy >= 0 & recs$prob_buy <= 1))
  expect_true(all(recs$prob_hold >= 0 & recs$prob_hold <= 1))
  expect_true(all(recs$prob_sell >= 0 & recs$prob_sell <= 1))
})

test_that("ts_predictions_to_records handles empty input", {
  recs <- ts_predictions_to_records(numeric(0), as.Date(character(0)), "AAPL")
  expect_equal(nrow(recs), 0)
})

test_that("ts_predictions_to_records respects thresholds", {
  predictions <- c(0.05, 0.01, 0.0, -0.01, -0.05)
  dates <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05"))
  
  recs_default <- ts_predictions_to_records(predictions, dates, "AAPL")
  recs_tight <- ts_predictions_to_records(predictions, dates, "AAPL", 
                                          buy_threshold = 0.01, 
                                          sell_threshold = 0.01)
  
  expect_equal(recs_default$predicted_class, c("BUY", "HOLD", "HOLD", "HOLD", "SELL"))
  expect_equal(recs_tight$predicted_class, c("BUY", "BUY", "HOLD", "SELL", "SELL"))
})

test_that("evaluate_ts_predictions computes regression metrics", {
  set.seed(42)
  n <- 20
  predictions <- rnorm(n, 0, 0.02)
  y_test <- predictions + rnorm(n, 0, 0.01)
  
  dates_test <- as.Date("2023-01-01") + 0:(n-1)
  ticker <- "TEST"
  
  ohlcv_test <- xts::xts(
    matrix(rnorm(n * 5, 100, 1), ncol = 5),
    order.by = dates_test,
    dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))
  )
  
  result <- evaluate_ts_predictions(
    predictions = predictions,
    y_test = y_test,
    ohlcv_test = ohlcv_test,
    dates_test = dates_test,
    ticker = ticker,
    initial_capital = 10000
  )
  
  expect_true(is.list(result))
  expect_true("regression" %in% names(result))
  expect_true("backtest" %in% names(result))
  
  reg <- result$regression
  expect_true("rmse" %in% names(reg))
  expect_true("mae" %in% names(reg))
  expect_true("r2" %in% names(reg))
  expect_true("ic" %in% names(reg))
  expect_true("directional_accuracy" %in% names(reg))
  
  expect_true(is.numeric(reg$rmse))
  expect_true(reg$rmse > 0)
})

test_that("evaluate_ts_predictions handles constant predictions", {
  n <- 20
  predictions <- rep(0, n)
  y_test <- rnorm(n, 0, 0.02)
  
  dates_test <- as.Date("2023-01-01") + 0:(n-1)
  
  ohlcv_test <- xts::xts(
    matrix(rnorm(n * 5, 100, 1), ncol = 5),
    order.by = dates_test,
    dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))
  )
  
  result <- evaluate_ts_predictions(
    predictions = predictions,
    y_test = y_test,
    ohlcv_test = ohlcv_test,
    dates_test = dates_test,
    ticker = "TEST"
  )
  
  expect_true(is.list(result))
})

test_that("is_ts_model correctly identifies Python TS models", {
  expect_true(is_ts_model("zero_return"))
  expect_true(is_ts_model("lstm"))
  expect_true(is_ts_model("garch"))
  expect_true(is_ts_model("ts_zero_return"))
  expect_true(is_ts_model("ts_lstm"))
})

test_that("execute_ts_models handles unavailable service gracefully", {
  skip_if_not_installed("httr")
  
  n <- 50
  X_raw <- matrix(rnorm(n * 6), ncol = 6)
  colnames(X_raw) <- c("f1", "f2", "f3", "f4", "f5", "f6")
  dates <- as.Date("2023-01-01") + 0:(n-1)
  split <- list(
    train = 1:30,
    validation = 31:40,
    test = 41:50
  )
  
  result <- tryCatch({
    execute_ts_models(
      X_raw = X_raw,
      y = NULL,
      dates = dates,
      split = split,
      models = c("zero_return", "historical_mean"),
      hyperparams = list(),
      horizon = 5
    )
  }, error = function(e) {
    list(
      zero_return = list(success = FALSE, error = conditionMessage(e)),
      historical_mean = list(success = FALSE, error = conditionMessage(e))
    )
  })
  
  expect_true(is.list(result))
})

message("Python time-series integration tests completed")
