# Debug trading execution
.ML_DIR <- file.path(getwd(), "backend", "analytics", "ml")
ml_dir <- .ML_DIR
assign(".ML_DIR", ml_dir, envir = globalenv())

source(file.path(ml_dir, "regression_trading.R"), local = FALSE)

# Load the latest experiment
result_files <- list.files("research/results", pattern = "lstm_experiment_AAPL.*\\.rds$", full.names = TRUE)
experiment <- readRDS(result_files[length(result_files)])

# Check RF predictions
rf_pred <- experiment$forecast_results$results$random_forest$all_predictions
cat("RF predictions sample:\n")
print(head(rf_pred, 20))

# Count signals
signals <- predictions_to_signals(rf_pred$predicted, threshold = 0.01)
cat(sprintf("\nSignals with threshold 0.01:\n"))
cat(sprintf("  BUY: %d\n", sum(signals == "BUY", na.rm=TRUE)))
cat(sprintf("  SELL: %d\n", sum(signals == "SELL", na.rm=TRUE)))
cat(sprintf("  HOLD: %d\n", sum(is.na(signals))))

# Try adaptive threshold
adaptive <- compute_adaptive_threshold(rf_pred$predicted, percentile = 0.2)
cat(sprintf("\nAdaptive threshold:\n"))
cat(sprintf("  BUY threshold: %.6f\n", adaptive$threshold_buy))
cat(sprintf("  SELL threshold: %.6f\n", adaptive$threshold_sell))
cat(sprintf("  BUY signals: %d\n", sum(adaptive$signals == "BUY", na.rm=TRUE)))
cat(sprintf("  SELL signals: %d\n", sum(adaptive$signals == "SELL", na.rm=TRUE)))

# Now test the actual trading evaluation with a simple example
cat("\n\n=== Testing execution engine ===\n")

# Load OHLCV
source(file.path(dirname(ml_dir), "data_retrieval.R"), local = TRUE)
ohlcv <- fetch_ohlcv("AAPL", "2020-01-01", "2025-10-01")

# Get prediction dates
pred_dates <- as.Date(rf_pred$date)
ohlcv_dates <- as.Date(zoo::index(ohlcv))

cat(sprintf("\nOHLCV date range: %s to %s\n", min(ohlcv_dates), max(ohlcv_dates)))
cat(sprintf("Prediction date range: %s to %s\n", min(pred_dates), max(pred_dates)))

# Check overlap
overlap <- pred_dates[pred_dates %in% ohlcv_dates]
cat(sprintf("Overlapping dates: %d\n", length(overlap)))

# Create a simple test with manual signals
test_dates <- overlap[1:20]
test_signals <- rep(NA_character_, 20)
test_signals[1] <- "BUY"   # Buy at first date
test_signals[10] <- "SELL" # Sell at 10th date

cat(sprintf("\nTest signals: %d BUY, %d SELL\n", sum(test_signals == "BUY"), sum(test_signals == "SELL")))

# Create signal xts
signal_xts <- xts::xts(test_signals, order.by = test_dates)

# Get OHLCV subset
ohlcv_subset <- ohlcv[test_dates, ]
cat(sprintf("OHLCV subset rows: %d\n", NROW(ohlcv_subset)))

# Run backtest
source(file.path(dirname(ml_dir), "execution.R"), local = TRUE)

cat("\nRunning backtest with manual signals...\n")
bt_result <- tryCatch({
  run_backtest(
    ohlcv = ohlcv_subset,
    signals = signal_xts,
    initial_capital = 10000,
    start_date = min(test_dates),
    end_date = max(test_dates),
    ticker = "AAPL",
    strategy = "Test",
    parameters = list(),
    transaction_cost = 0.001
  )
}, error = function(e) {
  cat(sprintf("ERROR: %s\n", e$message))
  NULL
})

if (!is.null(bt_result)) {
  cat(sprintf("Trades executed: %d\n", bt_result$summary$total_trades))
  if (!is.null(bt_result$completed_trades) && nrow(bt_result$completed_trades) > 0) {
    cat("\nCompleted trades:\n")
    print(bt_result$completed_trades)
  }
  if (!is.null(bt_result$trade_history) && nrow(bt_result$trade_history) > 0) {
    cat("\nTrade history:\n")
    print(bt_result$trade_history)
  }
}
