# AlphaForge Insight — Trading evaluation for regression predictions (Phase H)
#
# Converts regression predictions into trading signals and evaluates them
# using the EXISTING execution engine.
#
# Signal generation:
#   - Predicted return > threshold -> BUY signal
#   - Predicted return < -threshold -> SELL signal
#   - Otherwise -> HOLD (no signal)
#
# IMPORTANT ALIGNMENT NOTE:
#   - The prediction at time t forecasts Close[t+horizon]/Close[t] - 1.
#   - Execution can only occur at Open[t+1] or later (no look-ahead).
#   - The realized return from Open[t+1] to Close[t+horizon] differs from
#     the forecast by one day of overnight price movement (Close[t] to Open[t+1]).
#   - This gap is unavoidable and must be documented in results.
#
# THRESHOLD GUIDANCE:
#   - Default threshold 0.02 (2%) was too high for 5-day returns
#   - New default 0.01 (1%) is more realistic for 5-day horizons
#   - Use quantile-based thresholds for adaptive signal generation

library(xts)

#' Convert regression predictions to trading signals.
#'
#' @param predictions Numeric vector of predicted returns.
#' @param threshold Minimum absolute return to trigger BUY/SELL (default 0.01).
#' @return Character vector: "BUY", "SELL", or NA (HOLD).
predictions_to_signals <- function(predictions, threshold = 0.01) {
  predictions <- as.numeric(predictions)
  signals <- rep(NA_character_, length(predictions))
  
  signals[predictions > threshold] <- "BUY"
  signals[predictions < -threshold] <- "SELL"
  # NA signals = HOLD
  
  signals
}

#' Compute adaptive threshold based on prediction distribution.
#'
#' Uses percentiles of prediction distribution to generate signals.
#' Top/bottom 20% of predictions become BUY/SELL signals.
#'
#' @param predictions Numeric vector of predicted returns.
#' @param percentile Percentile cutoff for signals (default 0.2 = 20%).
#' @return list(threshold_buy, threshold_sell, signals).
compute_adaptive_threshold <- function(predictions, percentile = 0.2) {
  predictions <- as.numeric(predictions)
  predictions <- predictions[!is.na(predictions)]
  
  if (length(predictions) < 10) {
    return(list(
      threshold_buy = 0.01,
      threshold_sell = -0.01,
      signals = predictions_to_signals(predictions, 0.01)
    ))
  }
  
  # Use percentiles for adaptive thresholds
  threshold_buy <- quantile(predictions, probs = 1 - percentile, na.rm = TRUE)
  threshold_sell <- quantile(predictions, probs = percentile, na.rm = TRUE)
  
  signals <- rep(NA_character_, length(predictions))
  signals[predictions >= threshold_buy] <- "BUY"
  signals[predictions <= threshold_sell] <- "SELL"
  
  list(
    threshold_buy = as.numeric(threshold_buy),
    threshold_sell = as.numeric(threshold_sell),
    signals = signals
  )
}

#' Run trading backtest for regression predictions.
#'
#' Takes predictions aligned to dates, generates signals, and runs the
#' existing backtest engine.
#'
#' TIMESTAMP ALIGNMENT:
#'   - prediction_dates are the dates at which predictions were made (time t)
#'   - Signals at time t are executed at Open[t+1] by the execution engine
#'   - The prediction forecasts Close[t+5]/Close[t]-1
#'   - The realized return from Open[t+1] to Close[t+5] differs from forecast
#'
#' @param ohlcv Full OHLCV xts for the ticker.
#' @param prediction_dates Dates corresponding to predictions (signal dates).
#' @param predictions Numeric vector of predicted returns.
#' @param threshold Signal threshold (default 0.01 for 5-day horizon).
#' @param use_adaptive_threshold Use quantile-based adaptive threshold (default TRUE).
#' @param initial_capital Starting capital.
#' @param ticker Ticker symbol.
#' @return list(backtest_result, signals, alignment_note).
evaluate_predictions_trading <- function(ohlcv, prediction_dates, predictions,
                                         threshold = 0.01, use_adaptive_threshold = TRUE,
                                         initial_capital = 10000, ticker = "UNKNOWN") {
  # Source execution engine and metrics.
  ml_dir <- get("ml_dir", envir = parent.frame())
  if (is.null(ml_dir)) {
    ml_dir <- tryCatch(get(".ML_DIR", envir = globalenv()), error = function(e) NULL)
  }
  if (is.null(ml_dir)) {
    stop("ml_dir not found.")
  }
  analytics_dir <- dirname(ml_dir)
  source(file.path(analytics_dir, "metrics.R"), local = TRUE)
  source(file.path(analytics_dir, "execution.R"), local = TRUE)
  
  # Align predictions to OHLCV dates.
  ohlcv_dates <- as.Date(zoo::index(ohlcv))
  prediction_dates <- as.Date(prediction_dates)
  
  # Find overlapping dates.
  overlap_idx <- match(prediction_dates, ohlcv_dates)
  valid <- !is.na(overlap_idx)
  
  if (sum(valid) < 5) {
    return(list(
      success = FALSE,
      error = "Fewer than 5 overlapping dates between predictions and OHLCV.",
      backtest_result = NULL
    ))
  }
  
  # Filter to valid predictions.
  pred_valid <- predictions[valid]
  dates_valid <- prediction_dates[valid]
  
  # Generate signals.
  if (use_adaptive_threshold) {
    # Use adaptive threshold based on prediction distribution
    adaptive <- compute_adaptive_threshold(pred_valid, percentile = 0.2)
    signals_char <- adaptive$signals
    threshold_used <- list(
      buy = adaptive$threshold_buy,
      sell = adaptive$threshold_sell,
      method = "adaptive_quantile"
    )
  } else {
    signals_char <- predictions_to_signals(pred_valid, threshold)
    threshold_used <- list(
      buy = threshold,
      sell = -threshold,
      method = "fixed"
    )
  }
  
  # Count signals generated
  n_buy <- sum(signals_char == "BUY", na.rm = TRUE)
  n_sell <- sum(signals_char == "SELL", na.rm = TRUE)
  n_hold <- sum(is.na(signals_char))
  
  # Get OHLCV subset that includes signal dates AND next day for execution
  # The engine executes signal[t] at Open[t+1], so we need at least one day after
  min_date <- min(dates_valid)
  max_date <- max(dates_valid)
  
  # Get all OHLCV dates in range (need t+1 for execution)
  ohlcv_in_range <- ohlcv[ohlcv_dates >= min_date & ohlcv_dates <= max_date, ]
  
  if (NROW(ohlcv_in_range) == 0) {
    return(list(
      success = FALSE,
      error = "No OHLCV data in prediction date range.",
      backtest_result = NULL
    ))
  }
  
  # Create signal xts aligned to OHLCV dates in range
  ohlcv_range_dates <- as.Date(zoo::index(ohlcv_in_range))
  signal_vec <- rep(NA_character_, length(ohlcv_range_dates))
  
  # Map prediction dates to OHLCV range dates
  signal_idx <- match(dates_valid, ohlcv_range_dates)
  signal_vec[signal_idx[!is.na(signal_idx)]] <- signals_char[!is.na(signal_idx)]
  
  signal_xts <- xts::xts(signal_vec, order.by = ohlcv_range_dates)
  
  # Run backtest using existing engine.
  bt_result <- tryCatch({
    run_backtest(
      ohlcv = ohlcv_in_range,
      signals = signal_xts,
      initial_capital = initial_capital,
      start_date = min(ohlcv_range_dates),
      end_date = max(ohlcv_range_dates),
      ticker = ticker,
      strategy = "Regression_Forecast",
      parameters = list(threshold = threshold_used, horizon = 5),
      transaction_cost = 0.001  # 10 bps default
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
  
  # Extract trade count safely
  n_trades <- 0
  if (!is.null(bt_result$completed_trades) && is.data.frame(bt_result$completed_trades)) {
    n_trades <- nrow(bt_result$completed_trades)
  } else if (!is.null(bt_result$trade_history) && is.data.frame(bt_result$trade_history)) {
    n_trades <- nrow(bt_result$trade_history)
  }
  
  list(
    success = TRUE,
    backtest_result = bt_result,
    signals = data.frame(
      date = as.character(dates_valid),
      predicted_return = as.numeric(pred_valid),
      signal = signals_char,
      stringsAsFactors = FALSE
    ),
    signal_summary = list(
      n_buy = n_buy,
      n_sell = n_sell,
      n_hold = n_hold,
      threshold_used = threshold_used
    ),
    n_trades = n_trades,
    alignment_note = paste0(
      "Predictions forecast Close[t+5]/Close[t]-1. ",
      "Signals generated at Close[t], executed at Open[t+1]. ",
      "Realized return differs from forecast by overnight gap."
    )
  )
}

#' Generate Buy & Hold baseline.
#'
#' Buys at the first available date, holds through the entire period.
#'
#' @param ohlcv OHLCV xts for the period.
#' @param initial_capital Starting capital.
#' @return list(backtest_result, strategy = "Buy_and_Hold").
run_buy_hold_baseline <- function(ohlcv, initial_capital = 10000) {
  ml_dir <- get("ml_dir", envir = parent.frame())
  if (is.null(ml_dir)) {
    ml_dir <- tryCatch(get(".ML_DIR", envir = globalenv()), error = function(e) NULL)
  }
  if (is.null(ml_dir)) {
    stop("ml_dir not found.")
  }
  analytics_dir <- dirname(ml_dir)
  source(file.path(analytics_dir, "metrics.R"), local = TRUE)
  source(file.path(analytics_dir, "execution.R"), local = TRUE)
  
  dates <- zoo::index(ohlcv)
  n <- length(dates)
  
  # BUY at first date, no SELL (hold to end).
  signals <- rep(NA_character_, n)
  signals[1] <- "BUY"
  
  signal_xts <- xts::xts(signals, order.by = dates)
  
  run_backtest(
    ohlcv = ohlcv,
    signals = signal_xts,
    initial_capital = initial_capital,
    start_date = min(dates),
    end_date = max(dates),
    ticker = "BASELINE",
    strategy = "Buy_and_Hold",
    parameters = list(),
    transaction_cost = 0.001
  )
}

#' Generate MA Crossover baseline.
#'
#' Simple 10/50 SMA crossover strategy.
#'
#' @param ohlcv OHLCV xts.
#' @param initial_capital Starting capital.
#' @return list(backtest_result, strategy = "MA_Crossover").
run_ma_crossover_baseline <- function(ohlcv, initial_capital = 10000) {
  ml_dir <- get("ml_dir", envir = parent.frame())
  if (is.null(ml_dir)) {
    ml_dir <- tryCatch(get(".ML_DIR", envir = globalenv()), error = function(e) NULL)
  }
  if (is.null(ml_dir)) {
    stop("ml_dir not found.")
  }
  analytics_dir <- dirname(ml_dir)
  source(file.path(analytics_dir, "metrics.R"), local = TRUE)
  source(file.path(analytics_dir, "execution.R"), local = TRUE)
  
  close <- ohlcv[, "Close"]
  sma10 <- TTR::SMA(close, n = 10)
  sma50 <- TTR::SMA(close, n = 50)
  
  dates <- zoo::index(ohlcv)
  n <- length(dates)
  
  signals <- rep(NA_character_, n)
  
  # Generate crossover signals.
  for (i in 51:n) {
    if (!is.na(sma10[i]) && !is.na(sma50[i])) {
      if (as.numeric(sma10[i]) > as.numeric(sma50[i]) && 
          as.numeric(sma10[i-1]) <= as.numeric(sma50[i-1])) {
        signals[i] <- "BUY"
      } else if (as.numeric(sma10[i]) < as.numeric(sma50[i]) && 
                 as.numeric(sma10[i-1]) >= as.numeric(sma50[i-1])) {
        signals[i] <- "SELL"
      }
    }
  }
  
  signal_xts <- xts::xts(signals, order.by = dates)
  
  run_backtest(
    ohlcv = ohlcv,
    signals = signal_xts,
    initial_capital = initial_capital,
    start_date = min(dates),
    end_date = max(dates),
    ticker = "BASELINE",
    strategy = "MA_Crossover_10_50",
    parameters = list(fast = 10, slow = 50),
    transaction_cost = 0.001
  )
}

#' Compare trading performance across models and baselines.
#'
#' @param ohlcv OHLCV xts for the test period.
#' @param prediction_results Named list of prediction data.frames with columns:
#'   date, predicted, actual.
#' @param threshold Signal generation threshold (default 0.01 for 5-day horizon).
#' @param use_adaptive_threshold Use quantile-based adaptive threshold (default TRUE).
#' @param initial_capital Starting capital.
#' @return data.frame with one row per model/baseline and trading metrics.
compare_trading_performance <- function(ohlcv, prediction_results, threshold = 0.01,
                                        use_adaptive_threshold = TRUE,
                                        initial_capital = 10000) {
  results <- list()
  
  # Evaluate each model's predictions.
  for (model_name in names(prediction_results)) {
    pred_df <- prediction_results[[model_name]]
    if (is.null(pred_df) || nrow(pred_df) == 0) next
    
    trading_result <- tryCatch({
      evaluate_predictions_trading(
        ohlcv, pred_df$date, pred_df$predicted,
        threshold = threshold, 
        use_adaptive_threshold = use_adaptive_threshold,
        initial_capital = initial_capital
      )
    }, error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    })
    
    if (trading_result$success && !is.null(trading_result$backtest_result)) {
      bt <- trading_result$backtest_result
      
      # Safely extract metrics
      cagr <- if (!is.null(bt$summary$cagr)) bt$summary$cagr else NA_real_
      sharpe <- if (!is.null(bt$summary$sharpe)) bt$summary$sharpe else NA_real_
      maxdd <- if (!is.null(bt$summary$maximum_drawdown)) bt$summary$maximum_drawdown else NA_real_
      winrate <- if (!is.null(bt$summary$win_rate)) bt$summary$win_rate else NA_real_
      trades <- if (!is.null(bt$summary$total_trades)) bt$summary$total_trades else trading_result$n_trades
      
      results[[model_name]] <- data.frame(
        Model = model_name,
        Type = "ML",
        CAGR = cagr,
        Sharpe = sharpe,
        MaxDD = maxdd,
        WinRate = winrate,
        Trades = trades,
        N_Buy = trading_result$signal_summary$n_buy,
        N_Sell = trading_result$signal_summary$n_sell,
        stringsAsFactors = FALSE
      )
    } else {
      results[[model_name]] <- data.frame(
        Model = model_name,
        Type = "ML",
        CAGR = NA_real_,
        Sharpe = NA_real_,
        MaxDD = NA_real_,
        WinRate = NA_real_,
        Trades = NA_integer_,
        N_Buy = NA_integer_,
        N_Sell = NA_integer_,
        Error = if (!is.null(trading_result$error)) trading_result$error else "Unknown error",
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Add Buy & Hold baseline.
  bh <- tryCatch(
    run_buy_hold_baseline(ohlcv, initial_capital),
    error = function(e) NULL
  )
  if (!is.null(bh) && !is.null(bh$summary)) {
    results[["Buy_and_Hold"]] <- data.frame(
      Model = "Buy_and_Hold",
      Type = "Baseline",
      CAGR = if (!is.null(bh$summary$cagr)) bh$summary$cagr else NA_real_,
      Sharpe = if (!is.null(bh$summary$sharpe)) bh$summary$sharpe else NA_real_,
      MaxDD = if (!is.null(bh$summary$maximum_drawdown)) bh$summary$maximum_drawdown else NA_real_,
      WinRate = NA_real_,
      Trades = 1L,
      N_Buy = NA_integer_,
      N_Sell = NA_integer_,
      stringsAsFactors = FALSE
    )
  }
  
  # Add MA Crossover baseline.
  ma <- tryCatch(
    run_ma_crossover_baseline(ohlcv, initial_capital),
    error = function(e) NULL
  )
  if (!is.null(ma) && !is.null(ma$summary)) {
    results[["MA_Crossover"]] <- data.frame(
      Model = "MA_Crossover_10_50",
      Type = "Baseline",
      CAGR = if (!is.null(ma$summary$cagr)) ma$summary$cagr else NA_real_,
      Sharpe = if (!is.null(ma$summary$sharpe)) ma$summary$sharpe else NA_real_,
      MaxDD = if (!is.null(ma$summary$maximum_drawdown)) ma$summary$maximum_drawdown else NA_real_,
      WinRate = if (!is.null(ma$summary$win_rate)) ma$summary$win_rate else NA_real_,
      Trades = if (!is.null(ma$summary$total_trades)) ma$summary$total_trades else NA_integer_,
      N_Buy = NA_integer_,
      N_Sell = NA_integer_,
      stringsAsFactors = FALSE
    )
  }
  
  comparison <- do.call(rbind, results)
  rownames(comparison) <- NULL
  
  comparison
}
