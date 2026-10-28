# AlphaForge Insight — ML signal adapter + Top-N portfolio (ML Phase F)
#
# Bridges the ML prediction layer to the EXISTING backtesting engine without
# touching it (ML plan sections 9, 12, 20). Responsibilities:
#
#   1. Rank the cross-section of stocks by P(BUY) on a rebalance date
#      (probability-based stock ranking, ML plan section 8).
#   2. Select the Top-N (configurable) with equal weights.
#   3. Convert each selected stock's ML predictions into the engine's
#      exact signal contract (BUY/SELL/NA xts) and run the UNMODIFIED
#      run_backtest() per ticker (t+1-open execution preserved).
#   4. Aggregate the per-ticker equity curves into an equal-weight portfolio
#      curve and report the existing 8 metrics on that curve — computed by
#      the existing metrics layer, not a new one.
#
# The engine stays model-agnostic: it only ever sees its own signal format.

library(xts)
suppressPackageStartupMessages(library(TTR))

REBALANCE_PERIODS <- list(daily = 1, weekly = 5, biweekly = 10, monthly = 21)

#' Validate Top-N / rebalance configuration.
#'
#' @param top_n Raw Top-N count.
#' @param rebalance Raw rebalance frequency (daily/weekly/biweekly/monthly or
#'   a positive integer number of trading days).
#' @return list(top_n = integer, rebalance_days = integer).
#' @throws validation_error on violation.
validate_portfolio_config <- function(top_n = 5, rebalance = "weekly") {
  n <- suppressWarnings(as.integer(top_n))
  if (is.na(n) || n < 1) {
    stop(validation_error("top_n must be a positive integer (>= 1)."))
  }
  if (is.numeric(rebalance) && length(rebalance) == 1) {
    days <- as.integer(rebalance)
    if (is.na(days) || days < 1) {
      stop(validation_error("rebalance must be a named period or a positive integer."))
    }
  } else {
    key <- tolower(trimws(as.character(rebalance)))
    days <- REBALANCE_PERIODS[[key]]
    if (is.null(days)) {
      stop(validation_error(sprintf(
        "Unknown rebalance period '%s'. Supported: %s.",
        key, paste(names(REBALANCE_PERIODS), collapse = ", ")
      )))
    }
  }
  list(top_n = n, rebalance_days = days)
}

#' Rank stocks by predicted P(BUY) on a given date.
#'
#' @param prediction_frames Named list of prediction-record data.frames keyed
#'   by ticker (each with date, prob_buy, predicted_class).
#' @param as_of Date (or "YYYY-MM-DD") to rank on. The latest available
#'   prediction date <= as_of is used per ticker.
#' @param top_n Number of stocks to keep.
#' @return data.frame(ticker, prob_buy, prediction_date, predicted_class,
#'   rank) sorted best-first; only the top_n rows carry rank <= top_n.
rank_stocks_by_prob_buy <- function(prediction_frames, as_of, top_n = 5) {
  as_of <- as.Date(as_of)
  rows <- list()
  for (ticker in names(prediction_frames)) {
    recs <- prediction_frames[[ticker]]
    if (is.null(recs) || nrow(recs) == 0) next
    eligible <- recs[as.Date(recs$date) <= as_of, , drop = FALSE]
    if (nrow(eligible) == 0) next
    last <- eligible[nrow(eligible), , drop = FALSE]
    rows[[length(rows) + 1]] <- data.frame(
      ticker = ticker,
      prob_buy = as.numeric(last$prob_buy),
      prediction_date = as.Date(last$date),
      predicted_class = as.character(last$predicted_class),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(ticker = character(0), prob_buy = numeric(0),
                      prediction_date = as.Date(character(0)),
                      predicted_class = character(0), rank = integer(0)))
  }
  ranked <- do.call(rbind, rows)
  ranked <- ranked[order(-ranked$prob_buy, ranked$ticker), , drop = FALSE]
  ranked$rank <- seq_len(nrow(ranked))
  ranked[ranked$rank <= top_n, , drop = FALSE]
}

#' Run the EXISTING engine on one ticker's ML signals for a date window.
#'
#' Pure adapter: predictions -> ml_signal_series -> run_backtest (unchanged).
#'
#' @param ohlcv Canonical OHLCV xts for the ticker (full range).
#' @param records prediction_records() data.frame for the ticker (test rows).
#' @param window Index of dates to backtest (Date vector).
#' @param initial_capital Per-position capital.
#' @param signal_mode,prob_threshold Adapter settings.
#' @return The existing-engine result object (run_backtest output).
backtest_ticker_with_ml_signals <- function(ohlcv, records, window,
                                            initial_capital = 10000,
                                            signal_mode = "classes",
                                            prob_threshold = 0.5) {
  ohlcv_win <- ohlcv[as.Date(zoo::index(ohlcv)) %in% as.Date(window), ]
  if (NROW(ohlcv_win) < 3) {
    stop(validation_error(
      "Not enough overlapping market data in the backtest window."
    ))
  }
  sig <- ml_signal_series(records, zoo::index(ohlcv_win), signal_mode,
                          prob_threshold)
  run_backtest(
    ohlcv = ohlcv_win,
    signals = sig,
    initial_capital = initial_capital,
    start_date = utils::head(zoo::index(ohlcv_win), 1),
    end_date = utils::tail(zoo::index(ohlcv_win), 1),
    ticker = records$ticker[[1]] %||% NA_character_,
    strategy = "ML",
    parameters = list(signal_mode = signal_mode, prob_threshold = prob_threshold)
  )
}

#' Aggregate equal-weight per-ticker equity curves into a portfolio curve.
#'
#' Each ticker's daily percentage return is averaged across tickers per date
#' (equal weight, no fabrication: dates missing for one ticker are skipped
#' for that ticker; a date with no series at all is dropped). The portfolio
#' metric set is then computed by the EXISTING metrics functions.
#'
#' @param per_ticker_results Named list of run_backtest() outputs.
#' @param initial_capital Portfolio starting capital.
#' @param start_date,end_date Requested range for CAGR (Dates or strings).
#' @param risk_free_rate Annual risk-free rate (existing default 0.02).
#' @return list(portfolio_value = xts, daily_return = xts, metrics =
#'   compute_performance_metrics() output).
aggregate_portfolio_equity <- function(per_ticker_results, initial_capital,
                                       start_date, end_date,
                                       risk_free_rate = 0.02) {
  returns <- list()
  for (ticker in names(per_ticker_results)) {
    ec <- per_ticker_results[[ticker]]$equity_curve
    if (is.null(ec) || nrow(ec) < 2) next
    r <- ec$daily_return
    names(r) <- format(as.Date(ec$date), "%Y-%m-%d")
    returns[[ticker]] <- r
  }
  if (length(returns) == 0) {
    stop(validation_error(
      "No equity curves were produced; the backtest window may be too short."
    ))
  }
  all_dates <- sort(unique(unlist(lapply(returns, names))))
  port_ret <- vapply(all_dates, function(d) {
    vals <- vapply(returns, function(r) {
      v <- r[[d]]
      if (is.null(v) || is.na(v)) NA_real_ else v
    }, numeric(1))
    ok <- vals[!is.na(vals)]
    # The first curve date has NA returns for EVERY ticker by construction
    # (c(NA, ...)); mean(numeric(0)) would return NaN and cumprod() would
    # poison the whole portfolio-value curve. A date with no observable
    # return contributes 0 (no change in value).
    if (length(ok) == 0) 0 else mean(ok)
  }, numeric(1))

  value <- initial_capital * cumprod(1 + port_ret)
  dates <- as.Date(all_dates)
  equity_curve <- data.frame(
    date = dates,
    portfolio_value = as.numeric(value),
    daily_return = c(NA_real_, port_ret[-1]),
    stringsAsFactors = FALSE
  )
  completed <- do.call(rbind, lapply(per_ticker_results,
                                     function(x) x$completed_trades))
  metrics <- compute_performance_metrics(
    equity_curve, completed, initial_capital, start_date, end_date,
    risk_free_rate
  )
  list(
    portfolio_value = xts::xts(as.numeric(value), order.by = dates),
    daily_return = xts::xts(port_ret, order.by = dates),
    equity_curve = equity_curve,
    metrics = metrics
  )
}

#' Top-N probability-ranked portfolio backtest driven by ML predictions.
#'
#' Orchestrates ranking + selection + the EXISTING engine. Rebalance dates
#' are evaluated every `rebalance_days` trading days of the window; between
#' rebalances each selected ticker keeps its ML-driven signal series (the
#' engine's own state machine handles exits via SELL signals).
#'
#' @param market_data Named list of canonical OHLCV xts keyed by ticker.
#' @param prediction_frames Named list of prediction records keyed by ticker.
#' @param window Date vector (the TEST period) to trade over.
#' @param top_n Portfolio size (default 5).
#' @param rebalance Rebalance frequency (default "weekly").
#' @param initial_capital Total portfolio capital.
#' @param signal_mode,prob_threshold Adapter settings.
#' @return list(config, rebalances = list of {date, holdings data.frame},
#'   per_ticker = named list of engine result objects for tickers that were
#'   ever selected, portfolio = aggregate_portfolio_equity() output).
run_topn_portfolio_backtest <- function(market_data, prediction_frames,
                                        window, top_n = 5,
                                        rebalance = "weekly",
                                        initial_capital = 10000,
                                        signal_mode = "classes",
                                        prob_threshold = 0.5) {
  cfg <- validate_portfolio_config(top_n, rebalance)
  window <- sort(as.Date(window))
  if (length(window) < 5) {
    stop(validation_error(
      "The backtest window needs at least 5 trading days."
    ))
  }
  per_position_capital <- initial_capital / cfg$top_n

  rebalance_points <- window[seq(1, length(window), by = cfg$rebalance_days)]
  selected_union <- character(0)
  rebalances <- list()
  # NOTE: iterating with `for (d in rebalance_points)` strips the Date class
  # from each scalar (Date is a double under the hood, so the loop variable
  # arrives as a bare number). That broke `format(rb$date, "%Y-%m-%d")` in the
  # API layer with R's 'invalid \'trim\' argument' (the format string was
  # positionally bound to trim()) and aborted every /ml/backtest response.
  # Iterating over indices preserves the class; as.Date() below is defensive.
  for (i in seq_along(rebalance_points)) {
    d <- as.Date(rebalance_points[[i]])
    picks <- rank_stocks_by_prob_buy(prediction_frames, d, cfg$top_n)
    rebalances[[length(rebalances) + 1]] <- list(date = d, holdings = picks)
    selected_union <- union(selected_union, picks$ticker)
  }

  per_ticker <- list()
  for (ticker in selected_union) {
    ohlcv <- market_data[[ticker]]
    recs <- prediction_frames[[ticker]]
    if (is.null(ohlcv) || is.null(recs)) next
    per_ticker[[ticker]] <- backtest_ticker_with_ml_signals(
      ohlcv, recs, window, per_position_capital, signal_mode, prob_threshold
    )
  }

  portfolio <- aggregate_portfolio_equity(
    per_ticker, initial_capital, utils::head(window, 1),
    utils::tail(window, 1)
  )

  list(
    config = list(top_n = cfg$top_n,
                  rebalance_days = cfg$rebalance_days,
                  initial_capital = initial_capital,
                  signal_mode = signal_mode,
                  prob_threshold = prob_threshold),
    rebalances = rebalances,
    per_ticker = per_ticker,
    portfolio = portfolio
  )
}
