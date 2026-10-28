# AlphaForge Insight — performance metrics (Phase 5)
#
# All metrics are computed from the equity curve (portfolio values / daily
# returns) and the completed-trade log. Definitions follow
# IMPLEMENTATION_PLAN.md section J with the Phase 5 convention decisions
# documented below (the completion note for Phase 5 records each change).
#
# Conventions (documented for reproducibility):
#
#   Returns:  r_t = V_t / V_{t-1} - 1, where V_t is the close-marked
#             portfolio value. The first daily return is NA by construction
#             (no previous value); it is never silently replaced with 0.
#
#   Total Return (decimal internally):
#             TR = V_T / V_0 - 1
#
#   CAGR (decimal internally):
#             years       = (end_date - start_date in days) / 365.25
#                           (plan section J; uses the REQUESTED date range,
#                            not an assumed 252-day count)
#             CAGR = (V_T / V_0)^(1 / years) - 1
#
#   Volatility (annualized, decimal):
#             vol = sd(r_t) * sqrt(252); NA if fewer than 2 returns.
#
#   Risk-free daily rate (Phase 5 exact-compounding convention):
#             daily_rf = (1 + annual_rf)^(1/252) - 1
#             annual_rf = 0.02 (2%, documented Review 1 assumption).
#             NOTE: this supersedes the linear r_f = R_f / 252 sketch in
#             plan section J; the exact compounding is the implemented and
#             documented convention (PAPER.md section 8).
#
#   Sharpe (annualized):
#             excess_t = r_t - daily_rf
#             Sharpe = mean(excess) / sd(excess) * sqrt(252)
#             NA if sd(excess) == 0 (zero-return-variance case) or fewer
#             than 2 non-NA excess observations.
#
#   Sortino (annualized; MAR = daily risk-free rate):
#             downside_t   = min(r_t - daily_rf, 0)
#             downside_dev = sqrt(mean(downside_t^2))     (all observations)
#             Sortino = (mean(r_t) - daily_rf) / downside_dev * sqrt(252)
#             NA if downside_dev == 0 (zero-downside case) or fewer than 2
#             non-NA returns. Uses the same daily risk-free rate as Sharpe
#             (same annual assumption), NOT a duplicate of the Sharpe
#             denominator. Supersedes the MAR = 0 sketch in plan section J.
#
#   Maximum Drawdown (decimal fraction, negative):
#             peak_t   = max(V_1..V_t)
#             drawdown_t = V_t / peak_t - 1
#             MaxDD = min(drawdown_t)          (<= 0; 0 for a flat/rising curve)
#             Presentation layers convert the magnitude to a positive
#             percentage via `drawdown_magnitude_pct()`.
#
#   Win Rate (decimal):
#             Win Rate = profitable completed round trips / total completed
#             round trips. NA (documented) when there are NO completed round
#             trips. An open position is never a completed trade and never
#             counts as a win or a loss.
#
#   Total Trades:
#             One completed BUY -> SELL round trip = ONE trade (Phase 5
#             specification section 18; supersedes the count-of-executions
#             sketch in plan section J). Open positions do not count.
#
#   Undefined metrics return NA — never Inf, never NaN, never an arbitrary
#   fabricated number.

# ---------------------------------------------------------------------------
# Individual metric functions
# ---------------------------------------------------------------------------

#' Total return (decimal): V_T / V_0 - 1.
#' @param portfolio_values Numeric vector of close-marked portfolio values.
#' @param initial_capital Initial capital (V_0).
#' @return Decimal total return, or NA if inputs are invalid.
metric_total_return <- function(portfolio_values, initial_capital) {
  if (length(portfolio_values) == 0L || is.na(initial_capital) ||
      !is.finite(initial_capital) || initial_capital <= 0) {
    return(NA_real_)
  }
  final_v <- utils::tail(as.numeric(portfolio_values), 1)
  if (length(final_v) != 1L || is.na(final_v) || !is.finite(final_v)) {
    return(NA_real_)
  }
  final_v / initial_capital - 1
}

#' CAGR (decimal, annualized with 365.25-day years).
#' @param portfolio_values Numeric vector of portfolio values.
#' @param initial_capital Initial capital.
#' @param start_date Requested backtest start (Date or YYYY-MM-DD).
#' @param end_date Requested backtest end (Date or YYYY-MM-DD).
#' @return Decimal CAGR, or NA when the duration/final value makes it
#'   undefined (years <= 0, non-positive final value, invalid inputs).
metric_cagr <- function(portfolio_values, initial_capital, start_date, end_date) {
  if (length(portfolio_values) == 0L || is.na(initial_capital) ||
      !is.finite(initial_capital) || initial_capital <= 0) {
    return(NA_real_)
  }
  final_v <- utils::tail(as.numeric(portfolio_values), 1)
  if (length(final_v) != 1L || is.na(final_v) || !is.finite(final_v) ||
      final_v <= 0) {
    return(NA_real_)
  }
  s <- tryCatch(as.Date(start_date), error = function(e) NA)
  e <- tryCatch(as.Date(end_date), error = function(e) NA)
  if (is.na(s) || is.na(e)) return(NA_real_)
  years <- as.numeric(difftime(e, s, units = "days")) / 365.25
  if (is.na(years) || !is.finite(years) || years <= 0) return(NA_real_)
  (final_v / initial_capital) ^ (1 / years) - 1
}

#' Annualized volatility (decimal): sd(r_t) * sqrt(252).
#' @param daily_returns Numeric vector of daily portfolio returns (NA allowed
#'   for the first observation).
#' @return Decimal annualized volatility, or NA if fewer than 2 non-NA
#'   observations.
metric_volatility <- function(daily_returns) {
  r <- as.numeric(daily_returns)
  r <- r[!is.na(r)]
  if (length(r) < 2L) return(NA_real_)
  stats::sd(r) * sqrt(252)
}

#' Annualized Sharpe ratio with exact-compounding daily risk-free rate.
#' @param daily_returns Numeric vector of daily returns.
#' @param risk_free_rate Annual risk-free rate (default 0.02, i.e. 2%).
#' @return Sharpe, or NA when undefined (zero excess variance, insufficient
#'   observations, invalid risk-free rate).
metric_sharpe <- function(daily_returns, risk_free_rate = 0.02) {
  if (!is.numeric(risk_free_rate) || length(risk_free_rate) != 1L ||
      is.na(risk_free_rate) || !is.finite(risk_free_rate) || risk_free_rate < 0) {
    return(NA_real_)
  }
  daily_rf <- (1 + risk_free_rate) ^ (1 / 252) - 1
  excess <- as.numeric(daily_returns) - daily_rf
  excess <- excess[!is.na(excess)]
  if (length(excess) < 2L) return(NA_real_)
  sd_e <- stats::sd(excess)
  if (is.na(sd_e) || sd_e == 0) return(NA_real_)  # zero variance -> NA, not Inf
  mean(excess) / sd_e * sqrt(252)
}

#' Annualized Sortino ratio (MAR = daily risk-free rate).
#'
#' Uses the same 2%-annual risk-free assumption as Sharpe, converted to its
#' exact daily equivalent. The denominator is the downside deviation (the
#' root-mean-square of days BELOW the MAR), so this is not a Sharpe clone.
#'
#' @param daily_returns Numeric vector of daily returns.
#' @param risk_free_rate Annual risk-free rate (default 0.02).
#' @return Sortino, or NA when undefined (zero downside deviation,
#'   insufficient observations, invalid risk-free rate).
metric_sortino <- function(daily_returns, risk_free_rate = 0.02) {
  if (!is.numeric(risk_free_rate) || length(risk_free_rate) != 1L ||
      is.na(risk_free_rate) || !is.finite(risk_free_rate) || risk_free_rate < 0) {
    return(NA_real_)
  }
  daily_rf <- (1 + risk_free_rate) ^ (1 / 252) - 1
  r <- as.numeric(daily_returns)
  if (sum(!is.na(r)) < 2L) return(NA_real_)
  downside <- pmin(r - daily_rf, 0)
  downside_dev <- sqrt(mean(downside ^ 2, na.rm = TRUE))
  if (is.na(downside_dev) || downside_dev == 0) {
    return(NA_real_)  # zero downside -> NA, not Inf
  }
  (mean(r, na.rm = TRUE) - daily_rf) / downside_dev * sqrt(252)
}

#' Maximum drawdown (negative decimal fraction).
#'
#' Computed from the EQUITY CURVE (portfolio values), never from raw daily
#' returns: drawdown_t = V_t / max(V_1..V_t) - 1; MaxDD = min(drawdown).
#' The magnitude (positive percentage) is derived at presentation boundaries
#' via drawdown_magnitude_pct().
#'
#' @param portfolio_values Numeric vector of close-marked portfolio values.
#' @return Negative decimal (e.g. -0.25 for a 25% peak-to-trough decline),
#'   0 for a flat/never-declining curve, NA when undefined.
metric_max_drawdown <- function(portfolio_values) {
  v <- as.numeric(portfolio_values)
  if (length(v) == 0L || anyNA(v) || any(!is.finite(v))) return(NA_real_)
  peak <- cummax(v)
  if (any(peak == 0)) return(NA_real_)  # cannot compute ratios from zero peaks
  drawdown <- v / peak - 1
  min(drawdown)
}

#' Magnitude of a maximum-drawdown fraction as a positive percentage number.
#' @param max_drawdown Negative decimal from metric_max_drawdown (or 0).
#' @return Positive percentage magnitude, e.g. 25 for -0.25. NA preserved.
drawdown_magnitude_pct <- function(max_drawdown) {
  if (is.na(max_drawdown)) return(NA_real_)
  abs(max_drawdown) * 100
}

#' Win rate (decimal) from completed round-trip trades only.
#'
#' @param completed_trades data.frame with a `profit_loss` column (0-row
#'   frames allowed).
#' @return Decimal win rate in [0, 1], or NA (documented) when there are no
#'   completed trades. Open positions are excluded by construction.
metric_win_rate <- function(completed_trades) {
  if (is.null(completed_trades) || nrow(completed_trades) == 0L) {
    return(NA_real_)
  }
  pnl <- completed_trades$profit_loss
  if (is.null(pnl)) return(NA_real_)
  pnl <- pnl[!is.na(pnl)]
  if (length(pnl) == 0L) return(NA_real_)
  sum(pnl > 0) / length(pnl)
}

#' Total trades: one completed BUY -> SELL round trip = one trade.
#'
#' @param completed_trades data.frame of completed round trips.
#' @return Integer count of completed round trips (0 when none). Open
#'   positions are not completed trades and are not counted.
metric_total_trades <- function(completed_trades) {
  if (is.null(completed_trades)) return(0L)
  as.integer(nrow(completed_trades))
}

# ---------------------------------------------------------------------------
# Summary + full result object
# ---------------------------------------------------------------------------

#' Compute the eight-metric summary from an execution output.
#'
#' @param equity_curve data.frame (date, cash, shares, close, portfolio_value,
#'   daily_return) from execute_backtest.
#' @param completed_trades data.frame of completed round trips.
#' @param initial_capital Initial capital.
#' @param start_date,end_date Requested backtest date range (CAGR years).
#' @param risk_free_rate Annual risk-free rate (default 0.02).
#' @return Named list: initial_capital, final_value, total_return, cagr,
#'   sharpe, sortino, maximum_drawdown, win_rate, volatility, total_trades.
compute_performance_metrics <- function(equity_curve, completed_trades,
                                        initial_capital,
                                        start_date, end_date,
                                        risk_free_rate = 0.02) {
  pv <- as.numeric(equity_curve$portfolio_value)
  ret <- as.numeric(equity_curve$daily_return)
  list(
    initial_capital = initial_capital,
    final_value = utils::tail(pv, 1),
    total_return = metric_total_return(pv, initial_capital),
    cagr = metric_cagr(pv, initial_capital, start_date, end_date),
    sharpe = metric_sharpe(ret, risk_free_rate),
    sortino = metric_sortino(ret, risk_free_rate),
    maximum_drawdown = metric_max_drawdown(pv),
    win_rate = metric_win_rate(completed_trades),
    volatility = metric_volatility(ret),
    total_trades = metric_total_trades(completed_trades)
  )
}

#' Assemble the clean internal Phase 5 result object.
#'
#' @param execution Output of execute_backtest().
#' @param summary Output of compute_performance_metrics().
#' @param metadata list(ticker, strategy, parameters, start_date, end_date,
#'   transaction_cost, risk_free_rate, execution_convention).
#' @return list(summary, equity_curve, trades, metadata). R lists/data.frames
#'   only — the API JSON representation is a separate concern (Phase 6).
build_backtest_result <- function(execution, summary, metadata) {
  list(
    summary = summary,
    equity_curve = execution$equity_curve,
    trades = list(
      history = execution$trade_history,
      completed = execution$completed_trades,
      open = execution$open_position
    ),
    metadata = metadata
  )
}