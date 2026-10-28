# AlphaForge Insight — deterministic signal generation (Phase 4)
#
# PURE signal-generation functions: convert indicator values into directional
# signals without executing trades, tracking cash/shares, or building equity
# curves. Trade execution (at t+1 open) belongs to Phase 5.
#
# Look-ahead bias convention (documented for reproducibility):
#   - A signal computed at time t uses ONLY data observed at or before t
#     (e.g. MA crossover compares fast[t] vs slow[t] and their prior values;
#     RSI compares rsi[t] vs rsi[t-1] and the thresholds).
#   - The eventual backtesting engine will EXECUTE the signal at the t+1 OPEN
#     so that signal and execution never share the same timestamp's close.
#
# MA crossover semantics (IMPLEMENTATION_PLAN.md § I, § H):
#   - Bullish crossover (BUY):  fast[t] > slow[t]  AND  fast[t-1] <= slow[t-1]
#   - Bearish crossover (SELL): fast[t] < slow[t]  AND  fast[t-1] >= slow[t-1]
#   - Otherwise: no signal (NA)
#   - Signals represent actual state transitions; repeated BUY while the fast
#     remains above the slow does NOT generate new signals.
#
# RSI semantics (IMPLEMENTATION_PLAN.md § I, § H):
#   - Phase 4 implements CROSSING semantics to avoid repeated signals:
#       BUY when:  rsi[t-1] > oversold  AND  rsi[t] <= oversold
#       SELL when: rsi[t-1] < overbought AND rsi[t] >= overbought
#   - This convention ensures signals only fire on threshold transitions, not
#     every period RSI remains below/above the threshold.
#   - Otherwise: no signal (NA)

#' MA-crossover signals from fast/slow SMA series.
#'
#' @param fast xts of the fast SMA (aligned with `slow`).
#' @param slow xts of the slow SMA (aligned with `fast`).
#' @return xts of character signals: "BUY", "SELL", or NA. NA everywhere the
#'   warm-up leaves the comparison undefined.
generate_ma_crossover_signals <- function(fast, slow) {
  stopifnot(inherits(fast, "xts"), inherits(slow, "xts"))
  if (!identical(index(fast), index(slow))) {
    stop("Fast and slow SMA series must share the same time index.")
  }
  f <- as.numeric(fast)
  s <- as.numeric(slow)
  n <- length(f)
  out <- rep(NA_character_, n)
  if (n >= 2) {
    # Bullish:  fast[t] > slow[t]  AND  fast[t-1] <= slow[t-1]  (plan semantics)
    # Bearish:  fast[t] < slow[t]  AND  fast[t-1] >= slow[t-1]
    prev_above_or_eq <- f[-n] >= s[-n]
    prev_below_or_eq <- f[-n] <= s[-n]
    cur_above <- f[-1] > s[-1]
    cur_below <- f[-1] < s[-1]
    # Crossover only when BOTH values at t-1 and t are defined.
    valid_prev <- !is.na(f[-n]) & !is.na(s[-n])
    valid_cur <- !is.na(f[-1]) & !is.na(s[-1])
    ok <- valid_prev & valid_cur
    buy <- ok & prev_below_or_eq & cur_above
    sell <- ok & prev_above_or_eq & cur_below
    out[-1][buy] <- "BUY"
    out[-1][sell] <- "SELL"
  }
  xts::xts(out, order.by = index(fast))
}

#' RSI crossing signals (Phase 4: no repeated signals).
#'
#' Implements the crossing semantics documented above: BUY when RSI crosses
#' INTO/BELOW the oversold region; SELL when RSI crosses INTO/ABOVE the
#' overbought region. No signal is generated every period RSI remains
#' below/above the threshold.
#'
#' @param rsi xts of RSI values.
#' @param oversold Threshold below which a BUY is signalled (on crossing).
#' @param overbought Threshold above which a SELL is signalled (on crossing).
#' @return xts of character signals: "BUY", "SELL", or NA.
generate_rsi_signals <- function(rsi, oversold, overbought) {
  stopifnot(inherits(rsi, "xts"))
  if (!is.numeric(oversold) || !is.numeric(overbought) ||
      length(oversold) != 1 || length(overbought) != 1 ||
      is.na(oversold) || is.na(overbought)) {
    stop("oversold and overbought must be single numbers.")
  }
  if (oversold <= 0 || overbought >= 100 || oversold >= overbought) {
    stop("RSI thresholds must satisfy 0 < oversold < overbought < 100.")
  }
  v <- as.numeric(rsi)
  n <- length(v)
  out <- rep(NA_character_, n)
  
  if (n >= 2) {
    # Crossing semantics: BUY when RSI[t-1] > oversold AND RSI[t] <= oversold
    #                     SELL when RSI[t-1] < overbought AND RSI[t] >= overbought
    prev_above_os <- v[-n] > oversold
    cur_at_or_below_os <- v[-1] <= oversold
    prev_below_ob <- v[-n] < overbought
    cur_at_or_above_ob <- v[-1] >= overbought
    
    # Both values must be defined
    valid_prev <- !is.na(v[-n])
    valid_cur <- !is.na(v[-1])
    ok <- valid_prev & valid_cur
    
    buy <- ok & prev_above_os & cur_at_or_below_os
    sell <- ok & prev_below_ob & cur_at_or_above_ob
    
    out[-1][buy] <- "BUY"
    out[-1][sell] <- "SELL"
  }
  
  xts::xts(out, order.by = index(rsi))
}

#' Buy & Hold signals: enter exactly once on the first candle, then hold.
#'
#' Research-experiment strategy (PAPER.md section 9). The signal vector follows
#' the SAME Phase 4/5 signal convention as the MA/RSI signals, so the existing
#' production engine (`execute_backtest`/`run_backtest`) executes it with its
#' established semantics:
#'   - A BUY generated at the close of the FIRST candle is executed at the OPEN
#'     of the SECOND candle (t+1, no look-ahead, never the same candle).
#'   - No SELL is ever generated: the position is held to the end of the
#'     period and marked to market (an OPEN position, never a completed
#'     BUY->SELL round trip).
#'   - The signal on the final available candle is never executed (engine
#'     rule); with a 1-row series the signal is NA because there is no t+1
#'     candle on which the BUY could be executed.
#'
#' @param prices xts single-column price series used only for its time index
#'   (indicator/close series aligned with the OHLCV being backtested).
#' @return xts of character signals: "BUY" on the first row (when >= 2 rows),
#'   NA everywhere else.
generate_buy_and_hold_signals <- function(prices) {
  stopifnot(inherits(prices, "xts"))
  n <- NROW(prices)
  out <- rep(NA_character_, n)
  if (n >= 2L) {
    out[1L] <- "BUY"   # executed by the engine at the OPEN of row 2
  }
  xts::xts(out, order.by = index(prices))
}