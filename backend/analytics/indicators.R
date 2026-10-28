# AlphaForge Insight — technical indicators (Phase 3)
#
# Generic indicator calculations used by the strategy layer (Phase 4).
# Parameters are NOT hard-coded here: callers decide periods and thresholds.
#
#   calculate_sma(prices, period)   Simple moving average (TTR::SMA)
#   calculate_rsi(prices, period)   Relative Strength Index (TTR::RSI)
#
# Conventions (documented for reproducibility):
#   - Input is a single numeric price series, typically as xts (daily Close or
#     Adjusted Close). A plain numeric vector is also accepted; in that case a
#     synthetic time index is attached so downstream `xts` alignment rules hold.
#   - At time t only data observed at or before t is used (no look-ahead).
#     TTR's SMA and RSI are causal by construction.
#   - Warm-up: the first (period - 1) SMA values and the first (period) RSI
#     values are NA by design. NAs are PRESERVED — indicator values are never
#     silently replaced with zeros. Downstream logic (signal generation,
#     backtesting) must treat NA as "no value yet".
#   - RSI convention: TTR::RSI default smoothing, i.e. Wilder's RSI (RMA /
#     Wilder smoothing with alpha = 1/period on average gains and losses).
#     RSI is bounded to [0, 100] by TTR. This differs slightly from some
#     simple (non-smoothed) RSI formulations; tests pin the exact values.
#   - Timestamps: the returned object keeps the exact index of the input,
#     so indicator dates always align with the source price data.

library(xts)      # xts/zoo time series (is.xts, is.zoo, as.xts)
suppressPackageStartupMessages(library(zoo))

#' Validate a common indicator period argument.
#' @param period The user-supplied period.
#' @param name   Label used in error messages (default "period").
validate_period <- function(period, name = "period") {
  if (length(period) != 1 || is.na(period) || !is.numeric(period)) {
    stop("Invalid ", name, ": must be a single number.")
  }
  if (period < 1 || abs(period - round(period)) > 1e-9) {
    stop("Invalid ", name, ": must be a positive integer (>= 1).")
  }
  as.integer(round(period))
}

#' Coerce a price series to xts for indicator math.
#' @param prices Numeric vector or xts/zoo of a single price series.
#' @return An xts object with attribute `.alf_indexed` set when the input was
#'   a plain vector (so we can strip the synthetic index in output, if needed).
as_price_xts <- function(prices) {
  if (is.xts(prices)) {
    if (NCOL(prices) != 1) {
      stop("Indicator input must be a single price series (one column).")
    }
    return(prices)
  }
  if (is.zoo(prices)) {
    prices <- as.xts(prices)
    if (NCOL(prices) != 1) {
      stop("Indicator input must be a single price series (one column).")
    }
    return(prices)
  }
  if (is.numeric(prices) && is.null(dim(prices))) {
    n <- length(prices)
    if (n == 0) stop("Indicator input is empty.")
    idx <- as.Date("2000-01-01") + seq_len(n) - 1L
    out <- xts::as.xts(prices, order.by = idx)
    attr(out, ".alf_synthetic_index") <- TRUE
    return(out)
  }
  stop("Indicator input must be a numeric vector or an xts/zoo object.")
}

#' Simple moving average.
#'
#' @param prices Numeric vector or single-column xts of prices.
#' @param period Positive integer window length (e.g. 50 or 200).
#' @return xts of SMA aligned with `prices`; leading (period - 1) values NA.
#'   When the series is shorter than `period` (insufficient warm-up), an
#'   all-NA xts is returned aligned with the input — never an error.
calculate_sma <- function(prices, period) {
  period <- validate_period(period, "period")
  x <- as_price_xts(prices)
  if (NROW(x) < period) {
    return(xts::xts(rep(NA_real_, NROW(x)), order.by = index(x)))
  }
  sma <- TTR::SMA(x[, 1], n = period)
  # TTR::SMA preserves the index; keep only the values column.
  sma[, 1]
}

#' Relative Strength Index (Wilder, via TTR::RSI).
#'
#' @param prices Numeric vector or single-column xts of prices.
#' @param period Positive integer RSI look-back (default 14 in the strategy
#'   layer; not enforced here).
#' @return xts of RSI in [0, 100] aligned with `prices`; leading `period`
#'   values are NA (warm-up). If the series is shorter than `period`, an
#'   all-NA xts is returned aligned with the input. A perfectly flat series
#'   yields NA (no up/down movement → RSI is undefined by Wilder's method).
calculate_rsi <- function(prices, period) {
  period <- validate_period(period, "period")
  x <- as_price_xts(prices)
  if (NROW(x) < period) {
    return(xts::xts(rep(NA_real_, NROW(x)), order.by = index(x)))
  }
  rsi <- TTR::RSI(x[, 1], n = period)
  rsi[, 1]
}