# AlphaForge Insight — supervised label generation (ML Phase A)
#
# Converts FUTURE returns into BUY/HOLD/SELL class labels. The future return
# is used ONLY as the training target — it is never a model input feature
# (features.R never touches data after time t).
#
# Default (ML plan section 1):
#   future_return = Close[t + horizon] / Close[t] - 1   (horizon = 5 days)
#   future_return >  +2%  -> BUY
#   future_return <  -2%  -> SELL
#   otherwise             -> HOLD
#
# Leakage rule (ML plan section 1 "critical rule"): generate_labels() returns
# the label vector and its VALIDITY MASK only — the future returns themselves
# are dropped so they can never be joined back onto the feature matrix as an
# input column. Rows whose label is undefined (final `horizon` rows) are
# excluded from training/evaluation by the mask.

library(xts)
suppressPackageStartupMessages(library(TTR))

VALID_CLASSES <- c("BUY", "HOLD", "SELL")

# Documented defaults (ML plan sections 1, 13).
LABEL_DEFAULTS <- list(horizon = 5L, buy_threshold = 0.02, sell_threshold = 0.02)

#' Validate label configuration.
#'
#' @param horizon Raw horizon (positive integer).
#' @param buy_threshold Raw BUY threshold (positive decimal, e.g. 0.02).
#' @param sell_threshold Raw SELL threshold (positive decimal magnitude).
#' @return list(horizon = integer, buy_threshold, sell_threshold).
#' @throws validation_error on any violation.
validate_label_config <- function(horizon = LABEL_DEFAULTS$horizon,
                                  buy_threshold = LABEL_DEFAULTS$buy_threshold,
                                  sell_threshold = LABEL_DEFAULTS$sell_threshold) {
  h <- suppressWarnings(as.numeric(horizon))
  if (length(h) != 1 || is.na(h) || !is.finite(h) ||
      h < 1 || abs(h - round(h)) > 1e-9) {
    stop(validation_error("horizon must be a positive integer (>= 1)."))
  }
  h <- as.integer(round(h))

  bt <- suppressWarnings(as.numeric(buy_threshold))
  st <- suppressWarnings(as.numeric(sell_threshold))
  if (length(bt) != 1 || is.na(bt) || !is.finite(bt) || bt <= 0) {
    stop(validation_error("buy_threshold must be a positive decimal (e.g. 0.02 = 2%)."))
  }
  if (length(st) != 1 || is.na(st) || !is.finite(st) || st <= 0) {
    stop(validation_error("sell_threshold must be a positive decimal (e.g. 0.02 = 2%)."))
  }
  list(horizon = h, buy_threshold = bt, sell_threshold = st)
}

#' Generate supervised BUY/HOLD/SELL labels from future returns.
#'
#' future_return[t] = Close[t + horizon] / Close[t] - 1.
#' BUY when future_return > +buy_threshold, SELL when < -sell_threshold,
#' otherwise HOLD. The final `horizon` rows have no future price yet: their
#' labels are NA and they are masked OUT of training (never fabricated).
#'
#' @param close Numeric vector of daily closes (chronological).
#' @param horizon Positive integer trading-day horizon (default 5).
#' @param buy_threshold Positive decimal (default 0.02).
#' @param sell_threshold Positive decimal (default 0.02).
#' @return list(labels = character vector (BUY/HOLD/SELL/NA),
#'              future_return only INSIDE this list and used solely to derive
#'              `labels` — it is returned as `future_return` for diagnostics
#'              but the training assembly in pipeline.R never joins it back
#'              onto model inputs; valid = logical mask, TRUE where the label
#'              is defined).
validate_and_generate_labels <- function(close, horizon = LABEL_DEFAULTS$horizon,
                                         buy_threshold = LABEL_DEFAULTS$buy_threshold,
                                         sell_threshold = LABEL_DEFAULTS$sell_threshold) {
  cfg <- validate_label_config(horizon, buy_threshold, sell_threshold)
  close <- as.numeric(close)
  n <- length(close)
  if (n < cfg$horizon + 2L) {
    stop(validation_error(sprintf(
      "At least %d rows of data are required for a %d-day label horizon.",
      cfg$horizon + 2, cfg$horizon
    )))
  }

  future_return <- rep(NA_real_, n)
  idx <- seq_len(n - cfg$horizon)
  future_return[idx] <- close[idx + cfg$horizon] / close[idx] - 1

  labels <- rep(NA_character_, n)
  defined <- !is.na(future_return)
  labels[defined & future_return > cfg$buy_threshold] <- "BUY"
  labels[defined & future_return < -cfg$sell_threshold] <- "SELL"
  labels[defined & is.na(labels)] <- "HOLD"

  list(
    labels = labels,
    valid = defined,
    horizon = cfg$horizon,
    buy_threshold = cfg$buy_threshold,
    sell_threshold = cfg$sell_threshold
  )
}
