# AlphaForge Insight — Execution-aligned label generation (Phase G.3)
#
# Extends labels.R with Open-to-Open labels that align with execute_backtest()
# execution mechanics. Preserves the original Close-to-Close implementation.
#
# Execution flow (execution.R lines 234-310):
#   - Signal generated at Close[t]
#   - Executed at Open[t+1]
#   - Holding period return: (Open[t+k] - Open[t+1]) / Open[t+1]
#
# Correct label alignment for signal at Close[t]:
#   - Prediction happens at Close[t] (features use data up to Close[t])
#   - Execution at Open[t+1]
#   - Next-day execution at Open[t+2]
#   - Label: Open[t+2] / Open[t+1] - 1
#
# This is the ACTUAL return earned if:
#   - BUY signal at Close[t] executes at Open[t+1]
#   - SELL signal at Close[t+1] executes at Open[t+2]

library(xts)

# Define validation_error if not already defined
if (!exists("validation_error")) {
  validation_error <- function(msg) {
    structure(list(message = msg), class = c("validation_error", "error", "condition"))
  }
}

# Source dependencies if not already loaded
if (!exists("validate_label_config")) {
  source("backend/analytics/ml/labels.R")
}
if (!exists("VALID_CLASSES")) {
  VALID_CLASSES <- c("BUY", "HOLD", "SELL")
}

#' Generate execution-aligned Open-to-Open labels.
#'
#' For a signal at Close[t] that executes at Open[t+1], the target is the
#' return earned from Open[t+1] to Open[t+2] (next-day open-to-open).
#'
#' Label alignment:
#'   - Features at row t use data up to Close[t]
#'   - Signal generated at Close[t]
#'   - Executed at Open[t+1]
#'   - Holding period: Open[t+1] to Open[t+horizon+1]
#'   - Target: Open[t+horizon+1] / Open[t+1] - 1
#'
#' @param open Numeric vector of daily opens (chronological).
#' @param horizon Positive integer trading-day horizon (default 1 for next-day).
#' @param buy_threshold Positive decimal (default 0.02).
#' @param sell_threshold Positive decimal (default 0.02).
#' @return list(labels, valid, horizon, buy_threshold, sell_threshold,
#'              label_type = "open_to_open", execution_aligned = TRUE).
generate_open_to_open_labels <- function(open,
                                         horizon = 1L,
                                         buy_threshold = 0.02,
                                         sell_threshold = 0.02) {
  cfg <- validate_label_config(horizon, buy_threshold, sell_threshold)
  open <- as.numeric(open)
  n <- length(open)
  
  if (n < cfg$horizon + 2L) {
    stop(validation_error(sprintf(
      "At least %d rows required for horizon=%d open-to-open labels.",
      cfg$horizon + 2, cfg$horizon
    )))
  }
  
  # For signal at Close[t], execution at Open[t+1], target is Open[t+1+h]/Open[t+1] - 1
  # 
  # Example with horizon=1:
  # Row 1: Close[1], Open[1] = signal generated at Close[1]
  #        -> executes at Open[2] (row 2)
  #        -> exits at Open[3] (row 3)
  #        -> target = Open[3]/Open[2] - 1
  # So for row i, we need Open[i+1] and Open[i+1+horizon]
  # This requires i+1+horizon <= n, so i <= n-1-horizon
  
  future_return <- rep(NA_real_, n)
  
  # Valid indices: rows where we can compute the target
  # Need Open[i+1] and Open[i+1+horizon] to exist
  # Maximum i: n - 1 - horizon
  max_idx <- n - 1L - cfg$horizon
  if (max_idx >= 1L) {
    idx <- seq_len(max_idx)
    future_return[idx] <- open[idx + 1L + cfg$horizon] / open[idx + 1L] - 1
  }
  
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
    sell_threshold = cfg$sell_threshold,
    label_type = "open_to_open",
    execution_aligned = TRUE
  )
}

#' Generate Close-to-Close labels (original implementation).
#'
#' Wrapper around validate_and_generate_labels() for consistency.
#'
#' @param close Numeric vector of daily closes.
#' @param horizon Positive integer horizon.
#' @param buy_threshold Positive decimal.
#' @param sell_threshold Positive decimal.
#' @return list(labels, valid, ..., label_type = "close_to_close").
generate_close_to_close_labels <- function(close,
                                           horizon = 5L,
                                           buy_threshold = 0.02,
                                           sell_threshold = 0.02) {
  result <- validate_and_generate_labels(close, horizon, buy_threshold, sell_threshold)
  result$label_type <- "close_to_close"
  result$execution_aligned <- FALSE
  result
}

#' Compute label alignment statistics.
#'
#' Measures disagreement between Close-to-Close and Open-to-Open labels
#' to quantify the target/execution mismatch.
#'
#' @param close Numeric vector of daily closes.
#' @param open Numeric vector of daily opens (same length as close).
#' @param horizon Label horizon.
#' @param buy_threshold BUY threshold.
#' @param sell_threshold SELL threshold.
#' @return list(n_valid, n_agree, n_disagree, pct_disagree, confusion_matrix).
compute_label_alignment <- function(close, open, horizon = 1L,
                                    buy_threshold = 0.02, sell_threshold = 0.02) {
  ctc <- generate_close_to_close_labels(close, horizon, buy_threshold, sell_threshold)
  oto <- generate_open_to_open_labels(open, horizon, buy_threshold, sell_threshold)
  
  # Valid rows: both labels defined
  both_valid <- ctc$valid & oto$valid
  n_valid <- sum(both_valid)
  
  if (n_valid == 0) {
    return(list(n_valid = 0, n_agree = 0, n_disagree = 0, pct_disagree = NA_real_,
                confusion_matrix = NULL))
  }
  
  ctc_sub <- ctc$labels[both_valid]
  oto_sub <- oto$labels[both_valid]
  
  agree <- ctc_sub == oto_sub
  n_agree <- sum(agree)
  n_disagree <- n_valid - n_agree
  pct_disagree <- 100 * n_disagree / n_valid
  
  # Confusion matrix: rows = CtC, cols = OtO
  confusion <- table(CtC = factor(ctc_sub, levels = VALID_CLASSES),
                     OtO = factor(oto_sub, levels = VALID_CLASSES))
  
  list(
    n_valid = n_valid,
    n_agree = n_agree,
    n_disagree = n_disagree,
    pct_disagree = pct_disagree,
    confusion_matrix = confusion
  )
}
