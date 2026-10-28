# AlphaForge Insight — Regression target generation (Phase H)
#
# Generates continuous forward-return targets for time-series forecasting.
# This is a SEPARATE module from the classification labels (labels.R) to avoid
# any risk of destabilizing the existing ML classification pipeline.
#
# Target: forward_return[t] = Close[t + horizon] / Close[t] - 1
#
# Leakage prevention:
#   - The forward return is computed ONLY for the target variable.
#   - It is NEVER attached to feature matrices.
#   - The final `horizon` rows have NA targets (no future price available).
#
# Alignment convention:
#   - target[t] represents the return from Close[t] to Close[t + horizon].
#   - When used for trading: a prediction made at time t (using features up to t)
#     forecasts the return realized at Close[t + horizon].
#   - Execution can only occur at Open[t+1] or later — there is a 1-day gap
#     between feature observation and trade execution.

library(xts)

# Default horizon matches the classification label convention (5 trading days).
REGRESSION_HORIZON_DEFAULT <- 5L

#' Generate continuous forward-return targets.
#'
#' @param close Numeric vector of daily close prices (chronological).
#' @param horizon Number of trading days forward (default 5).
#' @return list(targets = numeric vector (NA for last `horizon` rows),
#'              valid = logical mask (TRUE where target is defined),
#'              horizon = integer).
generate_forward_return_target <- function(close, horizon = REGRESSION_HORIZON_DEFAULT) {
  close <- as.numeric(close)
  n <- length(close)
  horizon <- as.integer(horizon)
  
  if (is.na(horizon) || horizon < 1) {
    stop("horizon must be a positive integer.")
  }
  
  if (n < horizon + 2L) {
    stop(sprintf(
      "At least %d rows are required for a %d-day horizon.",
      horizon + 2, horizon
    ))
  }
  
  targets <- rep(NA_real_, n)
  idx <- seq_len(n - horizon)
  targets[idx] <- close[idx + horizon] / close[idx] - 1
  
  list(
    targets = targets,
    valid = !is.na(targets),
    horizon = horizon
  )
}

#' Validate regression target configuration.
#'
#' @param horizon Number of trading days forward.
#' @return list(horizon = integer).
validate_regression_config <- function(horizon = REGRESSION_HORIZON_DEFAULT) {
  h <- suppressWarnings(as.integer(horizon))
  if (is.na(h) || h < 1) {
    stop("horizon must be a positive integer.")
  }
  list(horizon = h)
}

#' Assemble regression dataset for one ticker.
#'
#' Same structure as assemble_ml_dataset() but with continuous targets.
#' Features are computed from the same OHLCV; targets are forward returns.
#'
#' @param ohlcv Canonical OHLCV xts.
#' @param ticker Ticker symbol.
#' @param horizon Forward return horizon (trading days).
#' @param groups Feature groups (NULL = all).
#' @param split_fractions Named list(train, validation, test).
#' @return list(ohlcv, ticker, features_xts, dates, X_raw, y (numeric targets),
#'   split, n, p, feature_names, horizon).
assemble_regression_dataset <- function(ohlcv = NULL, ticker, start, end,
                                        horizon = REGRESSION_HORIZON_DEFAULT,
                                        groups = NULL,
                                        split_fractions = ML_SPLIT_DEFAULTS) {
  # Source dependencies
  ml_dir <- get("ml_dir", envir = parent.frame())
  if (is.null(ml_dir)) {
    # Fallback: determine from call stack
    ml_dir <- file.path(dirname(sys.frame(1)$ofile %||% "."), "backend", "analytics", "ml")
    if (!file.exists(file.path(ml_dir, "features.R"))) {
      ml_dir <- file.path(getwd(), "backend", "analytics", "ml")
    }
  }
  source(file.path(ml_dir, "features.R"), local = TRUE)
  source(file.path(ml_dir, "preprocessing.R"), local = TRUE)
  
  if (is.null(ohlcv)) {
    source(file.path(dirname(ml_dir), "data_retrieval.R"), local = TRUE)
    ohlcv <- fetch_ohlcv(ticker, start, end)
  }
  
  feats <- build_features(ohlcv, groups)
  targets <- generate_forward_return_target(as.numeric(ohlcv[, "Close"]), horizon)
  
  X_raw <- feats$features
  y <- targets$targets
  valid <- targets$valid & !is.na(y)
  
  # Drop warm-up rows with mostly-NA features (same threshold as classification).
  non_na_frac <- 1 - rowSums(is.na(as.matrix(X_raw))) / ncol(X_raw)
  keep <- valid & (non_na_frac >= 0.7)
  
  if (sum(keep) < 10) {
    stop(sprintf(
      "Only %d usable rows for %s — widen the date range.",
      sum(keep), ticker
    ))
  }
  
  X_raw <- X_raw[keep, ]
  y <- y[keep]
  dates <- as.Date(zoo::index(X_raw))
  
  # Embargo prevents target overlap between splits.
  split <- chronological_split(nrow(X_raw), split_fractions, embargo = horizon)
  
  list(
    ohlcv = ohlcv,
    ticker = ticker,
    features_xts = feats$features,
    dates = dates,
    X_raw = as.matrix(X_raw),
    y = y,
    split = split,
    n = nrow(X_raw),
    p = ncol(X_raw),
    feature_names = feats$feature_names,
    horizon = horizon
  )
}
