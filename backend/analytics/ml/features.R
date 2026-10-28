# AlphaForge Insight — ML feature engineering (ML Phase A)
#
# Converts the EXISTING canonical OHLCV xts (Open/High/Low/Close/Adjusted/
# Volume, produced by analytics/data_retrieval.R::fetch_ohlcv) into a
# model-ready feature xts. Nothing here re-fetches data or re-implements the
# data layer; features are computed from the sanitized OHLCV the backtester
# itself uses, so ML and backtesting always see the same market data.
#
# Look-ahead convention (same as analytics/indicators.R):
#   - Every feature at time t uses ONLY data observed at or before t.
#     TTR indicators (SMA/EMA/RSI/MACD/Stochastic/ATR/BBands) are causal by
#     construction; returns use lagged prices (Close[t]/Close[t-k] - 1).
#   - Future information enters ONLY in labels.R (the target), never here.
#   - Warm-up NAs are PRESERVED (never zero-filled); downstream
#     preprocessing.R drops the shared warm-up rows explicitly.
#
# Feature groups (ML plan section 2):
#   returns   : daily_return, log_return, ret_5d, ret_10d, ret_20d,
#               price_change, gap_pct
#   trend     : sma_10, sma_20, sma_50, sma_200, ema_10, ema_20, ema_50,
#               close_sma20, close_sma50, ema20_ema50, sma20_sma50
#   momentum  : rsi_14, macd, macd_signal, macd_hist, roc_10, momentum_10,
#               stoch_k, stoch_d
#   volatility: atr_14, rolling_vol_20, bb_upper, bb_lower, bb_width
#   volume    : volume, volume_change, volume_ratio_20, obv

library(xts)
library(zoo)
suppressPackageStartupMessages(library(TTR))

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

#' Feature-group registry: the canonical feature names per group.
#' `features_to_matrix` filters on these; the UI's feature checkboxes map to
#' the group names via FEATURE_GROUP_ALIASES.
FEATURE_GROUPS <- list(
  returns    = c("daily_return", "log_return", "ret_5d", "ret_10d",
                 "ret_20d", "price_change", "gap_pct"),
  trend      = c("sma_10", "sma_20", "sma_50", "sma_200",
                 "ema_10", "ema_20", "ema_50",
                 "close_sma20", "close_sma50", "ema20_ema50", "sma20_sma50"),
  momentum   = c("rsi_14", "macd", "macd_signal", "macd_hist",
                 "roc_10", "momentum_10", "stoch_k", "stoch_d"),
  volatility = c("atr_14", "rolling_vol_20", "bb_upper", "bb_lower", "bb_width"),
  volume     = c("volume", "volume_change", "volume_ratio_20", "obv"),
  regime     = c("regime_vol_20", "regime_trend_50", "regime_drawdown",
                 "regime_momentum_20", "regime_class")
)

# UI-facing aliases (ML plan section 15 checkboxes) -> feature groups.
FEATURE_GROUP_ALIASES <- list(
  returns    = "returns",
  price      = "returns",
  trend      = "trend",
  sma        = "trend",
  ema        = "trend",
  rsi        = "momentum",
  macd       = "momentum",
  momentum   = "momentum",
  atr        = "volatility",
  volatility = "volatility",
  bollinger  = "volatility",
  volume     = "volume",
  regime     = "regime"
)

#' Resolve a UI feature-selection vector to canonical group names.
#'
#' Accepts group names ("rsi"), aliases ("bollinger"), "all"/NULL. Unknown
#' names raise a validation_error so clients see a 400, not silent drops.
#'
#' @param groups Character vector of group/alias names (NULL = all).
#' @return Canonical group names (unique, in FEATURE_GROUPS order).
resolve_feature_groups <- function(groups) {
  if (is.null(groups) || length(groups) == 0) {
    return(names(FEATURE_GROUPS))
  }
  groups <- unique(trimws(as.character(groups)))
  groups <- groups[nzchar(groups)]
  if (length(groups) == 1 && tolower(groups) %in% c("all", "*")) {
    return(names(FEATURE_GROUPS))
  }
  canonical <- FEATURE_GROUP_ALIASES[tolower(groups)]
  if (any(vapply(canonical, is.null, logical(1)))) {
    bad <- groups[is.na(canonical) | vapply(canonical, is.null, logical(1))]
    stop(validation_error(sprintf(
      "Unknown feature group(s): %s. Available: %s.",
      paste(bad, collapse = ", "),
      paste(c(names(FEATURE_GROUPS), names(FEATURE_GROUP_ALIASES)), collapse = ", ")
    )))
  }
  out <- unlist(canonical, use.names = FALSE)
  names(FEATURE_GROUPS)[names(FEATURE_GROUPS) %in% out]
}

# ---------------------------------------------------------------------------
# Builders (each returns a named numeric list aligned to the OHLCV index)
# ---------------------------------------------------------------------------

.build_return_features <- function(ohlcv) {
  close <- as.numeric(ohlcv[, "Close"])
  open  <- as.numeric(ohlcv[, "Open"])
  n <- length(close)
  pct_lag <- function(k) {
    out <- rep(NA_real_, n)
    if (n > k) out[(k + 1):n] <- close[(k + 1):n] / close[1:(n - k)] - 1
    out
  }
  list(
    daily_return = c(NA_real_, if (n > 1) close[-1] / close[-n] - 1 else NULL),
    log_return   = c(NA_real_, if (n > 1) log(close[-1] / close[-n]) else NULL),
    ret_5d       = pct_lag(5),
    ret_10d      = pct_lag(10),
    ret_20d      = pct_lag(20),
    price_change = c(NA_real_, if (n > 1) close[-1] - close[-n] else NULL),
    gap_pct      = c(NA_real_, if (n > 1) open[-1] / close[-n] - 1 else NULL)
  )
}

.build_trend_features <- function(ohlcv) {
  close <- ohlcv[, "Close"]
  n <- NROW(close)
  na_vec <- rep(NA_real_, n)
  sma_or_na <- function(p) {
    if (n < p) na_vec else as.numeric(TTR::SMA(close, n = p)[, 1])
  }
  ema_or_na <- function(p) {
    if (n < p) na_vec else as.numeric(TTR::EMA(close, n = p)[, 1])
  }
  sma10 <- sma_or_na(10); sma20 <- sma_or_na(20); sma50 <- sma_or_na(50)
  ema20 <- ema_or_na(20); ema50 <- ema_or_na(50)
  close_v <- as.numeric(close)
  ratio <- function(a, b) ifelse(is.na(a) | is.na(b) | b == 0, NA_real_, a / b)
  list(
    sma_10       = sma10,
    sma_20       = sma20,
    sma_50       = sma50,
    sma_200      = sma_or_na(200),
    ema_10       = ema_or_na(10),
    ema_20       = ema20,
    ema_50       = ema50,
    close_sma20  = ratio(close_v, sma20),
    close_sma50  = ratio(close_v, sma50),
    ema20_ema50  = ratio(ema20, ema50),
    sma20_sma50  = ratio(sma20, sma50)
  )
}

.build_momentum_features <- function(ohlcv) {
  close <- ohlcv[, "Close"]
  high  <- ohlcv[, "High"]
  low   <- ohlcv[, "Low"]
  n <- NROW(close)
  na_vec <- rep(NA_real_, n)
  rsi <- if (n < 14) na_vec else as.numeric(TTR::RSI(close, n = 14)[, 1])
  macd_raw <- if (n < 26) NULL else TTR::MACD(close, nFast = 12, nSlow = 26,
                                              nSig = 9, percent = FALSE)
  if (is.null(macd_raw)) {
    macd <- macd_signal <- na_vec
  } else {
    macd <- as.numeric(macd_raw[, 1]); macd_signal <- as.numeric(macd_raw[, 2])
  }
  roc <- if (n < 11) na_vec else as.numeric(TTR::ROC(close, n = 10)[, 1])
  mom <- if (n < 11) na_vec else as.numeric(TTR::momentum(close, n = 10)[, 1])
  stoch_raw <- if (n < 14) NULL else {
    TTR::stoch(ohlcv[, c("High", "Low", "Close")], nFastK = 14,
               nFastD = 3, nSlowD = 3)
  }
  if (is.null(stoch_raw)) {
    stoch_k <- stoch_d <- na_vec
  } else {
    stoch_k <- as.numeric(stoch_raw[, "fastK"])
    stoch_d <- as.numeric(stoch_raw[, "fastD"])
  }
  list(
    rsi_14      = rsi,
    macd        = macd,
    macd_signal = macd_signal,
    macd_hist   = macd - macd_signal,
    roc_10      = roc,
    momentum_10 = mom,
    stoch_k     = stoch_k,
    stoch_d     = stoch_d
  )
}

.build_volatility_features <- function(ohlcv) {
  close <- ohlcv[, "Close"]
  high  <- ohlcv[, "High"]
  low   <- ohlcv[, "Low"]
  n <- NROW(close)
  na_vec <- rep(NA_real_, n)
  hlc <- ohlcv[, c("High", "Low", "Close")]
  atr <- if (n < 14) na_vec else as.numeric(TTR::ATR(hlc, n = 14)$atr)
  ret <- TTR::ROC(close, n = 1)
  vol20 <- if (n < 21) na_vec else {
    as.numeric(zoo::rollapplyr(as.numeric(ret), width = 20, FUN = stats::sd,
                               fill = NA, partial = FALSE)) * sqrt(252)
  }
  bb <- if (n < 20) NULL else TTR::BBands(ohlcv[, c("High", "Low", "Close")],
                                          n = 20, sd = 2)
  if (is.null(bb)) {
    bb_upper <- bb_lower <- na_vec
  } else {
    bb_upper <- as.numeric(bb[, "up"]); bb_lower <- as.numeric(bb[, "dn"])
  }
  bb_width <- ifelse(is.na(bb_upper) | is.na(bb_lower) |
                       is.na(as.numeric(close)) | as.numeric(close) == 0,
                     NA_real_, (bb_upper - bb_lower) / as.numeric(close))
  list(
    atr_14         = atr,
    rolling_vol_20 = vol20,
    bb_upper       = bb_upper,
    bb_lower       = bb_lower,
    bb_width       = bb_width
  )
}

.build_volume_features <- function(ohlcv) {
  volume <- as.numeric(ohlcv[, "Volume"])
  close  <- as.numeric(ohlcv[, "Close"])
  n <- length(volume)
  avg_vol <- if (n < 21) rep(NA_real_, n) else {
    zoo::rollapplyr(volume, width = 20, FUN = mean, fill = NA, partial = FALSE)
  }
  obv <- if (n < 2) rep(NA_real_, n) else {
    sign_v <- sign(c(0, diff(close)))
    cumsum(volume * sign_v)
  }
  list(
    volume           = volume,
    volume_change    = c(NA_real_, if (n > 1) volume[-1] / pmax(volume[-n], 1) - 1 else NULL),
    volume_ratio_20  = ifelse(is.na(avg_vol) | avg_vol == 0, NA_real_, volume / avg_vol),
    obv              = obv
  )
}

.build_regime_features <- function(ohlcv) {
  close <- ohlcv[, "Close"]
  n <- NROW(close)
  na_vec <- rep(NA_real_, n)
  close_v <- as.numeric(close)
  
  ret <- TTR::ROC(close, n = 1)
  vol20 <- if (n < 21) na_vec else {
    as.numeric(zoo::rollapplyr(as.numeric(ret), width = 20, FUN = stats::sd,
                               fill = NA, partial = FALSE)) * sqrt(252)
  }
  
  sma50 <- if (n < 50) na_vec else as.numeric(TTR::SMA(close, n = 50)[, 1])
  trend_50 <- ifelse(is.na(sma50) | sma50 == 0, NA_real_, close_v / sma50 - 1)
  
  rolling_max <- if (n < 50) na_vec else {
    zoo::rollapplyr(close_v, width = 50, FUN = max, fill = NA, partial = FALSE)
  }
  drawdown <- ifelse(is.na(rolling_max) | rolling_max == 0, NA_real_,
                     close_v / rolling_max - 1)
  
  momentum_20 <- rep(NA_real_, n)
  if (n > 20) {
    momentum_20[21:n] <- close_v[21:n] / close_v[1:(n - 20)] - 1
  }
  
  vol_median <- stats::median(vol20, na.rm = TRUE)
  regime_class <- ifelse(is.na(vol20), NA_real_,
                         ifelse(vol20 < vol_median, 0, 1))
  
  list(
    regime_vol_20      = vol20,
    regime_trend_50    = trend_50,
    regime_drawdown    = drawdown,
    regime_momentum_20 = momentum_20,
    regime_class       = regime_class
  )
}

BUILDERS <- list(
  returns    = .build_return_features,
  trend      = .build_trend_features,
  momentum   = .build_momentum_features,
  volatility = .build_volatility_features,
  volume     = .build_volume_features,
  regime     = .build_regime_features
)

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Build the ML feature xts from a canonical OHLCV series.
#'
#' @param ohlcv Canonical xts (Open/High/Low/Close/Adjusted/Volume) from
#'   fetch_ohlcv(). Must have >= 2 rows.
#' @param groups Character vector of feature groups (NULL = all groups).
#' @return A named list:
#'   - `features`: xts of feature columns (index identical to ohlcv; warm-up
#'     rows carry NA by design)
#'   - `groups`:   the resolved group names
#'   - `feature_names`: column names of `features`
#' @throws validation_error on degenerate input or unknown groups.
build_features <- function(ohlcv, groups = NULL) {
  if (!inherits(ohlcv, "xts")) {
    stop(validation_error("features require an xts OHLCV object."))
  }
  if (NROW(ohlcv) < 2) {
    stop(validation_error(
      "At least 2 rows of market data are required to compute features."
    ))
  }
  resolved <- resolve_feature_groups(groups)

  columns <- list()
  for (g in resolved) {
    built <- BUILDERS[[g]](ohlcv)
    for (nm in names(built)) columns[[nm]] <- built[[nm]]
  }

  feature_names <- names(columns)
  mat <- do.call(cbind, lapply(columns, function(v) as.numeric(v)))
  colnames(mat) <- feature_names
  features <- xts::xts(mat, order.by = zoo::index(ohlcv))

  list(
    features = features,
    groups = resolved,
    feature_names = feature_names
  )
}

#' Convert a feature xts to a data.frame keyed by date.
#'
#' Convenience for tests and for assembling multi-ticker panels.
#'
#' @param features xts from build_features().
#' @return data.frame with a `date` column plus one column per feature.
features_to_df <- function(features) {
  df <- as.data.frame(as.matrix(features))
  df$date <- as.Date(zoo::index(features))
  rownames(df) <- NULL
  df[, c("date", setdiff(names(df), "date")), drop = FALSE]
}
