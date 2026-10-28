# AlphaForge Insight — Formulaic Alpha Features (Phase H.6)
#
# Implements WorldQuant-inspired formulaic alpha signals for time-series forecasting.
# All features are causally correct: use ONLY information available at or before time t.
#
# Feature families:
#   A. Momentum and Trend (5 features)
#   B. Mean Reversion (4 features)
#   C. Volume and Price Pressure (4 features)
#   D. Volatility and Risk (3 features)
#   E. Formulaic Alpha Transformations (2 features)
#
# Total: 18 candidate alpha signals

library(xts)
library(zoo)

# =============================================================================
# FEATURE GROUP REGISTRY
# =============================================================================

FORMULAIC_ALPHA_GROUPS <- list(
  momentum = c("multi_horizon_disagreement", "price_ma_ratio", "trend_slope",
               "momentum_vol_ratio", "ema_crossover"),
  reversion = c("reversion_5d", "return_zscore", "close_location_value",
                "large_move_reversion"),
  volume = c("volume_ratio", "volume_pressure", "obv_change", "intraday_intensity"),
  volatility = c("volatility_expansion", "downside_volatility", "vol_scaled_momentum"),
  formulaic = c("ts_rank_return", "decay_weighted_return")
)

FORMULAIC_ALPHA_NAMES <- unlist(FORMULAIC_ALPHA_GROUPS, use.names = FALSE)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Safe division with epsilon to avoid division by zero
safe_div <- function(num, den, eps = 1e-8) {
  ifelse(abs(den) < eps, num / eps, num / den)
}

#' Rolling mean with NA handling
rolling_mean <- function(x, n) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  zoo::rollapplyr(x, width = n, FUN = mean, fill = NA, partial = FALSE)
}

#' Rolling standard deviation with NA handling
rolling_sd <- function(x, n) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  zoo::rollapplyr(x, width = n, FUN = sd, fill = NA, partial = FALSE)
}

#' Rolling rank (time-series rank)
rolling_rank <- function(x, n) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  zoo::rollapplyr(x, width = n, FUN = function(v) rank(v)[n], fill = NA)
}

# =============================================================================
# FAMILY A: MOMENTUM AND TREND
# =============================================================================

#' A1. Multi-Horizon Return Disagreement
#' Measures disagreement between short and medium-term returns
build_multi_horizon_disagreement <- function(close) {
  n <- length(close)
  
  # Compute returns at different horizons
  ret_1d <- c(NA_real_, diff(close) / head(close, -1))
  ret_5d <- if (n > 5) c(rep(NA_real_, 5), close[6:n] / close[1:(n-5)] - 1) else rep(NA_real_, n)
  ret_10d <- if (n > 10) c(rep(NA_real_, 10), close[11:n] / close[1:(n-10)] - 1) else rep(NA_real_, n)
  ret_20d <- if (n > 20) c(rep(NA_real_, 20), close[21:n] / close[1:(n-20)] - 1) else rep(NA_real_, n)
  
  # Compute row-wise standard deviation
  ret_mat <- cbind(ret_1d, ret_5d, ret_10d, ret_20d)
  apply(ret_mat, 1, sd, na.rm = FALSE)
}

#' A2. Price Relative to Moving Average
build_price_ma_ratio <- function(close, ma_period = 50) {
  n <- length(close)
  if (n < ma_period) return(rep(NA_real_, n))
  
  sma <- TTR::SMA(close, n = ma_period)
  as.numeric(close / sma)
}

#' A3. Trend Strength (Linear Regression Slope)
build_trend_slope <- function(close, lookback = 20) {
  n <- length(close)
  if (n < lookback) return(rep(NA_real_, n))
  
  slope <- zoo::rollapplyr(close, width = lookback, FUN = function(v) {
    x <- seq_along(v)
    coef(lm(v ~ x))[2]  # Slope coefficient
  }, fill = NA, partial = FALSE)
  
  as.numeric(slope)
}

#' A4. Momentum-to-Volatility Ratio
build_momentum_vol_ratio <- function(close, mom_period = 20, vol_period = 20) {
  n <- length(close)
  max_period <- max(mom_period, vol_period)
  if (n < max_period) return(rep(NA_real_, n))
  
  # Momentum: N-day return
  momentum <- c(rep(NA_real_, mom_period), close[(mom_period+1):n] / close[1:(n-mom_period)] - 1)
  
  # Volatility: Rolling SD of daily returns
  ret <- c(NA_real_, diff(close) / head(close, -1))
  vol <- rolling_sd(ret, vol_period) * sqrt(252)
  
  safe_div(momentum, vol)
}

#' A5. Exponential Moving Average Crossover
build_ema_crossover <- function(close, short = 10, long = 30) {
  n <- length(close)
  if (n < long) return(rep(NA_real_, n))
  
  ema_short <- as.numeric(TTR::EMA(close, n = short))
  ema_long <- as.numeric(TTR::EMA(close, n = long))
  
  safe_div(ema_short, ema_long) - 1
}

# =============================================================================
# FAMILY B: MEAN REVERSION
# =============================================================================

#' B1. Short-Horizon Reversion Signal
build_reversion_5d <- function(close) {
  n <- length(close)
  if (n < 6) return(rep(NA_real_, n))
  
  ret_5d <- c(rep(NA_real_, 5), close[6:n] / close[1:(n-5)] - 1)
  -ret_5d  # Negative for reversion
}

#' B2. Return Z-Score
build_return_zscore <- function(close, ret_period = 5, lookback = 60) {
  n <- length(close)
  if (n < lookback + ret_period) return(rep(NA_real_, n))
  
  # Compute 5-day returns
  ret <- c(rep(NA_real_, ret_period), close[(ret_period+1):n] / close[1:(n-ret_period)] - 1)
  
  # Rolling mean and SD
  ret_mean <- rolling_mean(ret, lookback)
  ret_sd <- rolling_sd(ret, lookback)
  
  safe_div(ret - ret_mean, ret_sd)
}

#' B3. Close Location Value
build_close_location_value <- function(high, low, close) {
  # Where does close fall within the day's range?
  # Ranges from -0.5 (close at low) to +0.5 (close at high)
  range_val <- high - low
  safe_div(close - low, range_val) - 0.5
}

#' B4. Large-Move Reversion
build_large_move_reversion <- function(close, vol_period = 20, threshold_mult = 2) {
  n <- length(close)
  if (n < vol_period + 1) return(rep(NA_real_, n))
  
  # Daily returns
  ret <- c(NA_real_, diff(close) / head(close, -1))
  
  # Rolling volatility
  vol <- rolling_sd(ret, vol_period)
  
  # Large move indicator
  large_move <- abs(ret) > threshold_mult * vol
  
  # Reversion signal: negative sign of large moves
  ifelse(large_move, -sign(ret), 0)
}

# =============================================================================
# FAMILY C: VOLUME AND PRICE PRESSURE
# =============================================================================

#' C1. Volume Ratio
build_volume_ratio <- function(volume, lookback = 20) {
  avg_vol <- rolling_mean(volume, lookback)
  safe_div(volume, avg_vol)
}

#' C2. Volume-Weighted Price Pressure
build_volume_pressure <- function(close, volume, lookback = 20) {
  n <- length(close)
  if (n < lookback + 1) return(rep(NA_real_, n))
  
  # Daily returns
  ret <- c(NA_real_, diff(close) / head(close, -1))
  
  # Volume ratio
  vol_ratio <- build_volume_ratio(volume, lookback)
  
  # Price change weighted by log volume ratio
  ret * log(1 + pmax(vol_ratio - 1, 0))
}

#' C3. On-Balance Volume Change
build_obv_change <- function(close, volume, lookback = 5) {
  n <- length(close)
  if (n < lookback + 1) return(rep(NA_real_, n))
  
  # OBV
  sign_v <- sign(c(0, diff(close)))
  obv <- cumsum(volume * sign_v)
  
  # 5-day change
  obv_lag <- c(rep(NA_real_, lookback), head(obv, -lookback))
  safe_div(obv - obv_lag, abs(obv))
}

#' C4. Intraday Intensity
build_intraday_intensity <- function(high, low, close, volume) {
  # Close position relative to high/low, scaled by volume
  safe_div(2 * close - high - low, volume) * 1e6
}

# =============================================================================
# FAMILY D: VOLATILITY AND RISK
# =============================================================================

#' D1. Volatility Expansion
build_volatility_expansion <- function(close, short = 5, long = 20) {
  n <- length(close)
  if (n < long + 1) return(rep(NA_real_, n))
  
  # Daily returns
  ret <- c(NA_real_, diff(close) / head(close, -1))
  
  # Short and long-term volatility
  vol_short <- rolling_sd(ret, short)
  vol_long <- rolling_sd(ret, long)
  
  safe_div(vol_short, vol_long)
}

#' D2. Downside Volatility
build_downside_volatility <- function(close, lookback = 20) {
  n <- length(close)
  if (n < lookback + 1) return(rep(NA_real_, n))
  
  # Daily returns
  ret <- c(NA_real_, diff(close) / head(close, -1))
  
  # Rolling downside volatility (only negative returns)
  down_vol <- zoo::rollapplyr(ret, width = lookback, FUN = function(v) {
    down_ret <- v[v < 0]
    if (length(down_ret) < 2) NA_real_ else sd(down_ret)
  }, fill = NA, partial = FALSE)
  
  as.numeric(down_vol)
}

#' D3. Volatility-Scaled Momentum
build_vol_scaled_momentum <- function(close, mom_period = 10, vol_period = 20) {
  n <- length(close)
  if (n < max(mom_period, vol_period) + 1) return(rep(NA_real_, n))
  
  # Momentum
  mom <- c(rep(NA_real_, mom_period), close[(mom_period+1):n] / close[1:(n-mom_period)] - 1)
  
  # Volatility
  ret <- c(NA_real_, diff(close) / head(close, -1))
  vol <- rolling_sd(ret, vol_period) * sqrt(mom_period)
  
  safe_div(mom, vol)
}

# =============================================================================
# FAMILY E: FORMULAIC ALPHA TRANSFORMATIONS
# =============================================================================

#' E1. Time-Series Rank of Returns
build_ts_rank_return <- function(close, ret_period = 5, lookback = 20) {
  n <- length(close)
  if (n < lookback + ret_period) return(rep(NA_real_, n))
  
  # Compute returns
  ret <- c(rep(NA_real_, ret_period), close[(ret_period+1):n] / close[1:(n-ret_period)] - 1)
  
  # Time-series rank
  rolling_rank(ret, lookback)
}

#' E2. Decay-Weighted Return
build_decay_weighted_return <- function(close, lookback = 10) {
  n <- length(close)
  if (n < lookback + 1) return(rep(NA_real_, n))
  
  # Daily returns
  ret <- c(NA_real_, diff(close) / head(close, -1))
  
  # Decay weights (linear decay: 10, 9, 8, ..., 1)
  weights <- rev(seq_len(lookback))
  weights_sum <- sum(weights)
  
  # Weighted sum
  decay_ret <- zoo::rollapplyr(ret, width = lookback, FUN = function(v) {
    sum(v * weights, na.rm = FALSE) / weights_sum
  }, fill = NA, partial = FALSE)
  
  as.numeric(decay_ret)
}

# =============================================================================
# MASTER BUILDER
# =============================================================================

#' Build all formulaic alpha features from OHLCV data
#'
#' @param ohlcv xts object with columns: Open, High, Low, Close, Volume
#' @param groups Character vector of feature groups to build (NULL = all)
#' @return list(features = xts, feature_names = character vector, groups = character vector)
build_formulaic_alphas <- function(ohlcv, groups = NULL) {
  
  if (!inherits(ohlcv, "xts")) {
    stop("ohlcv must be an xts object")
  }
  
  # Extract columns
  open <- as.numeric(ohlcv[, "Open"])
  high <- as.numeric(ohlcv[, "High"])
  low <- as.numeric(ohlcv[, "Low"])
  close <- as.numeric(ohlcv[, "Close"])
  volume <- as.numeric(ohlcv[, "Volume"])
  
  n <- length(close)
  
  if (n < 65) {
    warning("Insufficient data for formulaic alphas (need at least 65 rows)")
  }
  
  # Resolve groups
  if (is.null(groups)) {
    groups <- names(FORMULAIC_ALPHA_GROUPS)
  }
  
  # Build features
  columns <- list()
  
  # Family A: Momentum
  if ("momentum" %in% groups) {
    columns$multi_horizon_disagreement <- build_multi_horizon_disagreement(close)
    columns$price_ma_ratio <- build_price_ma_ratio(close)
    columns$trend_slope <- build_trend_slope(close)
    columns$momentum_vol_ratio <- build_momentum_vol_ratio(close)
    columns$ema_crossover <- build_ema_crossover(close)
  }
  
  # Family B: Reversion
  if ("reversion" %in% groups) {
    columns$reversion_5d <- build_reversion_5d(close)
    columns$return_zscore <- build_return_zscore(close)
    columns$close_location_value <- build_close_location_value(high, low, close)
    columns$large_move_reversion <- build_large_move_reversion(close)
  }
  
  # Family C: Volume
  if ("volume" %in% groups) {
    columns$volume_ratio <- build_volume_ratio(volume)
    columns$volume_pressure <- build_volume_pressure(close, volume)
    columns$obv_change <- build_obv_change(close, volume)
    columns$intraday_intensity <- build_intraday_intensity(high, low, close, volume)
  }
  
  # Family D: Volatility
  if ("volatility" %in% groups) {
    columns$volatility_expansion <- build_volatility_expansion(close)
    columns$downside_volatility <- build_downside_volatility(close)
    columns$vol_scaled_momentum <- build_vol_scaled_momentum(close)
  }
  
  # Family E: Formulaic
  if ("formulaic" %in% groups) {
    columns$ts_rank_return <- build_ts_rank_return(close)
    columns$decay_weighted_return <- build_decay_weighted_return(close)
  }
  
  # Build matrix
  feature_names <- names(columns)
  mat <- do.call(cbind, lapply(columns, function(v) as.numeric(v)))
  colnames(mat) <- feature_names
  
  # Create xts
  features <- xts::xts(mat, order.by = zoo::index(ohlcv))
  
  list(
    features = features,
    feature_names = feature_names,
    groups = groups,
    n_features = length(feature_names)
  )
}

# =============================================================================
# DIAGNOSTIC FUNCTIONS
# =============================================================================

#' Analyze feature quality
analyze_formulaic_features <- function(features) {
  
  feat_mat <- as.matrix(features)
  n <- nrow(feat_mat)
  p <- ncol(feat_mat)
  
  # Missing value analysis
  miss_frac <- colMeans(is.na(feat_mat))
  
  # Summary statistics (after removing NAs)
  feat_stats <- data.frame(
    feature = colnames(feat_mat),
    miss_frac = miss_frac,
    mean = apply(feat_mat, 2, mean, na.rm = TRUE),
    sd = apply(feat_mat, 2, sd, na.rm = TRUE),
    min = apply(feat_mat, 2, min, na.rm = TRUE),
    max = apply(feat_mat, 2, max, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Correlation matrix
  cor_mat <- cor(feat_mat, use = "pairwise.complete.obs")
  
  # High correlation pairs
  high_cor <- which(abs(cor_mat) > 0.95 & row(cor_mat) < col(cor_mat), arr.ind = TRUE)
  if (nrow(high_cor) > 0) {
    high_cor_pairs <- data.frame(
      feature1 = rownames(cor_mat)[high_cor[, 1]],
      feature2 = colnames(cor_mat)[high_cor[, 2]],
      correlation = cor_mat[high_cor]
    )
  } else {
    high_cor_pairs <- NULL
  }
  
  list(
    stats = feat_stats,
    correlation_matrix = cor_mat,
    high_correlation_pairs = high_cor_pairs,
    n_complete_cases = sum(complete.cases(feat_mat))
  )
}
