# AlphaForge Insight — News Feature Pipeline (Phase H.3A)
#
# Implements news ingestion, normalization, session alignment, and feature
# extraction with rigorous temporal leakage prevention.
#
# CRITICAL TEMPORAL ALIGNMENT RULES:
#   - A feature at time t may only use news published BEFORE or AT the
#     decision cutoff for time t.
#   - Decision cutoff: market close (4pm ET) on date t for signals executed
#     at open[t+1].
#   - After-hours news (published after 4pm ET) contributes to features
#     for the NEXT trading day.
#   - Weekend/holiday news maps to the NEXT trading day.
#   - Missing news is EXPLICIT - not the same as neutral sentiment.
#
# This pipeline supports both real news data (from GDELT or other sources)
# and synthetic news fixtures for testing. Synthetic records are always
# clearly marked and never mixed with real data in production experiments.

library(xts)
library(zoo)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# Decision cutoff: market close time (4pm ET)
DECISION_CUTOFF_HOUR <- 16
DECISION_CUTOFF_MIN <- 0

# Timezone for all timestamp normalization
PIPELINE_TIMEZONE <- "America/New_York"

# Minimum relevance score for article to be included (0-1 scale)
MIN_RELEVANCE_SCORE <- 0.5

# Days to look back for rolling news features
NEWS_LOOKBACK_DAYS <- 7

# ---------------------------------------------------------------------------
# Timestamp Normalization
# ---------------------------------------------------------------------------

#' Normalize timestamp to pipeline timezone.
#'
#' Converts various timestamp formats to POSIXct in America/New_York timezone.
#'
#' @param timestamp Input timestamp (POSIXct, character, or numeric).
#' @param tz Original timezone (default "UTC").
#' @return POSIXct in pipeline timezone.
normalize_timestamp <- function(timestamp, tz = "UTC") {
  # Handle POSIXct directly
  if (inherits(timestamp, "POSIXct")) {
    # Already POSIXct, just ensure proper timezone
    result <- as.POSIXct(as.numeric(timestamp), origin = "1970-01-01", tz = PIPELINE_TIMEZONE)
    return(result)
  }
  
  # Handle POSIXlt
  if (inherits(timestamp, "POSIXlt")) {
    result <- as.POSIXct(timestamp, tz = PIPELINE_TIMEZONE)
    return(result)
  }
  
  if (is.character(timestamp)) {
    # Try common formats
    formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y%m%dT%H%M%S",
                 "%Y-%m-%d %H:%M", "%Y-%m-%d")
    
    parsed <- tryCatch({
      # First try as numeric string (YYYYMMDDHHMMSS format)
      if (nchar(timestamp) == 14 && grepl("^\\d+$", timestamp)) {
        as.POSIXct(timestamp, format = "%Y%m%d%H%M%S", tz = tz)
      } else {
        # Try each format
        for (fmt in formats) {
          result <- tryCatch(
            as.POSIXct(timestamp, format = fmt, tz = tz),
            error = function(e) NULL
          )
          if (!is.null(result)) {
            return(result)
          }
        }
        stop("No matching format")
      }
    }, error = function(e) {
      stop(sprintf("Cannot parse timestamp: %s", timestamp))
    })
    
    # Convert to pipeline timezone
    return(as.POSIXct(as.numeric(parsed), origin = "1970-01-01", tz = PIPELINE_TIMEZONE))
  }
  
  if (is.numeric(timestamp)) {
    # Assume Unix timestamp (seconds since 1970-01-01)
    return(as.POSIXct(timestamp, origin = "1970-01-01", tz = PIPELINE_TIMEZONE))
  }
  
  stop(sprintf("Unsupported timestamp type: %s", class(timestamp)[1]))
}

#' Get trading date for a timestamp.
#'
#' Maps a timestamp to the trading date it belongs to:
#'   - Articles before 4pm ET on a trading day -> that day
#'   - Articles after 4pm ET on a trading day -> NEXT trading day
#'   - Articles on weekends/holidays -> NEXT trading day
#'
#' @param timestamp POSIXct timestamp.
#' @param trading_days Vector of Date objects for valid trading days.
#' @return Date (trading date this article contributes to).
get_trading_date <- function(timestamp, trading_days) {
  # Normalize to Date and time
  dt <- as.Date(timestamp, tz = PIPELINE_TIMEZONE)
  hour <- as.integer(format(timestamp, "%H", tz = PIPELINE_TIMEZONE))
  minute <- as.integer(format(timestamp, "%M", tz = PIPELINE_TIMEZONE))
  
  # Check if after decision cutoff (4pm ET)
  after_cutoff <- (hour > DECISION_CUTOFF_HOUR) ||
                  (hour == DECISION_CUTOFF_HOUR && minute > DECISION_CUTOFF_MIN)
  
  # Check if weekend
  dow <- as.integer(format(dt, "%w"))  # 0 = Sunday, 6 = Saturday
  is_weekend <- dow %in% c(0, 6)
  
  # Check if holiday (not in trading_days)
  is_holiday <- !dt %in% trading_days
  
  if (after_cutoff || is_weekend || is_holiday) {
    # Find next trading day
    future_dates <- trading_days[trading_days > dt]
    if (length(future_dates) == 0) {
      warning("No future trading day available for timestamp")
      return(dt)  # Fallback to the date itself
    }
    return(future_dates[1])
  }
  
  # Same trading day
  dt
}

#' Check if timestamp is before decision cutoff for a trading date.
#'
#' @param timestamp POSIXct timestamp to check.
#' @param trading_date Date to compare against.
#' @return TRUE if timestamp is before or at cutoff for that trading date.
is_before_cutoff <- function(timestamp, trading_date) {
  cutoff <- as.POSIXct(paste(trading_date, sprintf("%02d:%02d:00",
                          DECISION_CUTOFF_HOUR, DECISION_CUTOFF_MIN)),
                       tz = PIPELINE_TIMEZONE)
  timestamp <= cutoff
}

# ---------------------------------------------------------------------------
# News Ingestion and Deduplication
# ---------------------------------------------------------------------------

#' Deduplicate news articles.
#'
#' Removes duplicate articles based on article_id. If duplicates exist with
#' different timestamps, keeps the EARLIEST timestamp (conservative for
#' leakage prevention).
#'
#' @param news_df data.frame with article_id and timestamp columns.
#' @return data.frame with duplicates removed.
deduplicate_news <- function(news_df) {
  if (!"article_id" %in% names(news_df)) {
    warning("No article_id column - cannot deduplicate")
    return(news_df)
  }
  
  # Sort by timestamp (earliest first)
  news_df <- news_df[order(news_df$timestamp), ]
  
  # Keep first occurrence of each article_id (earliest timestamp)
  news_df[!duplicated(news_df$article_id), ]
}

#' Filter news by ticker relevance.
#'
#' @param news_df data.frame with ticker_sentiment or ticker column.
#' @param ticker Ticker to filter for.
#' @param min_relevance Minimum relevance score (0-1).
#' @return data.frame filtered to relevant articles.
filter_news_by_ticker <- function(news_df, ticker, min_relevance = MIN_RELEVANCE_SCORE) {
  if (!"ticker" %in% names(news_df)) {
    stop("news_df must have 'ticker' column")
  }
  
  # Primary filter: exact ticker match
  result <- news_df[news_df$ticker == ticker, ]
  
  # Secondary filter: relevance score if available
  if ("relevance_score" %in% names(news_df) && !is.null(min_relevance)) {
    rel <- as.numeric(result$relevance_score)
    result <- result[is.na(rel) | rel >= min_relevance, ]
  }
  
  result
}

#' Validate news dataset structure.
#'
#' Checks required columns and data types.
#'
#' @param news_df data.frame to validate.
#' @return TRUE if valid, stops with error otherwise.
validate_news_structure <- function(news_df) {
  required_cols <- c("timestamp", "ticker")
  
  missing <- setdiff(required_cols, names(news_df))
  if (length(missing) > 0) {
    stop(sprintf("news_df missing required columns: %s", paste(missing, collapse = ", ")))
  }
  
  # Check for empty dataset
  if (nrow(news_df) == 0) {
    warning("news_df has zero rows")
    return(TRUE)
  }
  
  # Validate timestamps
  tryCatch({
    normalize_timestamp(news_df$timestamp[1])
  }, error = function(e) {
    stop(sprintf("Invalid timestamp in news_df: %s", e$message))
  })
  
  TRUE
}

# ---------------------------------------------------------------------------
# Session Alignment
# ---------------------------------------------------------------------------

#' Align news articles to trading sessions.
#'
#' For each article, determines which trading session it contributes to
#' based on publication timestamp and market hours.
#'
#' @param news_df data.frame with timestamp column.
#' @param trading_days Vector of Date objects for valid trading days.
#' @return data.frame with added session_date column.
align_news_to_sessions <- function(news_df, trading_days) {
  if (nrow(news_df) == 0) {
    news_df$session_date <- as.Date(character(0))
    return(news_df)
  }
  
  # Normalize timestamps
  news_df$timestamp_normalized <- sapply(news_df$timestamp, function(ts) {
    as.character(normalize_timestamp(ts))
  })
  news_df$timestamp_normalized <- as.POSIXct(news_df$timestamp_normalized, tz = PIPELINE_TIMEZONE)
  
  # Determine trading date for each article
  news_df$session_date <- sapply(news_df$timestamp_normalized, function(ts) {
    as.character(get_trading_date(ts, trading_days))
  })
  news_df$session_date <- as.Date(news_df$session_date)
  
  # Add flag for after-hours articles
  news_df$is_after_hours <- sapply(seq_len(nrow(news_df)), function(i) {
    ts <- news_df$timestamp_normalized[i]
    session <- news_df$session_date[i]
    !is_before_cutoff(ts, session)
  })
  
  news_df
}

# ---------------------------------------------------------------------------
# Feature Extraction
# ---------------------------------------------------------------------------

#' Compute sentiment proportions.
#'
#' @param sentiment_categories Vector of sentiment categories.
#' @return list(bullish, somewhat_bullish, neutral, somewhat_bearish, bearish proportions).
compute_sentiment_proportions <- function(sentiment_categories) {
  sentiment_categories <- sentiment_categories[!is.na(sentiment_categories)]
  
  if (length(sentiment_categories) == 0) {
    return(list(
      prop_bullish = NA_real_,
      prop_somewhat_bullish = NA_real_,
      prop_neutral = NA_real_,
      prop_somewhat_bearish = NA_real_,
      prop_bearish = NA_real_
    ))
  }
  
  counts <- table(sentiment_categories)
  total <- sum(counts)
  
  list(
    prop_bullish = as.numeric(counts["Bullish"]) / total,
    prop_somewhat_bullish = as.numeric(counts["Somewhat_Bullish"]) / total,
    prop_neutral = as.numeric(counts["Neutral"]) / total,
    prop_somewhat_bearish = as.numeric(counts["Somewhat_Bearish"]) / total,
    prop_bearish = as.numeric(counts["Bearish"]) / total
  )
}

#' Aggregate news features for a single trading session.
#'
#' @param session_news data.frame of news for one trading session.
#' @param session_date Date of the trading session.
#' @return list of aggregated features.
aggregate_session_features <- function(session_news, session_date) {
  if (nrow(session_news) == 0) {
    # EXPLICIT MISSING NEWS - not the same as neutral sentiment
    return(list(
      news_count = 0,
      news_missing = TRUE,
      news_sentiment_mean = NA_real_,
      news_sentiment_sd = NA_real_,
      news_prop_bullish = NA_real_,
      news_prop_somewhat_bullish = NA_real_,
      news_prop_neutral = NA_real_,
      news_prop_somewhat_bearish = NA_real_,
      news_prop_bearish = NA_real_,
      news_latest_sentiment = NA_real_,
      news_hours_since_latest = NA_real_,
      news_event_count = 0L
    ))
  }
  
  # Compute sentiment features
  sentiment_scores <- as.numeric(session_news$sentiment_score)
  sentiment_scores <- sentiment_scores[!is.na(sentiment_scores)]
  
  sentiment_mean <- if (length(sentiment_scores) > 0) mean(sentiment_scores) else NA_real_
  sentiment_sd <- if (length(sentiment_scores) > 1) sd(sentiment_scores) else NA_real_
  
  # Sentiment proportions
  props <- compute_sentiment_proportions(session_news$sentiment_category)
  
  # Recency features
  latest_ts <- max(session_news$timestamp_normalized, na.rm = TRUE)
  session_cutoff <- as.POSIXct(paste(session_date, sprintf("%02d:%02d:00",
                                        DECISION_CUTOFF_HOUR, DECISION_CUTOFF_MIN)),
                               tz = PIPELINE_TIMEZONE)
  hours_since_latest <- if (!is.na(latest_ts)) {
    as.numeric(difftime(session_cutoff, latest_ts, units = "hours"))
  } else {
    NA_real_
  }
  
  latest_sentiment <- if (length(sentiment_scores) > 0) {
    # Get sentiment of most recent article
    latest_idx <- which.max(session_news$timestamp_normalized)
    as.numeric(session_news$sentiment_score[latest_idx])
  } else {
    NA_real_
  }
  
  # Event category count
  event_count <- length(unique(session_news$event_category[!is.na(session_news$event_category)]))
  
  list(
    news_count = nrow(session_news),
    news_missing = FALSE,
    news_sentiment_mean = sentiment_mean,
    news_sentiment_sd = sentiment_sd,
    news_prop_bullish = props$prop_bullish,
    news_prop_somewhat_bullish = props$prop_somewhat_bullish,
    news_prop_neutral = props$prop_neutral,
    news_prop_somewhat_bearish = props$prop_somewhat_bearish,
    news_prop_bearish = props$prop_bearish,
    news_latest_sentiment = latest_sentiment,
    news_hours_since_latest = hours_since_latest,
    news_event_count = as.integer(event_count)
  )
}

#' Compute rolling news features.
#'
#' For each date, aggregates news from the previous N trading days.
#'
#' @param session_features List of per-session features.
#' @param dates Vector of trading dates.
#' @param lookback_days Number of days to look back (default 7).
#' @return data.frame with rolling features.
compute_rolling_features <- function(session_features, dates, lookback_days = NEWS_LOOKBACK_DAYS) {
  n <- length(dates)
  
  # Initialize output
  result <- data.frame(
    date = dates,
    news_count_7d = integer(n),
    news_sentiment_mean_7d = numeric(n),
    news_sentiment_momentum = numeric(n),
    news_count_trend = numeric(n),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(dates)) {
    # Determine lookback window
    start_idx <- max(1, i - lookback_days)
    window_indices <- start_idx:(i - 1)  # Exclude current day (no leakage)
    
    if (length(window_indices) == 0) {
      result$news_count_7d[i] <- 0
      result$news_sentiment_mean_7d[i] <- NA_real_
      result$news_sentiment_momentum[i] <- NA_real_
      result$news_count_trend[i] <- NA_real_
      next
    }
    
    # Aggregate over window
    window_features <- session_features[window_indices]
    
    # Total count
    result$news_count_7d[i] <- sum(sapply(window_features, function(f) f$news_count), na.rm = TRUE)
    
    # Mean sentiment (weighted by count)
    sentiments <- sapply(window_features, function(f) f$news_sentiment_mean)
    counts <- sapply(window_features, function(f) f$news_count)
    valid_idx <- !is.na(sentiments) & counts > 0
    
    if (any(valid_idx)) {
      result$news_sentiment_mean_7d[i] <- sum(sentiments[valid_idx] * counts[valid_idx]) / sum(counts[valid_idx])
    } else {
      result$news_sentiment_mean_7d[i] <- NA_real_
    }
    
    # Sentiment momentum: current vs 7-day average
    if (!is.na(session_features[[i]]$news_sentiment_mean) && !is.na(result$news_sentiment_mean_7d[i])) {
      result$news_sentiment_momentum[i] <- session_features[[i]]$news_sentiment_mean - result$news_sentiment_mean_7d[i]
    } else {
      result$news_sentiment_momentum[i] <- NA_real_
    }
    
    # Count trend: recent 3 days vs previous 4 days
    if (i > 4) {
      recent_counts <- sum(sapply(session_features[(i-3):(i-1)], function(f) f$news_count), na.rm = TRUE)
      older_counts <- sum(sapply(session_features[max(1, i-7):(i-4)], function(f) f$news_count), na.rm = TRUE)
      result$news_count_trend[i] <- if (older_counts > 0) {
        (recent_counts - older_counts) / older_counts
      } else {
        NA_real_
      }
    } else {
      result$news_count_trend[i] <- NA_real_
    }
  }
  
  result
}

# ---------------------------------------------------------------------------
# Main Pipeline Functions
# ---------------------------------------------------------------------------

#' Build news features from news dataset.
#'
#' Main entry point for news feature generation. Takes a news dataset and
#' OHLCV data, aligns news to trading sessions, and generates model-ready features.
#'
#' TEMPORAL LEAKAGE PREVENTION:
#'   - Only news published BEFORE the decision cutoff for date t contributes
#'     to features for date t.
#'   - Rolling features use ONLY data from dates BEFORE t.
#'
#' @param news_df data.frame with news articles (timestamp, ticker, sentiment_score, etc.).
#' @param ohlcv xts OHLCV data (provides trading dates).
#' @param ticker Ticker symbol to filter for.
#' @param lookback_days Days to look back for rolling features.
#' @return list(features = xts, feature_names = character, metadata = list).
build_news_features <- function(news_df, ohlcv, ticker, lookback_days = NEWS_LOOKBACK_DAYS) {
  
  # Validate inputs
  validate_news_structure(news_df)
  
  # Extract trading dates from OHLCV
  trading_dates <- as.Date(index(ohlcv))
  
  # Filter news by ticker
  ticker_news <- filter_news_by_ticker(news_df, ticker)
  
  # Check if synthetic
  is_synthetic <- is_synthetic_news(ticker_news)
  if (is_synthetic) {
    validate_synthetic_marking(ticker_news)
  }
  
  # Deduplicate
  ticker_news <- deduplicate_news(ticker_news)
  
  # Align to sessions
  ticker_news <- align_news_to_sessions(ticker_news, trading_dates)
  
  # Initialize feature storage
  n <- nrow(ohlcv)
  feature_matrix <- matrix(NA_real_, nrow = n, ncol = 12)
  colnames(feature_matrix) <- c(
    "news_count", "news_missing", "news_sentiment_mean", "news_sentiment_sd",
    "news_prop_bullish", "news_prop_somewhat_bullish", "news_prop_neutral",
    "news_prop_somewhat_bearish", "news_prop_bearish", "news_latest_sentiment",
    "news_hours_since_latest", "news_event_count"
  )
  
  # Compute per-session features
  session_features <- vector("list", n)
  
  for (i in seq_along(trading_dates)) {
    dt <- trading_dates[i]
    
    # Get news for this session (already aligned)
    session_news <- ticker_news[ticker_news$session_date == dt, ]
    
    # Aggregate
    session_features[[i]] <- aggregate_session_features(session_news, dt)
    
    # Fill feature matrix
    feature_matrix[i, ] <- c(
      session_features[[i]]$news_count,
      as.integer(session_features[[i]]$news_missing),
      session_features[[i]]$news_sentiment_mean,
      session_features[[i]]$news_sentiment_sd,
      session_features[[i]]$news_prop_bullish,
      session_features[[i]]$news_prop_somewhat_bullish,
      session_features[[i]]$news_prop_neutral,
      session_features[[i]]$news_prop_somewhat_bearish,
      session_features[[i]]$news_prop_bearish,
      session_features[[i]]$news_latest_sentiment,
      session_features[[i]]$news_hours_since_latest,
      session_features[[i]]$news_event_count
    )
  }
  
  # Convert to xts
  features_xts <- xts(feature_matrix, order.by = index(ohlcv))
  
  # Compute rolling features
  rolling <- compute_rolling_features(session_features, trading_dates, lookback_days)
  
  rolling_xts <- xts(
    cbind(
      rolling$news_count_7d,
      rolling$news_sentiment_mean_7d,
      rolling$news_sentiment_momentum,
      rolling$news_count_trend
    ),
    order.by = index(ohlcv)
  )
  colnames(rolling_xts) <- c(
    "news_count_7d", "news_sentiment_mean_7d",
    "news_sentiment_momentum", "news_count_trend"
  )
  
  # Combine
  features_xts <- cbind(features_xts, rolling_xts)
  
  # Metadata
  metadata <- list(
    ticker = ticker,
    is_synthetic = is_synthetic,
    n_articles_total = nrow(ticker_news),
    n_sessions_with_news = sum(sapply(session_features, function(f) f$news_count > 0)),
    lookback_days = lookback_days,
    timezone = PIPELINE_TIMEZONE,
    decision_cutoff = sprintf("%02d:%02d ET", DECISION_CUTOFF_HOUR, DECISION_CUTOFF_MIN)
  )
  
  list(
    features = features_xts,
    feature_names = colnames(features_xts),
    metadata = metadata,
    aligned_news = ticker_news  # For debugging/testing
  )
}

#' Integrate news features with market features.
#'
#' Combines news features with existing market features from build_features().
#'
#' @param ohlcv xts OHLCV data.
#' @param news_df data.frame of news articles.
#' @param ticker Ticker symbol.
#' @param market_groups Feature groups for market features (NULL = all).
#' @return list(features = xts, feature_names = character, metadata = list).
build_combined_features <- function(ohlcv, news_df, ticker, market_groups = NULL) {
  
  # Source feature builder if not in environment
  if (!exists("build_features")) {
    ml_dir <- file.path(getwd(), "backend", "analytics", "ml")
    source(file.path(ml_dir, "features.R"), local = TRUE)
  }
  
  # Build market features
  market_result <- build_features(ohlcv, groups = market_groups)
  
  # Build news features
  news_result <- build_news_features(news_df, ohlcv, ticker)
  
  # Combine
  combined_features <- cbind(market_result$features, news_result$features)
  
  # Metadata
  metadata <- list(
    ticker = ticker,
    n_market_features = ncol(market_result$features),
    n_news_features = ncol(news_result$features),
    n_total_features = ncol(combined_features),
    market_groups = market_result$groups,
    news_is_synthetic = news_result$metadata$is_synthetic
  )
  
  list(
    features = combined_features,
    feature_names = colnames(combined_features),
    metadata = metadata,
    market_features = market_result,
    news_features = news_result
  )
}
