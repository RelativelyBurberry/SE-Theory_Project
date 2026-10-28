# AlphaForge Insight — Synthetic News Fixture Generator (Phase H.3A)
#
# GENERATES SYNTHETIC DATA FOR PIPELINE VALIDATION ONLY.
# This module creates deterministic, reproducible synthetic news records
# to test temporal alignment, leakage prevention, and feature engineering.
#
# CRITICAL: These are NOT real news headlines. Never present synthetic
# records as actual historical events or use them to claim predictive power.
#
# Usage:
#   - Pipeline architecture validation
#   - Temporal leakage testing
#   - Feature engineering verification
#
# Do NOT:
#   - Infer real sentiment from stock returns
#   - Create artificial correlations designed to show profitability
#   - Mix synthetic records with real data in production experiments
#   - Claim synthetic results demonstrate real-world forecasting value

library(xts)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# Sentinel value to mark synthetic records
SYNTHETIC_NEWS_MARKER <- "SYNTHETIC_PIPELINE_TEST"

# US Market hours (Eastern Time)
MARKET_OPEN_HOUR <- 9
MARKET_OPEN_MIN <- 30
MARKET_CLOSE_HOUR <- 16
MARKET_CLOSE_MIN <- 0

# Timezone for timestamp normalization
DEFAULT_TIMEZONE <- "America/New_York"

# Sentiment categories
SENTIMENT_CATEGORIES <- c("Bullish", "Somewhat_Bullish", "Neutral", "Somewhat_Bearish", "Bearish")

# Event categories for synthetic news
EVENT_CATEGORIES <- c(
  "earnings",
  "product_launch",
  "management_change",
  "regulatory",
  "analyst_rating",
  "market_event",
  "financial_markets",
  "technology"
)

# ---------------------------------------------------------------------------
# Synthetic News Record Generation
# ---------------------------------------------------------------------------

#' Generate a single synthetic news record.
#'
#' @param timestamp Publication timestamp (POSIXct).
#' @param ticker Ticker symbol.
#' @param sentiment_score Sentiment score (-1 to 1).
#' @param event_category Event category.
#' @param source Source identifier.
#' @param article_id Unique article ID.
#' @return list with all news record fields.
generate_synthetic_record <- function(timestamp, ticker, sentiment_score = NULL,
                                       event_category = NULL, source = NULL,
                                       article_id = NULL) {
  
  # Generate deterministic values if not provided
  if (is.null(sentiment_score)) {
    # Deterministic sentiment based on timestamp and ticker
    # Use modulo to avoid integer overflow, ensure positive integer
    ts_numeric <- as.numeric(timestamp)
    seed_val <- abs(as.integer(round(ts_numeric) %% 1000000000L)) + sum(utf8ToInt(ticker))
    set.seed(seed_val)
    sentiment_score <- runif(1, -1, 1)
  }
  
  if (is.null(event_category)) {
    ts_numeric <- as.numeric(timestamp)
    seed_val <- abs(as.integer(round(ts_numeric) %% 1000000000L)) * 2L + sum(utf8ToInt(ticker))
    set.seed(seed_val)
    event_category <- sample(EVENT_CATEGORIES, 1)
  }
  
  if (is.null(source)) {
    ts_numeric <- as.numeric(timestamp)
    source <- sprintf("synthetic_source_%d", abs(as.integer(round(ts_numeric) %% 10000L)))
  }
  
  if (is.null(article_id)) {
    ts_numeric <- as.numeric(timestamp)
    article_id <- sprintf("SYN_%s_%d", ticker, abs(as.integer(round(ts_numeric) %% 100000000L)))
  }
  
  # Determine sentiment category from score
  sentiment_category <- if (sentiment_score >= 0.35) {
    "Bullish"
  } else if (sentiment_score >= 0.15) {
    "Somewhat_Bullish"
  } else if (sentiment_score >= -0.15) {
    "Neutral"
  } else if (sentiment_score >= -0.35) {
    "Somewhat_Bearish"
  } else {
    "Bearish"
  }
  
  list(
    # CRITICAL: Marker to identify synthetic records
    is_synthetic = TRUE,
    synthetic_marker = SYNTHETIC_NEWS_MARKER,
    
    # Core fields
    timestamp = timestamp,
    timezone = DEFAULT_TIMEZONE,
    ticker = ticker,
    
    # Sentiment fields
    sentiment_score = sentiment_score,
    sentiment_category = sentiment_category,
    
    # Event classification
    event_category = event_category,
    
    # Provenance
    source = source,
    article_id = article_id,
    
    # Warning text (always present in synthetic records)
    warning = "SYNTHETIC DATA - NOT A REAL NEWS ARTICLE - FOR PIPELINE TESTING ONLY"
  )
}

#' Generate synthetic news dataset for a ticker over a date range.
#'
#' @param ticker Ticker symbol.
#' @param start_date Start date (Date or character "YYYY-MM-DD").
#' @param end_date End date (Date or character "YYYY-MM-DD").
#' @param n_articles_per_day Average articles per trading day (default 3).
#' @param seed Random seed for reproducibility.
#' @param include_adversarial Include adversarial test cases (default TRUE).
#' @return data.frame of synthetic news records.
generate_synthetic_news <- function(ticker, start_date, end_date,
                                     n_articles_per_day = 3,
                                     seed = 42,
                                     include_adversarial = TRUE) {
  
  set.seed(seed)
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (start_date > end_date) {
    stop("start_date must be before or equal to end_date")
  }
  
  # Generate trading days (exclude weekends)
  all_dates <- seq(start_date, end_date, by = "day")
  trading_days <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
  
  if (length(trading_days) == 0) {
    stop("No trading days in date range")
  }
  
  records <- list()
  
  for (i_day in seq_along(trading_days)) {
    dt <- trading_days[i_day]
    # Random number of articles for this day (Poisson distribution)
    n_articles <- max(0, rpois(1, lambda = n_articles_per_day))
    
    for (i in seq_len(n_articles)) {
      # Generate random publication time
      hour <- sample(MARKET_OPEN_HOUR:MARKET_CLOSE_HOUR, 1)
      minute <- sample(0:59, 1)
      
      # Create timestamp
      timestamp <- as.POSIXct(paste(format(dt, "%Y-%m-%d"), sprintf("%02d:%02d:00", hour, minute)),
                              tz = DEFAULT_TIMEZONE)
      
      record <- generate_synthetic_record(
        timestamp = timestamp,
        ticker = ticker
      )
      
      records[[length(records) + 1]] <- record
    }
  }
  
  # Add adversarial test cases if requested
  if (include_adversarial) {
    adversarial <- generate_adversarial_cases(ticker, trading_days)
    records <- c(records, adversarial)
  }
  
  # Convert to data.frame
  if (length(records) == 0) {
    return(data.frame(
      is_synthetic = logical(0),
      synthetic_marker = character(0),
      timestamp = as.POSIXct(character(0), tz = DEFAULT_TIMEZONE),
      timezone = character(0),
      ticker = character(0),
      sentiment_score = numeric(0),
      sentiment_category = character(0),
      event_category = character(0),
      source = character(0),
      article_id = character(0),
      warning = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Build data.frame manually to preserve POSIXct class
  n_records <- length(records)
  
  # Pre-allocate vectors
  is_synthetic_vec <- logical(n_records)
  synthetic_marker_vec <- character(n_records)
  timestamp_vec <- double(n_records)  # Store as numeric (seconds since epoch)
  timezone_vec <- character(n_records)
  ticker_vec <- character(n_records)
  sentiment_score_vec <- numeric(n_records)
  sentiment_category_vec <- character(n_records)
  event_category_vec <- character(n_records)
  source_vec <- character(n_records)
  article_id_vec <- character(n_records)
  warning_vec <- character(n_records)
  
  for (i in seq_len(n_records)) {
    r <- records[[i]]
    is_synthetic_vec[i] <- isTRUE(r$is_synthetic)
    synthetic_marker_vec[i] <- as.character(r$synthetic_marker)
    # Extract numeric timestamp value, handling POSIXct properly
    if (inherits(r$timestamp, "POSIXt")) {
      timestamp_vec[i] <- as.numeric(as.POSIXct(r$timestamp, tz = DEFAULT_TIMEZONE))
    } else {
      timestamp_vec[i] <- as.numeric(r$timestamp)
    }
    timezone_vec[i] <- as.character(r$timezone)
    ticker_vec[i] <- as.character(r$ticker)
    sentiment_score_vec[i] <- as.numeric(r$sentiment_score)
    sentiment_category_vec[i] <- as.character(r$sentiment_category)
    event_category_vec[i] <- as.character(r$event_category)
    source_vec[i] <- as.character(r$source)
    article_id_vec[i] <- as.character(r$article_id)
    warning_vec[i] <- as.character(r$warning)
  }
  
  # Create data.frame with POSIXct timestamp column
  result_df <- data.frame(
    is_synthetic = is_synthetic_vec,
    synthetic_marker = synthetic_marker_vec,
    timestamp = as.POSIXct(timestamp_vec, origin = "1970-01-01", tz = DEFAULT_TIMEZONE),
    timezone = timezone_vec,
    ticker = ticker_vec,
    sentiment_score = sentiment_score_vec,
    sentiment_category = sentiment_category_vec,
    event_category = event_category_vec,
    source = source_vec,
    article_id = article_id_vec,
    warning = warning_vec,
    stringsAsFactors = FALSE
  )
  
  result_df
}

#' Generate adversarial test cases for leakage testing.
#'
#' These cases are designed to test temporal alignment and leakage prevention.
#'
#' @param ticker Ticker symbol.
#' @param trading_days Vector of trading day dates.
#' @return List of adversarial records.
generate_adversarial_cases <- function(ticker, trading_days) {
  
  if (length(trading_days) < 5) {
    return(list())
  }
  
  records <- list()
  
  # Case 1: After-hours article (published after market close)
  # This should NOT influence features for that day's close
  day1 <- trading_days[1]
  after_close <- as.POSIXct(paste(format(day1, "%Y-%m-%d"), "17:30:00"), tz = DEFAULT_TIMEZONE)
  records[[length(records) + 1]] <- generate_synthetic_record(
    timestamp = after_close,
    ticker = ticker,
    sentiment_score = 0.8,
    event_category = "earnings",
    article_id = paste0("TEST_AFTER_CLOSE_", ticker, "_", format(day1, "%Y%m%d"))
  )
  
  # Case 2: Pre-market article (published before market open)
  # This SHOULD influence features for that day
  day2 <- trading_days[min(2, length(trading_days))]
  pre_market <- as.POSIXct(paste(format(day2, "%Y-%m-%d"), "08:00:00"), tz = DEFAULT_TIMEZONE)
  records[[length(records) + 1]] <- generate_synthetic_record(
    timestamp = pre_market,
    ticker = ticker,
    sentiment_score = -0.7,
    event_category = "analyst_rating",
    article_id = paste0("TEST_PRE_MARKET_", ticker, "_", format(day2, "%Y%m%d"))
  )
  
  # Case 3: Weekend article (Saturday)
  # Should map to next trading day
  weekend_date <- trading_days[3]
  # Find next Saturday
  saturday <- weekend_date + (6 - as.numeric(format(weekend_date, "%w")))
  weekend_ts <- as.POSIXct(paste(format(saturday, "%Y-%m-%d"), "10:00:00"), tz = DEFAULT_TIMEZONE)
  records[[length(records) + 1]] <- generate_synthetic_record(
    timestamp = weekend_ts,
    ticker = ticker,
    sentiment_score = 0.5,
    event_category = "market_event",
    article_id = paste0("TEST_WEEKEND_", ticker, "_", format(saturday, "%Y%m%d"))
  )
  
  # Case 4: Duplicate article (same article_id, different timestamps)
  # Should be deduplicated
  day4 <- trading_days[min(4, length(trading_days))]
  dup_time1 <- as.POSIXct(paste(format(day4, "%Y-%m-%d"), "10:00:00"), tz = DEFAULT_TIMEZONE)
  dup_time2 <- as.POSIXct(paste(format(day4, "%Y-%m-%d"), "11:00:00"), tz = DEFAULT_TIMEZONE)
  
  records[[length(records) + 1]] <- generate_synthetic_record(
    timestamp = dup_time1,
    ticker = ticker,
    sentiment_score = 0.6,
    article_id = paste0("TEST_DUPLICATE_", ticker, "_", format(day4, "%Y%m%d"))
  )
  records[[length(records) + 1]] <- generate_synthetic_record(
    timestamp = dup_time2,
    ticker = ticker,
    sentiment_score = 0.6,
    article_id = paste0("TEST_DUPLICATE_", ticker, "_", format(day4, "%Y%m%d"))
  )
  
  # Case 5: Article with missing sentiment (NA)
  # Should be handled gracefully
  day5 <- trading_days[min(5, length(trading_days))]
  missing_sentiment <- as.POSIXct(paste(format(day5, "%Y-%m-%d"), "12:00:00"), tz = DEFAULT_TIMEZONE)
  records[[length(records) + 1]] <- list(
    is_synthetic = TRUE,
    synthetic_marker = SYNTHETIC_NEWS_MARKER,
    timestamp = missing_sentiment,
    timezone = DEFAULT_TIMEZONE,
    ticker = ticker,
    sentiment_score = NA_real_,
    sentiment_category = NA_character_,
    event_category = "financial_markets",
    source = "synthetic_source_missing",
    article_id = paste0("TEST_MISSING_SENTIMENT_", ticker, "_", format(day5, "%Y%m%d")),
    warning = "SYNTHETIC DATA - NOT A REAL NEWS ARTICLE - FOR PIPELINE TESTING ONLY"
  )
  
  # Case 6: Future-dated article (timestamp in future)
  # This MUST be rejected or assigned to correct date by pipeline
  # (Note: In real pipeline, this would come from data source, not generated)
  # We'll create a record dated "tomorrow" relative to last trading day
  last_day <- trading_days[length(trading_days)]
  future_date <- last_day + 1
  future_ts <- as.POSIXct(paste(format(future_date, "%Y-%m-%d"), "10:00:00"), tz = DEFAULT_TIMEZONE)
  records[[length(records) + 1]] <- generate_synthetic_record(
    timestamp = future_ts,
    ticker = ticker,
    sentiment_score = -0.4,
    event_category = "regulatory",
    article_id = paste0("TEST_FUTURE_DATED_", ticker, "_", format(future_date, "%Y%m%d"))
  )
  
  records
}

#' Check if a news dataset contains synthetic records.
#'
#' @param news_df data.frame of news records.
#' @return TRUE if any records are synthetic, FALSE otherwise.
is_synthetic_news <- function(news_df) {
  if (!"is_synthetic" %in% names(news_df)) {
    return(FALSE)
  }
  any(news_df$is_synthetic == TRUE, na.rm = TRUE)
}

#' Validate that a news dataset is properly marked as synthetic.
#'
#' @param news_df data.frame of news records.
#' @return TRUE if validation passes, stops with error otherwise.
validate_synthetic_marking <- function(news_df) {
  if (!is_synthetic_news(news_df)) {
    return(TRUE)  # Real data, no marking needed
  }
  
  # Check that ALL synthetic records are marked
  synthetic_rows <- which(news_df$is_synthetic == TRUE)
  
  if (length(synthetic_rows) == 0) {
    return(TRUE)
  }
  
  # Verify marker is present
  if (!"synthetic_marker" %in% names(news_df)) {
    stop("CRITICAL: Synthetic news records lack synthetic_marker field")
  }
  
  markers <- news_df$synthetic_marker[synthetic_rows]
  if (!all(markers == SYNTHETIC_NEWS_MARKER, na.rm = TRUE)) {
    stop("CRITICAL: Synthetic news records have incorrect marker value")
  }
  
  # Verify warning is present
  if (!"warning" %in% names(news_df)) {
    stop("CRITICAL: Synthetic news records lack warning field")
  }
  
  warnings <- news_df$warning[synthetic_rows]
  if (!all(grepl("SYNTHETIC", warnings, ignore.case = TRUE), na.rm = TRUE)) {
    stop("CRITICAL: Synthetic news records lack SYNTHETIC warning")
  }
  
  TRUE
}

#' Get summary of adversarial test cases in a dataset.
#'
#' @param news_df data.frame of news records.
#' @return data.frame summarizing test cases.
summarize_adversarial_cases <- function(news_df) {
  if (!"article_id" %in% names(news_df)) {
    return(data.frame())
  }
  
  test_cases <- news_df[grepl("^TEST_", news_df$article_id), ]
  
  if (nrow(test_cases) == 0) {
    return(data.frame())
  }
  
  data.frame(
    article_id = test_cases$article_id,
    timestamp = test_cases$timestamp,
    sentiment_score = test_cases$sentiment_score,
    test_type = ifelse(grepl("AFTER_CLOSE", test_cases$article_id), "after_hours",
                       ifelse(grepl("PRE_MARKET", test_cases$article_id), "pre_market",
                              ifelse(grepl("WEEKEND", test_cases$article_id), "weekend",
                                     ifelse(grepl("DUPLICATE", test_cases$article_id), "duplicate",
                                            ifelse(grepl("MISSING", test_cases$article_id), "missing_sentiment",
                                                   ifelse(grepl("FUTURE", test_cases$article_id), "future_dated",
                                                          "unknown"))))))
  )
}
