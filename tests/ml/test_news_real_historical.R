# AlphaForge Insight — Tests for Real Historical News Adapter (Phase H.3B)
#
# Validates that real historical news data:
#   1. Has correct structure matching news_pipeline.R input format
#   2. Is marked as real (not synthetic)
#   3. Has valid timestamps and sentiment scores
#   4. Passes through pipeline without leakage
#
# Part of Phase H.3B: Minimal Historical News Integration MVP

library(testthat)

# Set ML directory
ml_dir <- file.path(dirname(dirname(getwd())), "backend", "analytics", "ml")

# =============================================================================
# CONTEXT: Real Historical News Data Validation
# =============================================================================

context("Phase H.3B - Real Historical News Adapter")

# =============================================================================
# DATA STRUCTURE TESTS
# =============================================================================

test_that("Real historical news has correct structure", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # Check required columns
  required_cols <- c("timestamp", "ticker", "sentiment_score", 
                     "sentiment_category", "event_category", 
                     "source", "article_id")
  
  expect_true(all(required_cols %in% names(news)),
              info = paste("Missing columns:", 
                          paste(setdiff(required_cols, names(news)), collapse = ", ")))
  
  # Check data types
  expect_true(inherits(news$timestamp, "POSIXt"),
              info = "timestamp must be POSIXt")
  expect_true(is.character(news$ticker),
              info = "ticker must be character")
  expect_true(is.numeric(news$sentiment_score),
              info = "sentiment_score must be numeric")
})

test_that("Real news is marked as non-synthetic", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # All records should be marked as NOT synthetic
  if ("is_synthetic" %in% names(news)) {
    expect_true(all(news$is_synthetic == FALSE, na.rm = TRUE),
                info = "All real news records must have is_synthetic = FALSE")
  }
  
  # Should NOT have synthetic marker
  if ("synthetic_marker" %in% names(news)) {
    expect_true(all(is.na(news$synthetic_marker)),
                info = "Real news should not have synthetic_marker values")
  }
})

test_that("Real news has valid sentiment scores", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # Sentiment scores should be in [-1, 1]
  expect_true(all(news$sentiment_score >= -1 & news$sentiment_score <= 1, 
                  na.rm = TRUE),
              info = "Sentiment scores must be in [-1, 1] range")
  
  # No NA sentiment scores (hand-curated data)
  expect_true(all(!is.na(news$sentiment_score)),
              info = "Hand-curated news should not have NA sentiment scores")
})

test_that("Real news has valid timestamps", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # No NA timestamps
  expect_true(all(!is.na(news$timestamp)),
              info = "Real news should not have NA timestamps")
  
  # All timestamps should be in the expected date range
  config <- REAL_NEWS_CONFIG
  start_ts <- as.POSIXct(config$start_date, tz = "America/New_York")
  end_ts <- as.POSIXct(config$end_date, tz = "America/New_York") + 24 * 3600
  
  expect_true(all(news$timestamp >= start_ts & news$timestamp <= end_ts),
              info = "All timestamps should be within configured date range")
})

test_that("Real news has unique article IDs", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # All article IDs should be unique
  expect_equal(length(news$article_id), length(unique(news$article_id)),
               info = "All article IDs must be unique")
})

test_that("Real news covers configured tickers", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  config <- REAL_NEWS_CONFIG
  
  # Should have events for all configured tickers
  tickers_in_data <- unique(news$ticker)
  expect_true(all(config$tickers %in% tickers_in_data),
              info = paste("Missing tickers:", 
                          paste(setdiff(config$tickers, tickers_in_data), 
                                collapse = ", ")))
})

# =============================================================================
# TEMPORAL LEAKAGE PREVENTION TESTS
# =============================================================================

test_that("Real news passes temporal alignment without leakage", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  source(file.path(ml_dir, "news_synthetic.R"), local = TRUE)
  source(file.path(ml_dir, "news_pipeline.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # Create synthetic OHLCV for testing
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  
  # Create dummy OHLCV
  set.seed(42)
  ohlcv <- xts::xts(
    matrix(rnorm(length(trading_days) * 5, mean = 100, sd = 5), 
           ncol = 5, 
           dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
    order.by = trading_days
  )
  
  # Test for each ticker
  for (ticker in c("AAPL", "MSFT", "GOOGL")) {
    ticker_news <- news[news$ticker == ticker, ]
    
    if (nrow(ticker_news) > 0) {
      # Should not error when aligning
      result <- tryCatch({
        aligned <- align_news_to_sessions(ticker_news, trading_days)
        list(success = TRUE, aligned = aligned)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      
      expect_true(result$success,
                  info = sprintf("News alignment failed for %s: %s", 
                                ticker, result$error))
      
      # Check after-hours handling
      if (result$success) {
        aligned <- result$aligned
        
        # Check that after-hours articles map to next trading day
        if ("is_after_hours" %in% names(aligned)) {
          after_hours <- aligned[aligned$is_after_hours == TRUE, ]
          if (nrow(after_hours) > 0) {
            for (i in 1:nrow(after_hours)) {
              article_date <- as.Date(after_hours$timestamp[i])
              session_date <- after_hours$session_date[i]
              
              # Session date should be AFTER article date
              expect_true(session_date > article_date,
                          info = sprintf("After-hours article %s should map to future session",
                                        after_hours$article_id[i]))
            }
          }
        }
      }
    }
  }
})

test_that("Real news feature pipeline produces valid features", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  source(file.path(ml_dir, "news_synthetic.R"), local = TRUE)
  source(file.path(ml_dir, "news_pipeline.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # Create synthetic OHLCV for testing
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  
  set.seed(42)
  ohlcv <- xts::xts(
    matrix(rnorm(length(trading_days) * 5, mean = 100, sd = 5), 
           ncol = 5, 
           dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
    order.by = trading_days
  )
  
  # Build features for AAPL
  aapl_news <- news[news$ticker == "AAPL", ]
  
  result <- tryCatch({
    build_news_features(aapl_news, ohlcv, "AAPL")
  }, error = function(e) {
    list(error = conditionMessage(e))
  })
  
  expect_null(result$error, info = paste("Feature building failed:", result$error))
  
  if (is.null(result$error)) {
    # Check feature structure
    expect_true(inherits(result$features, "xts"),
                info = "Features should be xts object")
    expect_equal(ncol(result$features), 16,
                info = "Should have 16 news features")
    expect_true(all(!is.na(result$feature_names)),
                info = "Feature names should not be NA")
    
    # Check metadata
    expect_false(result$metadata$is_synthetic,
                info = "Metadata should indicate real (non-synthetic) data")
  }
})

# =============================================================================
# DATA QUALITY TESTS
# =============================================================================

test_that("Real news sentiment categories match scores", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # Check sentiment category consistency
  for (i in 1:nrow(news)) {
    score <- news$sentiment_score[i]
    category <- news$sentiment_category[i]
    
    if (!is.na(score) && !is.na(category)) {
      if (score >= 0.35) {
        expect_equal(category, "Bullish",
                    info = sprintf("Score %.2f should be Bullish, got %s", 
                                  score, category))
      } else if (score >= 0.15) {
        expect_equal(category, "Somewhat_Bullish",
                    info = sprintf("Score %.2f should be Somewhat_Bullish, got %s",
                                  score, category))
      } else if (score >= -0.15) {
        expect_equal(category, "Neutral",
                    info = sprintf("Score %.2f should be Neutral, got %s",
                                  score, category))
      } else if (score >= -0.35) {
        expect_equal(category, "Somewhat_Bearish",
                    info = sprintf("Score %.2f should be Somewhat_Bearish, got %s",
                                  score, category))
      } else {
        expect_equal(category, "Bearish",
                    info = sprintf("Score %.2f should be Bearish, got %s",
                                  score, category))
      }
    }
  }
})

test_that("Real news has no future-dated articles", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  # No article should be dated after end of experiment period
  config <- REAL_NEWS_CONFIG
  end_date <- as.POSIXct(config$end_date, tz = "America/New_York") + 24 * 3600
  
  expect_true(all(news$timestamp <= end_date),
              info = "No articles should be dated after experiment end date")
  
  # No article should be dated before start of experiment period
  start_date <- as.POSIXct(config$start_date, tz = "America/New_York")
  
  expect_true(all(news$timestamp >= start_date),
              info = "No articles should be dated before experiment start date")
})

test_that("Real news event categories are valid", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  news <- get_real_historical_news()
  
  valid_categories <- c("earnings", "product_launch", "management_change", 
                       "regulatory", "analyst_rating", "market_event",
                       "financial_markets", "technology")
  
  expect_true(all(news$event_category %in% valid_categories),
              info = paste("Invalid categories:", 
                          paste(setdiff(news$event_category, valid_categories), 
                                collapse = ", ")))
})

# =============================================================================
# SUMMARY STATISTICS TEST
# =============================================================================

test_that("Real news summary is accurate", {
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  summary <- summarize_real_news()
  news <- get_real_historical_news()
  
  expect_equal(summary$n_total, nrow(news),
              info = "Summary n_total should match actual row count")
  
  expect_equal(sort(names(summary$events_per_ticker)), 
               sort(unique(news$ticker)),
              info = "Summary should include all tickers")
})
