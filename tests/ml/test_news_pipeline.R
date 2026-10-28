# AlphaForge Insight — News Pipeline Adversarial Tests (Phase H.3A)
#
# Rigorous tests to verify temporal leakage prevention in the news feature pipeline.
# These tests use adversarial cases to ensure that future information cannot
# influence past features.
#
# Test Categories:
#   1. Future-dated news rejection
#   2. After-hours alignment
#   3. Weekend/holiday alignment
#   4. Duplicate handling
#   5. Missing data handling
#   6. Timezone edge cases
#   7. Deterministic output
#   8. Synthetic/real data separation

library(xts)
library(zoo)

# ---------------------------------------------------------------------------
# Test Helper Functions
# ---------------------------------------------------------------------------

#' Create minimal OHLCV for testing.
#'
#' @param dates Vector of dates.
#' @return xts OHLCV object.
create_test_ohlcv <- function(dates) {
  n <- length(dates)
  set.seed(42)
  
  prices <- 100 + cumsum(rnorm(n, 0, 0.01))
  
  ohlcv <- xts(
    cbind(
      Open = prices,
      High = prices * 1.01,
      Low = prices * 0.99,
      Close = prices,
      Volume = sample(1000000:5000000, n, replace = TRUE)
    ),
    order.by = as.Date(dates)
  )
  
  ohlcv
}

#' Run a single test and report result.
#'
#' @param test_name Name of the test.
#' @param test_expr Expression to evaluate.
#' @return TRUE if test passes, FALSE otherwise.
run_test <- function(test_name, test_expr) {
  cat(sprintf("Testing: %s... ", test_name))
  
  result <- tryCatch({
    eval(test_expr)
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
    return(FALSE)
  })
  
  if (isTRUE(result)) {
    cat("PASSED\n")
    TRUE
  } else {
    cat("FAILED\n")
    FALSE
  }
}

# ---------------------------------------------------------------------------
# Test 1: Future-Dated News Rejection
# ---------------------------------------------------------------------------

test_future_dated_news <- function() {
  # Setup: 5 trading days
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create future-dated article (beyond last trading day)
  future_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "SYNTHETIC_PIPELINE_TEST",
    timestamp = as.POSIXct("2026-10-20 10:00:00", tz = "America/New_York"),  # Future date
    timezone = "America/New_York",
    ticker = "TEST",
    sentiment_score = 0.5,
    sentiment_category = "Bullish",
    event_category = "test",
    source = "test",
    article_id = "TEST_FUTURE_1",
    warning = "SYNTHETIC DATA - FOR TESTING ONLY",
    stringsAsFactors = FALSE
  )
  
  # Build features
  features <- build_news_features(future_news, ohlcv, "TEST")
  
  # All features should be missing (no articles in valid date range)
  all(features$features[, "news_count"] == 0)
}

# ---------------------------------------------------------------------------
# Test 2: After-Hours Alignment
# ---------------------------------------------------------------------------

test_after_hours_alignment <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create after-hours article (5pm on day 1)
  after_close_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "SYNTHETIC_PIPELINE_TEST",
    timestamp = as.POSIXct("2026-10-13 17:00:00", tz = "America/New_York"),  # After 4pm close
    timezone = "America/New_York",
    ticker = "TEST",
    sentiment_score = 0.5,
    sentiment_category = "Bullish",
    event_category = "test",
    source = "test",
    article_id = "TEST_AFTER_HOURS_1",
    warning = "SYNTHETIC DATA - FOR TESTING ONLY",
    stringsAsFactors = FALSE
  )
  
  features <- build_news_features(after_close_news, ohlcv, "TEST")
  
  # Article should contribute to NEXT trading day, not day 1
  day1_idx <- 1
  day2_idx <- 2
  
  day1_count <- as.numeric(features$features[day1_idx, "news_count"])
  day2_count <- as.numeric(features$features[day2_idx, "news_count"])
  
  # Day 1 should have NO news (after-hours article moves to day 2)
  # Day 2 should have the article
  day1_count == 0 && day2_count == 1
}

# ---------------------------------------------------------------------------
# Test 3: Weekend/Holiday Alignment
# ---------------------------------------------------------------------------

test_weekend_alignment <- function() {
  # Setup: include a weekend
  dates <- seq(as.Date("2026-10-16"), as.Date("2026-10-20"), by = "day")
  # Oct 17 = Saturday, Oct 18 = Sunday
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create Saturday article
  saturday_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "SYNTHETIC_PIPELINE_TEST",
    timestamp = as.POSIXct("2026-10-17 10:00:00", tz = "America/New_York"),  # Saturday
    timezone = "America/New_York",
    ticker = "TEST",
    sentiment_score = 0.5,
    sentiment_category = "Bullish",
    event_category = "test",
    source = "test",
    article_id = "TEST_WEEKEND_1",
    warning = "SYNTHETIC DATA - FOR TESTING ONLY",
    stringsAsFactors = FALSE
  )
  
  features <- build_news_features(saturday_news, ohlcv, "TEST")
  
  # Saturday article should map to Monday (Oct 19)
  aligned_news <- features$aligned_news
  monday_session <- as.Date("2026-10-19")
  
  nrow(aligned_news) > 0 && aligned_news$session_date[1] == monday_session
}

# ---------------------------------------------------------------------------
# Test 4: Duplicate Handling
# ---------------------------------------------------------------------------

test_duplicate_handling <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create duplicate articles (same article_id, different timestamps)
  dup_news <- data.frame(
    is_synthetic = c(TRUE, TRUE),
    synthetic_marker = c("SYNTHETIC_PIPELINE_TEST", "SYNTHETIC_PIPELINE_TEST"),
    timestamp = c(
      as.POSIXct("2026-10-13 10:00:00", tz = "America/New_York"),
      as.POSIXct("2026-10-13 11:00:00", tz = "America/New_York")
    ),
    timezone = c("America/New_York", "America/New_York"),
    ticker = c("TEST", "TEST"),
    sentiment_score = c(0.5, 0.5),
    sentiment_category = c("Bullish", "Bullish"),
    event_category = c("test", "test"),
    source = c("test", "test"),
    article_id = c("TEST_DUP_1", "TEST_DUP_1"),  # Same article_id
    warning = c("SYNTHETIC DATA - FOR TESTING ONLY", "SYNTHETIC DATA - FOR TESTING ONLY"),
    stringsAsFactors = FALSE
  )
  
  features <- build_news_features(dup_news, ohlcv, "TEST")
  
  # Should count as 1 article, not 2
  day1_count <- as.numeric(features$features[1, "news_count"])
  day1_count == 1
}

# ---------------------------------------------------------------------------
# Test 5: Missing Data Handling
# ---------------------------------------------------------------------------

test_missing_sentiment_handling <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create article with missing sentiment
  missing_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "SYNTHETIC_PIPELINE_TEST",
    timestamp = as.POSIXct("2026-10-13 10:00:00", tz = "America/New_York"),
    timezone = "America/New_York",
    ticker = "TEST",
    sentiment_score = NA_real_,  # Missing
    sentiment_category = NA_character_,  # Missing
    event_category = "test",
    source = "test",
    article_id = "TEST_MISSING_1",
    warning = "SYNTHETIC DATA - FOR TESTING ONLY",
    stringsAsFactors = FALSE
  )
  
  # Should not crash
  features <- tryCatch({
    build_news_features(missing_news, ohlcv, "TEST")
  }, error = function(e) {
    return(NULL)
  })
  
  # Should complete without error
  !is.null(features) && 
    features$features[1, "news_count"] == 1 &&
    is.na(features$features[1, "news_sentiment_mean"])
}

# ---------------------------------------------------------------------------
# Test 6: Timezone Edge Cases
# ---------------------------------------------------------------------------

test_timezone_conversion <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create article in UTC (5 hours ahead of ET)
  utc_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "SYNTHETIC_PIPELINE_TEST",
    timestamp = as.POSIXct("2026-10-13 20:00:00", tz = "UTC"),  # 4pm ET = 8pm UTC
    timezone = "UTC",
    ticker = "TEST",
    sentiment_score = 0.5,
    sentiment_category = "Bullish",
    event_category = "test",
    source = "test",
    article_id = "TEST_TZ_1",
    warning = "SYNTHETIC DATA - FOR TESTING ONLY",
    stringsAsFactors = FALSE
  )
  
  features <- build_news_features(utc_news, ohlcv, "TEST")
  
  # 8pm UTC = 4pm ET (at cutoff), should stay on same day
  features$features[1, "news_count"] == 1
}

test_timezone_ordering <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create two articles: one in UTC, one in ET
  mixed_news <- data.frame(
    is_synthetic = c(TRUE, TRUE),
    synthetic_marker = c("SYNTHETIC_PIPELINE_TEST", "SYNTHETIC_PIPELINE_TEST"),
    timestamp = c(
      as.POSIXct("2026-10-13 15:00:00", tz = "UTC"),  # 11am ET
      as.POSIXct("2026-10-13 14:00:00", tz = "America/New_York")  # 2pm ET
    ),
    timezone = c("UTC", "America/New_York"),
    ticker = c("TEST", "TEST"),
    sentiment_score = c(0.5, 0.6),
    sentiment_category = c("Bullish", "Bullish"),
    event_category = c("test", "test"),
    source = c("test", "test"),
    article_id = c("TEST_TZ_MIXED_1", "TEST_TZ_MIXED_2"),
    warning = c("SYNTHETIC DATA - FOR TESTING ONLY", "SYNTHETIC DATA - FOR TESTING ONLY"),
    stringsAsFactors = FALSE
  )
  
  features <- build_news_features(mixed_news, ohlcv, "TEST")
  
  # Should have 2 articles for day 1
  features$features[1, "news_count"] == 2
}

# ---------------------------------------------------------------------------
# Test 7: Deterministic Output
# ---------------------------------------------------------------------------

test_deterministic_output <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create deterministic synthetic news
  set.seed(42)
  news <- generate_synthetic_news("TEST", "2026-10-13", "2026-10-17",
                                   n_articles_per_day = 3, seed = 42,
                                   include_adversarial = FALSE)
  
  # Build features twice
  features1 <- build_news_features(news, ohlcv, "TEST")
  features2 <- build_news_features(news, ohlcv, "TEST")
  
  # Should be identical
  all.equal(features1$features, features2$features)
}

# ---------------------------------------------------------------------------
# Test 8: Synthetic/Real Data Separation
# ---------------------------------------------------------------------------

test_synthetic_marking <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Generate synthetic news
  news <- generate_synthetic_news("TEST", "2026-10-13", "2026-10-17",
                                   seed = 42, include_adversarial = FALSE)
  
  # Check marking
  all(news$is_synthetic == TRUE) &&
    all(news$synthetic_marker == "SYNTHETIC_PIPELINE_TEST") &&
    all(grepl("SYNTHETIC", news$warning))
}

test_synthetic_validation <- function() {
  # Create synthetic news without proper marking
  bad_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "WRONG_MARKER",
    timestamp = as.POSIXct("2026-10-13 10:00:00", tz = "America/New_York"),
    timezone = "America/New_York",
    ticker = "TEST",
    sentiment_score = 0.5,
    sentiment_category = "Bullish",
    event_category = "test",
    source = "test",
    article_id = "TEST_BAD_1",
    warning = "NOT A PROPER WARNING",
    stringsAsFactors = FALSE
  )
  
  # Should fail validation
  tryCatch({
    validate_synthetic_marking(bad_news)
    FALSE  # Should not reach here
  }, error = function(e) {
    TRUE  # Expected error
  })
}

# ---------------------------------------------------------------------------
# Test 9: Pre-Market Alignment
# ---------------------------------------------------------------------------

test_pre_market_alignment <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create pre-market article (8am ET, before 9:30am open)
  pre_market_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "SYNTHETIC_PIPELINE_TEST",
    timestamp = as.POSIXct("2026-10-13 08:00:00", tz = "America/New_York"),
    timezone = "America/New_York",
    ticker = "TEST",
    sentiment_score = 0.5,
    sentiment_category = "Bullish",
    event_category = "test",
    source = "test",
    article_id = "TEST_PRE_MARKET_1",
    warning = "SYNTHETIC DATA - FOR TESTING ONLY",
    stringsAsFactors = FALSE
  )
  
  features <- build_news_features(pre_market_news, ohlcv, "TEST")
  
  # Pre-market article should contribute to same day
  features$features[1, "news_count"] == 1
}

# ---------------------------------------------------------------------------
# Test 10: Adversarial Future Information Injection
# ---------------------------------------------------------------------------

test_adversarial_future_injection <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-17"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create news for day 3
  day3_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "SYNTHETIC_PIPELINE_TEST",
    timestamp = as.POSIXct("2026-10-15 10:00:00", tz = "America/New_York"),
    timezone = "America/New_York",
    ticker = "TEST",
    sentiment_score = 0.8,  # Strong sentiment
    sentiment_category = "Bullish",
    event_category = "test",
    source = "test",
    article_id = "TEST_ADVERSARIAL_1",
    warning = "SYNTHETIC DATA - FOR TESTING ONLY",
    stringsAsFactors = FALSE
  )
  
  features <- build_news_features(day3_news, ohlcv, "TEST")
  
  # Day 1 and Day 2 should have ZERO news count
  # (article is in the future relative to them)
  day1_count <- as.numeric(features$features[1, "news_count"])
  day2_count <- as.numeric(features$features[2, "news_count"])
  
  # Day 3 should have the article
  day3_count <- as.numeric(features$features[3, "news_count"])
  
  day1_count == 0 && day2_count == 0 && day3_count == 1
}

# ---------------------------------------------------------------------------
# Test 11: Rolling Features No Leakage
# ---------------------------------------------------------------------------

test_rolling_features_no_leakage <- function() {
  # Setup
  dates <- seq(as.Date("2026-10-13"), as.Date("2026-10-23"), by = "day")
  trading_days <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  ohlcv <- create_test_ohlcv(trading_days)
  
  # Create news only on the LAST day
  last_day_news <- data.frame(
    is_synthetic = TRUE,
    synthetic_marker = "SYNTHETIC_PIPELINE_TEST",
    timestamp = as.POSIXct(paste(tail(trading_days, 1), "10:00:00"), tz = "America/New_York"),
    timezone = "America/New_York",
    ticker = "TEST",
    sentiment_score = 0.8,
    sentiment_category = "Bullish",
    event_category = "test",
    source = "test",
    article_id = "TEST_ROLLING_1",
    warning = "SYNTHETIC DATA - FOR TESTING ONLY",
    stringsAsFactors = FALSE
  )
  
  features <- build_news_features(last_day_news, ohlcv, "TEST")
  
  # Check rolling 7-day count
  # All days EXCEPT the last should have 0 in rolling count
  n <- nrow(features$features)
  rolling_counts <- as.numeric(features$features[1:(n-1), "news_count_7d"])
  
  all(rolling_counts == 0)
}

# ---------------------------------------------------------------------------
# Run All Tests
# ---------------------------------------------------------------------------

run_all_news_pipeline_tests <- function() {
  cat("\n=== NEWS PIPELINE ADVERSARIAL TESTS ===\n\n")
  
  tests <- list(
    future_dated = expression(test_future_dated_news()),
    after_hours = expression(test_after_hours_alignment()),
    weekend = expression(test_weekend_alignment()),
    duplicate = expression(test_duplicate_handling()),
    missing = expression(test_missing_sentiment_handling()),
    timezone_conv = expression(test_timezone_conversion()),
    timezone_order = expression(test_timezone_ordering()),
    deterministic = expression(test_deterministic_output()),
    synthetic_mark = expression(test_synthetic_marking()),
    synthetic_valid = expression(test_synthetic_validation()),
    pre_market = expression(test_pre_market_alignment()),
    adversarial = expression(test_adversarial_future_injection()),
    rolling = expression(test_rolling_features_no_leakage())
  )
  
  results <- sapply(names(tests), function(name) {
    run_test(name, tests[[name]])
  })
  
  cat(sprintf("\n=== RESULTS: %d/%d tests passed ===\n", sum(results), length(results)))
  
  if (all(results)) {
    cat("ALL TESTS PASSED\n")
  } else {
    cat("SOME TESTS FAILED\n")
    failed <- names(results)[!results]
    cat(sprintf("Failed tests: %s\n", paste(failed, collapse = ", ")))
  }
  
  invisible(results)
}

# ---------------------------------------------------------------------------
# Main Execution
# ---------------------------------------------------------------------------

if (!interactive() && identical(sys.nframe(), 0L)) {
  # Source dependencies
  ml_dir <- file.path(getwd(), "backend", "analytics", "ml")
  source(file.path(ml_dir, "news_synthetic.R"), local = TRUE)
  source(file.path(ml_dir, "news_pipeline.R"), local = TRUE)
  
  run_all_news_pipeline_tests()
}
