# AlphaForge Insight — Phase H.3B Audit Script
#
# Systematic verification of source data, experiment design, and coverage
# for the minimal historical news integration MVP.

library(xts)

# Set working directory
setwd("C:/Users/Lenovo/Desktop/VIT/5th Sem/Software Engineering Project")

# Source the news data
source("backend/analytics/ml/news_real_historical.R")

# =============================================================================
# 1. SOURCE DATA VERIFICATION
# =============================================================================

cat("=== SECTION 1: SOURCE DATA VERIFICATION ===\n\n")

news <- get_real_historical_news()

cat("Total events:", nrow(news), "\n\n")

# Events by ticker
cat("Events by ticker:\n")
ticker_counts <- table(news$ticker)
print(ticker_counts)
cat("\n")

# Events by month
cat("Events by month:\n")
news$month <- format(news$timestamp, "%Y-%m")
month_counts <- table(news$month)
print(month_counts)
cat("\n")

# Events by ticker and month
cat("Events by ticker and month:\n")
ticker_month <- table(news$ticker, news$month)
print(ticker_month)
cat("\n")

# Events by category
cat("Events by category:\n")
category_counts <- table(news$event_category)
print(category_counts)
cat("\n")

# Events by source
cat("Events by source:\n")
source_counts <- table(news$source)
print(source_counts)
cat("\n")

# =============================================================================
# 2. TIMESTAMP PRECISION ANALYSIS
# =============================================================================

cat("=== SECTION 2: TIMESTAMP PRECISION ANALYSIS ===\n\n")

# Check timestamp precision
has_time <- all(format(news$timestamp, "%H:%M:%S") != "00:00:00")
cat("All timestamps have time component:", has_time, "\n\n")

# Time distribution
news$hour <- as.integer(format(news$timestamp, "%H"))
cat("Hour distribution:\n")
hour_counts <- table(news$hour)
print(hour_counts)
cat("\n")

# After-hours events (after 4pm ET)
after_hours <- news[news$hour >= 16, ]
cat("After-hours events (>= 4pm ET):", nrow(after_hours), "\n")
if (nrow(after_hours) > 0) {
  cat("After-hours articles:\n")
  for (i in 1:nrow(after_hours)) {
    cat(sprintf("  %s %s: %s\n", 
                format(after_hours$timestamp[i], "%Y-%m-%d %H:%M"),
                after_hours$ticker[i],
                after_hours$headline[i]))
  }
}
cat("\n")

# Weekend events
news$dow <- as.integer(format(news$timestamp, "%w"))
weekend <- news[news$dow %in% c(0, 6), ]
cat("Weekend events (Sat/Sun):", nrow(weekend), "\n")
if (nrow(weekend) > 0) {
  cat("Weekend articles:\n")
  for (i in 1:nrow(weekend)) {
    cat(sprintf("  %s %s (%s): %s\n",
                format(weekend$timestamp[i], "%Y-%m-%d %H:%M"),
                weekdays(weekend$timestamp[i]),
                weekend$ticker[i],
                weekend$headline[i]))
  }
}
cat("\n")

# =============================================================================
# 3. SENTIMENT DISTRIBUTION
# =============================================================================

cat("=== SECTION 3: SENTIMENT DISTRIBUTION ===\n\n")

cat("Sentiment score statistics:\n")
cat(sprintf("  Mean: %.3f\n", mean(news$sentiment_score, na.rm = TRUE)))
cat(sprintf("  SD: %.3f\n", sd(news$sentiment_score, na.rm = TRUE)))
cat(sprintf("  Min: %.3f\n", min(news$sentiment_score, na.rm = TRUE)))
cat(sprintf("  Max: %.3f\n", max(news$sentiment_score, na.rm = TRUE)))
cat("\n")

cat("Sentiment category distribution:\n")
sentiment_counts <- table(news$sentiment_category)
print(sentiment_counts)
cat("\n")

cat("Sentiment by ticker:\n")
for (ticker in unique(news$ticker)) {
  ticker_news <- news[news$ticker == ticker, ]
  cat(sprintf("  %s: mean=%.3f, sd=%.3f, n=%d\n",
              ticker,
              mean(ticker_news$sentiment_score, na.rm = TRUE),
              sd(ticker_news$sentiment_score, na.rm = TRUE),
              nrow(ticker_news)))
}
cat("\n")

# =============================================================================
# 4. VERIFICATION STATUS
# =============================================================================

cat("=== SECTION 4: DATA VERIFICATION STATUS ===\n\n")

# Categorize events by verification level
cat("Verification levels:\n")
cat("  Level 1 (VERIFIED): Earnings dates/times from company IR - verifiable\n")
cat("  Level 2 (VERIFIABLE): Press releases - dates verifiable, times estimated\n")
cat("  Level 3 (PARTIALLY VERIFIED): News reports - dates verifiable, times estimated\n")
cat("  Level 4 (MANUAL): Sentiment scores - subjective manual annotation\n\n")

# Classify events
cat("Event classification:\n")
earnings <- news[news$event_category == "earnings", ]
cat(sprintf("  Earnings (Level 1-2): %d events\n", nrow(earnings)))

regulatory <- news[news$event_category == "regulatory", ]
cat(sprintf("  Regulatory (Level 2-3): %d events\n", nrow(regulatory)))

product <- news[news$event_category == "product_launch", ]
cat(sprintf("  Product launches (Level 2): %d events\n", nrow(product)))

analyst <- news[news$event_category == "analyst_rating", ]
cat(sprintf("  Analyst ratings (Level 3): %d events\n", nrow(analyst)))

other <- news[!news$event_category %in% c("earnings", "regulatory", "product_launch", "analyst_rating"), ]
cat(sprintf("  Other (Level 2-3): %d events\n", nrow(other)))
cat("\n")

cat("IMPORTANT: All sentiment scores are MANUAL annotations (Level 4)\n")
cat("No ground truth sentiment verification was performed.\n\n")

# =============================================================================
# 5. SAVE DETAILED EVENT TABLE
# =============================================================================

cat("=== SECTION 5: DETAILED EVENT TABLE ===\n\n")

# Create detailed event table
event_table <- data.frame(
  Date = format(news$timestamp, "%Y-%m-%d"),
  Time = format(news$timestamp, "%H:%M"),
  Ticker = news$ticker,
  Category = news$event_category,
  Sentiment = sprintf("%.2f", news$sentiment_score),
  Source = news$source,
  Headline = substr(news$headline, 1, 60),
  stringsAsFactors = FALSE
)

# Sort by date and ticker
event_table <- event_table[order(event_table$Date, event_table$Ticker), ]

print(event_table, row.names = FALSE)

# Save to CSV
write.csv(event_table, "research/results/phase_h3b_event_audit.csv", row.names = FALSE)
cat("\nEvent table saved to: research/results/phase_h3b_event_audit.csv\n")

cat("\n=== AUDIT COMPLETE ===\n")
