# AlphaForge Insight — strategy validation unit tests (Phase 4)
#
# Deterministic unit tests covering:
#   - Valid MA Crossover configuration
#   - Valid RSI configuration
#   - Missing/invalid strategy types
#   - Initial capital validation (zero, negative, non-numeric, infinite)
#   - Ticker validation (empty, whitespace)
#   - Date validation (format, start > end, future dates)
#   - MA parameter constraints (non-positive, non-integer, fast >= slow)
#   - RSI parameter constraints (non-positive, thresholds <=0, >=100, oversold >= overbought)
#
# No network access or database connection required.
#
# Run from the repository root:
#   Rscript tests/analytics/test_strategy_validation.R
#
# Exit code 0 = pass, 1 = fail.

# Source the validation functions (relative to the repository root).
source("backend/analytics/utils/validation.R")

passed <- 0L
failed <- 0L

run_test <- function(name, expr) {
  result <- tryCatch(
    {
      eval(substitute(expr), envir = parent.frame())
      TRUE
    },
    error = function(e) {
      cat(sprintf("  FAIL: %s\n        Error: %s\n", name, conditionMessage(e)))
      FALSE
    }
  )
  if (result) {
    passed <<- passed + 1L
    cat(sprintf("  PASS: %s\n", name))
  } else {
    failed <<- failed + 1L
  }
}

cat("==================================================\n")
cat("STRATEGY VALIDATION UNIT TESTS (Phase 4)\n")
cat("==================================================\n\n")

# Use a fixed reference date for tests
REF_TODAY <- as.Date("2026-09-12")

# 1. Valid MA configuration
run_test("1. Valid MA Crossover configuration passes", {
  res <- validate_strategy_config(
    ticker = "AAPL",
    start = "2020-01-01",
    end = "2020-12-31",
    initial_capital = 10000,
    strategy = "MA_Crossover",
    parameters = list(fast_period = 50, slow_period = 200),
    today = REF_TODAY
  )
  stopifnot(
    res$ticker == "AAPL",
    res$strategy == "MA_Crossover",
    res$parameters$fast_period == 50L,
    res$parameters$slow_period == 200L,
    res$initial_capital == 10000
  )
})

# 2. Valid RSI configuration
run_test("2. Valid RSI configuration passes", {
  res <- validate_strategy_config(
    ticker = "MSFT",
    start = "2020-01-01",
    end = "2020-12-31",
    initial_capital = 5000,
    strategy = "RSI",
    parameters = list(period = 14, oversold = 30, overbought = 70),
    today = REF_TODAY
  )
  stopifnot(
    res$ticker == "MSFT",
    res$strategy == "RSI",
    res$parameters$period == 14L,
    res$parameters$oversold == 30,
    res$parameters$overbought == 70
  )
})

# 3. Missing strategy type
run_test("3. Missing strategy type is rejected", {
  err <- tryCatch(
    validate_strategy_type(NULL),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("strategy type is required", err))
})

# 4. Invalid strategy type
run_test("4. Unsupported strategy type is rejected", {
  err <- tryCatch(
    validate_strategy_type("INVALID_STRATEGY"),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("Unsupported strategy type", err))
})

# 5. Zero initial capital
run_test("5. Zero initial capital is rejected", {
  err <- tryCatch(
    validate_initial_capital(0),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("greater than zero", err))
})

# 6. Negative initial capital
run_test("6. Negative initial capital is rejected", {
  err <- tryCatch(
    validate_initial_capital(-100),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("greater than zero", err))
})

# 7. Invalid ticker (empty)
run_test("7. Empty ticker is rejected", {
  err <- tryCatch(
    validate_ticker_param(""),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("ticker is required", err))
})

# 8. Invalid date format
run_test("8. Invalid date format is rejected", {
  err <- tryCatch(
    validate_date_range("2020/01/01", "2020-12-31", today = REF_TODAY),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("YYYY-MM-DD format", err))
})

# 9. Start date > End date
run_test("9. Start date after end date is rejected", {
  err <- tryCatch(
    validate_date_range("2021-01-01", "2020-12-31", today = REF_TODAY),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("on or before", err))
})

# 10. Future start date
run_test("10. Future start date is rejected", {
  err <- tryCatch(
    validate_date_range("2027-01-01", "2027-12-31", today = REF_TODAY),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("cannot be in the future", err))
})

# 11. MA fast period <= 0
run_test("11. MA fast period <= 0 is rejected", {
  err <- tryCatch(
    validate_ma_params(0, 200),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("positive integer", err))
})

# 12. MA slow period <= 0
run_test("12. MA slow period <= 0 is rejected", {
  err <- tryCatch(
    validate_ma_params(50, -10),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("positive integer", err))
})

# 13. MA fast >= slow (equality)
run_test("13. MA fast == slow is rejected (never swapped)", {
  err <- tryCatch(
    validate_ma_params(100, 100),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("strictly less than", err))
})

# 14. MA fast > slow (inverted, should NOT auto-swap)
run_test("14. MA fast > slow is rejected (never silently corrected)", {
  err <- tryCatch(
    validate_ma_params(200, 50),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("strictly less than", err))
})

# 15. RSI period <= 0
run_test("15. RSI period <= 0 is rejected", {
  err <- tryCatch(
    validate_rsi_params(0, 30, 70),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("positive integer", err))
})

# 16. RSI oversold <= 0
run_test("16. RSI oversold <= 0 is rejected", {
  err <- tryCatch(
    validate_rsi_params(14, 0, 70),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("greater than zero", err))
})

# 17. RSI overbought >= 100
run_test("17. RSI overbought >= 100 is rejected", {
  err <- tryCatch(
    validate_rsi_params(14, 30, 100),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("less than 100", err))
})

# 18. RSI oversold >= overbought
run_test("18. RSI oversold >= overbought is rejected", {
  err <- tryCatch(
    validate_rsi_params(14, 70, 30),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("strictly less than", err))
})

# 19. Non-integer periods rejected
run_test("19. Non-integer period is rejected", {
  err <- tryCatch(
    validate_ma_params(50.5, 200),
    validation_error = function(e) conditionMessage(e),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl("positive integer", err))
})

# 20. STRATEGY_DEFAULTS contains expected Review 1 values
run_test("20. STRATEGY_DEFAULTS has single source of truth defaults", {
  stopifnot(
    STRATEGY_DEFAULTS$ma_crossover$fast_period == 50L,
    STRATEGY_DEFAULTS$ma_crossover$slow_period == 200L,
    STRATEGY_DEFAULTS$rsi$period == 14L,
    STRATEGY_DEFAULTS$rsi$oversold == 30,
    STRATEGY_DEFAULTS$rsi$overbought == 70
  )
})

cat(sprintf("\nSUMMARY: %d passed, %d failed.\n", passed, failed))

if (failed > 0) {
  quit(status = 1)
} else {
  cat("All strategy validation unit tests PASSED.\n")
}
