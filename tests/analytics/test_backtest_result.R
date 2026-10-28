# AlphaForge Insight — full-pipeline backtest integration tests (Phase 5)
#
# DETERMINISTIC: no network access, no database. Exercises the complete
# Phase 5 pipeline through run_backtest():
#
#   signals -> t+1 OPEN execution -> cash/shares -> trade log ->
#   daily equity -> metrics -> clean result object
#
# and verifies determinism, the result-object structure, open-position
# handling, total-trade counting and the final-candle rule end-to-end.
#
# Run from the repository root:
#   Rscript tests/analytics/test_backtest_result.R
#
# Exit code 0 = pass, 1 = fail. Requires: xts, zoo.

suppressPackageStartupMessages(source("backend/analytics/execution.R"))
suppressPackageStartupMessages(source("backend/analytics/metrics.R"))

results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

expect_equal_num <- function(x, y, tol = 1e-8) {
  isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol,
                   check.attributes = FALSE))
}

# ---------------------------------------------------------------------------
# Fixtures (shared with test_execution.R / test_metrics.R)
# ---------------------------------------------------------------------------
START <- as.Date("2024-01-01")
make_ohlcv <- function(open, close, start = START) {
  n <- length(open)
  xts::xts(cbind(Open = as.numeric(open), Close = as.numeric(close)),
           order.by = start + 0:(n - 1))
}

# Two completed round trips (open 100..109, close 105..114).
E3_ohlcv <- make_ohlcv(100:109, 105:114)
E3_signals <- c(NA_character_, "BUY", NA_character_, "SELL", NA_character_,
                "BUY", NA_character_, "SELL", NA_character_, NA_character_)

# One BUY, never sold (open final position, marked to market).
E2_signals <- c(NA_character_, "BUY", NA_character_, NA_character_, NA_character_)
E1_ohlcv <- make_ohlcv(c(100, 101, 102, 103, 104), c(110, 111, 112, 113, 114))

cat("==================================================\n")
cat("FULL-PIPELINE BACKTEST INTEGRATION TESTS (Phase 5)\n")
cat("==================================================\n\n")

# ===========================================================================
# A. COMPLETE RUN: two round trips on E3
# ===========================================================================
res <- run_backtest(
  ohlcv = E3_ohlcv, signals = E3_signals, initial_capital = 10000,
  start_date = "2024-01-01", end_date = "2024-01-10",
  ticker = "TEST", strategy = "RSI",
  parameters = list(period = 14, oversold = 30, overbought = 70),
  transaction_cost = 0, risk_free_rate = 0.02
)

record("A1. result object has summary / equity_curve / trades / metadata",
       all(c("summary", "equity_curve", "trades", "metadata") %in% names(res)))

record("A2. summary carries all 10 documented fields",
       identical(names(res$summary),
                 c("initial_capital", "final_value", "total_return", "cagr",
                   "sharpe", "sortino", "maximum_drawdown", "win_rate",
                   "volatility", "total_trades")))

record("A3. equity curve has date/cash/shares/close/portfolio_value/daily_return",
       identical(names(res$equity_curve),
                 c("date", "cash", "shares", "close", "portfolio_value",
                   "daily_return")) &&
         nrow(res$equity_curve) == 10)

record("A4. trades structure: history + completed + open",
       all(c("history", "completed", "open") %in% names(res$trades)) &&
         nrow(res$trades$history) == 4 &&
         nrow(res$trades$completed) == 2 &&
         is.null(res$trades$open))

record("A5. metadata records ticker/strategy/parameters/dates/cost/rf/convention",
       identical(res$metadata$ticker, "TEST") &&
         identical(res$metadata$strategy, "RSI") &&
         identical(res$metadata$parameters$period, 14) &&
         identical(format(res$metadata$start_date), "2024-01-01") &&
         identical(format(res$metadata$end_date), "2024-01-10") &&
         expect_equal_num(res$metadata$transaction_cost, 0) &&
         expect_equal_num(res$metadata$risk_free_rate, 0.02) &&
         grepl("OPEN of t\\+1", res$metadata$execution_convention))

record("A6. summary values for E3 (final 10388, +3.88%, 2 trades, win 1.0)",
       expect_equal_num(res$summary$final_value, 10388) &&
         expect_equal_num(res$summary$total_return, 10388 / 10000 - 1, tol = 1e-12) &&
         expect_equal_num(res$summary$total_trades, 2) &&
         expect_equal_num(res$summary$win_rate, 1) &&
         res$summary$maximum_drawdown <= 0 &&
         !is.na(res$summary$sharpe) && !is.na(res$summary$sortino) &&
         !is.na(res$summary$volatility) && !is.na(res$summary$cagr))

record("A7. no Inf/NaN in the full result summary",
       !any(is.infinite(unlist(res$summary))) &&
         !any(is.nan(unlist(res$summary))))

# ===========================================================================
# B. DETERMINISM
# ===========================================================================
res_again <- run_backtest(
  ohlcv = E3_ohlcv, signals = E3_signals, initial_capital = 10000,
  start_date = "2024-01-01", end_date = "2024-01-10",
  ticker = "TEST", strategy = "RSI",
  parameters = list(period = 14, oversold = 30, overbought = 70),
  transaction_cost = 0, risk_free_rate = 0.02
)
record("B1. identical inputs produce an identical result object",
       identical(res, res_again))

# ===========================================================================
# C. OPEN FINAL POSITION
# ===========================================================================
res_open <- run_backtest(
  ohlcv = E1_ohlcv, signals = E2_signals, initial_capital = 10000,
  start_date = "2024-01-01", end_date = "2024-01-05",
  ticker = "TEST", strategy = "MA_Crossover",
  parameters = list(fast_period = 2, slow_period = 3),
  transaction_cost = 0, risk_free_rate = 0.02
)
record("C1. open position: not completed, not counted, marked to market",
       nrow(res_open$trades$completed) == 0 &&
         !is.null(res_open$trades$open) &&
         expect_equal_num(res_open$summary$total_trades, 0) &&
         is.na(res_open$summary$win_rate) &&
         expect_equal_num(res_open$summary$final_value, 4 + 98 * 114) &&
         expect_equal_num(res_open$summary$total_return, (4 + 98 * 114) / 10000 - 1,
                          tol = 1e-12))

record("C2. open position record carries entry/mark/value/unrealized P/L",
       identical(format(res_open$trades$open$entry_date), "2024-01-03") &&
         expect_equal_num(res_open$trades$open$entry_price, 102) &&
         expect_equal_num(res_open$trades$open$shares, 98) &&
         expect_equal_num(res_open$trades$open$mark_price, 114) &&
         expect_equal_num(res_open$trades$open$current_value, 11172) &&
         expect_equal_num(res_open$trades$open$unrealized_pnl, 11172 - 9996))

# ===========================================================================
# D. TOTAL-TRADE COUNTING (one round trip = one trade)
# ===========================================================================
# BUY, SELL, BUY(open) on E3-length data:
sig_partial <- c(NA_character_, "BUY", NA_character_, "SELL", NA_character_,
                 "BUY", NA_character_, NA_character_, NA_character_, NA_character_)
res_partial <- run_backtest(E3_ohlcv, sig_partial, 10000,
                            "2024-01-01", "2024-01-10",
                            ticker = "TEST", strategy = "MA_Crossover")
record("D1. one completed round trip + one open -> total trades = 1 (not 3)",
       nrow(res_partial$trades$history) == 3 &&
         nrow(res_partial$trades$completed) == 1 &&
         expect_equal_num(res_partial$summary$total_trades, 1) &&
         expect_equal_num(res_partial$summary$win_rate, 1) &&
         !is.null(res_partial$trades$open))

# ===========================================================================
# E. SIGNAL ON THE FINAL CANDLE (end-to-end)
# ===========================================================================
sig_final_buy <- c(NA_character_, NA_character_, NA_character_, NA_character_, "BUY")
res_final <- run_backtest(E1_ohlcv, sig_final_buy, 10000,
                          "2024-01-01", "2024-01-05",
                          ticker = "TEST", strategy = "MA_Crossover")
record("E1. signal on final candle is never executed (flat result)",
       nrow(res_final$trades$history) == 0 &&
         expect_equal_num(res_final$summary$final_value, 10000) &&
         expect_equal_num(res_final$summary$total_trades, 0) &&
         is.na(res_final$summary$win_rate))

# ===========================================================================
# F. EMPTY / DEGENERATE INPUTS COVERED BY DOCUMENTED ERRORS
# ===========================================================================
empty_xts <- xts::xts(
  matrix(numeric(0), nrow = 0, ncol = 2,
         dimnames = list(NULL, c("Open", "Close"))),
  order.by = as.Date(character(0)))
record("F1. empty price data rejected by run_backtest",
       inherits(try(run_backtest(empty_xts, character(0), 10000,
                                 "2024-01-01", "2024-01-05"), silent = TRUE),
                "try-error"))
record("F2. invalid capital rejected by run_backtest",
       inherits(try(run_backtest(E1_ohlcv, rep(NA_character_, 5), 0,
                                 "2024-01-01", "2024-01-05"), silent = TRUE),
                "try-error"))

# ===========================================================================
# Summary
# ===========================================================================
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d backtest-pipeline tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL BACKTEST PIPELINE TESTS PASSED\n")
quit(status = 0)