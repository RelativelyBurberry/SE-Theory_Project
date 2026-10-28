# AlphaForge Insight — backtesting execution + equity-curve unit tests (Phase 5)
#
# DETERMINISTIC: no network access, no database. Validates the execution
# engine (backend/analytics/execution.R) against hand-built fixtures:
#
#   Execution semantics (IMPLEMENTATION_PLAN.md section H, Phase 5 spec):
#     - signal at close of t  ->  executed at OPEN of t+1 (no look-ahead)
#     - BUY only from FLAT; SELL only from LONG; repeated/stale signals
#       are ignored by the state machine
#     - order size = floor(cash / execution_price) shares
#     - portfolio_value[t] = cash[t] + shares[t] * Close[t]
#     - open final position is marked to market but is NOT a completed trade
#     - signal on the final candle is never executed
#
# Run from the repository root:
#   Rscript tests/analytics/test_execution.R
#
# Exit code 0 = pass, 1 = fail. Requires: xts, zoo.

suppressPackageStartupMessages(source("backend/analytics/execution.R"))

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
# Fixture helpers
# ---------------------------------------------------------------------------
START <- as.Date("2024-01-01")

# Simple 2-column OHLCV (Open, Close) with deterministic daily dates.
make_ohlcv <- function(open, close, start = START) {
  n <- length(open)
  xts::xts(cbind(Open = as.numeric(open), Close = as.numeric(close)),
           order.by = start + 0:(n - 1))
}

# ---- E1: single completed round trip (proves t+1 OPEN execution) ----------
#   day:        1     2     3     4     5
#   open:     100   101   102   103   104
#   close:    110   111   112   113   114
#   signal:    NA   BUY    NA  SELL    NA
# BUY at close of day 2 -> executed at OPEN day 3 (102); SELL at close of
# day 4 -> executed at OPEN day 5 (104).
E1_ohlcv <- make_ohlcv(c(100, 101, 102, 103, 104),
                       c(110, 111, 112, 113, 114))
E1_signals <- c(NA_character_, "BUY", NA_character_, "SELL", NA_character_)
E1_cap <- 10000

# ---- E2: open final position (BUY, never sold) ----------------------------
E2_signals <- c(NA_character_, "BUY", NA_character_, NA_character_, NA_character_)

# ---- E3: two independent completed round trips ----------------------------
#   day:        1    2    3    4    5    6    7    8    9   10
#   open:     100  101  102  103  104  105  106  107  108  109
#   close:    105  106  107  108  109  110  111  112  113  114
#   signal:    NA  BUY   NA SELL   NA  BUY   NA SELL   NA   NA
E3_ohlcv <- make_ohlcv(100:109, 105:114)
E3_signals <- c(NA_character_, "BUY", NA_character_, "SELL", NA_character_,
                "BUY", NA_character_, "SELL", NA_character_, NA_character_)

# ---- E4: repeated BUY while LONG / repeated SELL while FLAT ----------------
E4_signals <- c(NA_character_, "BUY", "BUY", NA_character_, "SELL", "SELL",
                NA_character_, "BUY", NA_character_, NA_character_)

# ---- E6a: BUY signal on the FINAL candle only ------------------------------
E6a_signals <- c(NA_character_, NA_character_, NA_character_,
                 NA_character_, "BUY")
# ---- E6b: SELL signal on the FINAL candle (never executed) ------------------
E6b_signals <- c(NA_character_, "BUY", NA_character_, NA_character_, "SELL")

cat("==================================================\n")
cat("EXECUTION ENGINE UNIT TESTS (Phase 5)\n")
cat("==================================================\n\n")

# ===========================================================================
# 1-16: EXECUTION SEMANTICS
# ===========================================================================

# 1. No signals -> capital unchanged.
r_no_sig <- execute_backtest(E1_ohlcv,
                             rep(NA_character_, 5), E1_cap)
record("1. no signals: capital unchanged (0 trades, flat equity)",
       nrow(r_no_sig$trade_history) == 0 &&
         nrow(r_no_sig$completed_trades) == 0 &&
         is.null(r_no_sig$open_position) &&
         all(r_no_sig$equity_curve$portfolio_value == E1_cap))

# 2. BUY executes at the OPEN of t+1.
r1 <- execute_backtest(E1_ohlcv, E1_signals, E1_cap)
buy_row <- r1$trade_history[1, ]
record("2. BUY executes at t+1 OPEN (signal d2 -> execution at OPEN d3=102)",
       identical(buy_row$side, "BUY") &&
         identical(format(buy_row$signal_date), "2024-01-02") &&
         identical(format(buy_row$execution_date), "2024-01-03") &&
         expect_equal_num(buy_row$execution_price, 102))

# 3. BUY does NOT execute at signal-day CLOSE (111) nor the same-day OPEN (101).
record("3. BUY price is OPEN[t+1], NOT Close[t] (111) nor Open[t] (101)",
       !expect_equal_num(buy_row$execution_price, 111) &&
         !expect_equal_num(buy_row$execution_price, 101))

# 4. SELL executes at the OPEN of t+1.
sell_row <- r1$trade_history[2, ]
record("4. SELL executes at t+1 OPEN (signal d4 -> execution at OPEN d5=104)",
       identical(sell_row$side, "SELL") &&
         identical(format(sell_row$signal_date), "2024-01-04") &&
         identical(format(sell_row$execution_date), "2024-01-05") &&
         expect_equal_num(sell_row$execution_price, 104))

# 5. BUY reduces cash correctly: 10000 - 98*102 = 4.
record("5. BUY reduces cash correctly (cash_after = 10000 - 98*102 = 4)",
       expect_equal_num(buy_row$shares, 98) &&
         expect_equal_num(buy_row$cash_after, 4) &&
         expect_equal_num(buy_row$cash_after,
                          E1_cap - buy_row$shares * buy_row$execution_price))

# 6. SELL increases cash correctly: 4 + 98*104 = 10196.
record("6. SELL increases cash correctly (cash_after = 4 + 98*104 = 10196)",
       expect_equal_num(sell_row$cash_after, 10196) &&
         expect_equal_num(sell_row$cash_after,
                          buy_row$cash_after + sell_row$shares * sell_row$execution_price))

# 7. Shares = floor(cash / execution_price).
record("7. shares = floor(cash / execution_price) = floor(10000/102) = 98",
       expect_equal_num(buy_row$shares, floor(10000 / 102)))

# 8. Cash is never negative anywhere in the simulation.
all_cash <- c(r1$equity_curve$cash, r1$trade_history$cash_after,
              r_no_sig$equity_curve$cash)
record("8. cash never becomes negative (all fixtures)",
       all(is.finite(all_cash)) && all(all_cash >= 0))

# 9. Long-only state transitions: FLAT -> LONG on BUY, LONG -> FLAT on SELL.
record("9. long-only state transitions (FLAT->LONG->FLAT)",
       identical(r1$trade_history$position_after, c("LONG", "FLAT")) &&
         all(r1$trade_history$shares >= 0) &&
         all(r1$equity_curve$shares >= 0))

# 10. Repeated BUY while already LONG does not duplicate the position.
r4 <- execute_backtest(E3_ohlcv, E4_signals, E1_cap)
buys_in_e4 <- r4$trade_history[r4$trade_history$side == "BUY", ]
record("10. repeated BUY while LONG creates NO duplicate position",
       nrow(r4$trade_history) == 3 &&
         nrow(buys_in_e4) == 2 &&
         expect_equal_num(buys_in_e4$shares[1], 98) &&
         expect_equal_num(buys_in_e4$shares[2], 95) &&
         identical(format(buys_in_e4$execution_date), c("2024-01-03", "2024-01-09")))

# 11. Repeated SELL while FLAT does not create invalid trades.
sells_in_e4 <- r4$trade_history[r4$trade_history$side == "SELL", ]
record("11. repeated SELL while FLAT creates NO invalid trade",
       nrow(sells_in_e4) == 1 &&
         identical(format(sells_in_e4$execution_date), "2024-01-06") &&
         nrow(r4$trade_history) == 3)  # BUY, SELL, BUY only

# 12. Open final position is marked to market (cash + shares * last close).
r2 <- execute_backtest(E1_ohlcv, E2_signals, E1_cap)
last_eq <- r2$equity_curve[nrow(r2$equity_curve), ]
record("12. open final position marked to market (4 + 98*114 = 11176)",
       expect_equal_num(last_eq$portfolio_value, 11176) &&
         expect_equal_num(r2$final_value, 11176) &&
         expect_equal_num(r2$open_position$current_value, 98 * 114))

# 13. Open final position is NOT a completed trade.
record("13. open final position is not a completed trade",
       nrow(r2$completed_trades) == 0 &&
         !is.null(r2$open_position) &&
         nrow(r2$trade_history) == 1)

# 14. Completed round trip has the correct P/L and return.
rt <- r1$completed_trades
record("14. completed round trip P/L = (104-102)*98 = 196, return = 2/102",
       nrow(rt) == 1 &&
         expect_equal_num(rt$profit_loss, 196) &&
         expect_equal_num(rt$return, 2 / 102) &&
         expect_equal_num(rt$shares, 98) &&
         identical(format(rt$entry_date), "2024-01-03") &&
         identical(format(rt$exit_date), "2024-01-05"))

# 15. Multiple round trips are tracked independently.
r3 <- execute_backtest(E3_ohlcv, E3_signals, E1_cap)
rt3 <- r3$completed_trades
record("15. multiple round trips tracked independently",
       nrow(rt3) == 2 &&
         expect_equal_num(rt3$entry_price[1], 102) &&
         expect_equal_num(rt3$exit_price[1], 104) &&
         expect_equal_num(rt3$profit_loss[1], 196) &&
         expect_equal_num(rt3$entry_price[2], 106) &&
         expect_equal_num(rt3$exit_price[2], 108) &&
         expect_equal_num(rt3$profit_loss[2], 192) &&
         expect_equal_num(rt3$return[2], 2 / 106))

# 16. Trade IDs are deterministic and assigned in execution order.
record("16. trade ids are sequential, deterministic and chronological",
       identical(r3$trade_history$trade_id, 1:4) &&
         all(diff(as.numeric(r3$trade_history$execution_date)) > 0))

# ===========================================================================
# 17-22: EQUITY CURVE
# ===========================================================================

pv3 <- r3$equity_curve

# 17. One equity observation per market-data date.
record("17. one equity observation per market date",
       nrow(pv3) == NROW(E3_ohlcv) && nrow(r_no_sig$equity_curve) == 5)

# 18. portfolio_value == cash + shares * close, for every row.
record("18. portfolio_value = cash + shares * close on every row",
       all(expect_equal_num(pv3$portfolio_value,
                            pv3$cash + pv3$shares * pv3$close)))

# 19. Daily return = pv[t]/pv[t-1] - 1.
expected_dr <- c(NA_real_,
                 pv3$portfolio_value[-1] / pv3$portfolio_value[-nrow(pv3)] - 1)
record("19. daily_return = pv[t]/pv[t-1] - 1",
       all(expect_equal_num(pv3$daily_return, expected_dr)))

# 20. First-day return is NA (documented), never silently zero.
record("20. first-day daily_return is NA, not 0",
       is.na(pv3$daily_return[1]) &&
         !expect_equal_num(pv3$daily_return[1], 0))

# 21. Equity curve is strictly chronological and aligned with the data.
record("21. equity curve is chronological and aligned with input dates",
       identical(format(pv3$date), format(index(E3_ohlcv))) &&
         all(diff(as.numeric(pv3$date)) > 0))

# 22. Final portfolio value is correct.
record("22. final portfolio value correct (10388 for E3)",
       expect_equal_num(r3$final_value, 10388) &&
         expect_equal_num(tail(pv3$portfolio_value, 1), 10388))

# ===========================================================================
# EDGE CASES
# ===========================================================================

# 23. Signal on the FINAL candle is never executed (BUY at d5 -> flat, 0 trades).
r_e6a <- execute_backtest(E1_ohlcv, E6a_signals, E1_cap)
record("23. BUY signal on final candle is NOT executed (no fabricated day)",
       nrow(r_e6a$trade_history) == 0 &&
         nrow(r_e6a$completed_trades) == 0 &&
         is.null(r_e6a$open_position) &&
         all(r_e6a$equity_curve$portfolio_value == E1_cap))

# 24. SELL on final candle not executed; prior BUY stays open (marked to market).
r_e6b <- execute_backtest(E1_ohlcv, E6b_signals, E1_cap)
record("24. SELL on final candle not executed; open position marked to market",
       nrow(r_e6b$trade_history) == 1 &&
         identical(r_e6b$trade_history$side, "BUY") &&
         nrow(r_e6b$completed_trades) == 0 &&
         !is.null(r_e6b$open_position) &&
         expect_equal_num(r_e6b$final_value, 4 + 98 * 114))

# 25. Insufficient cash for one share -> BUY skipped, capital untouched.
r5 <- execute_backtest(E1_ohlcv, E1_signals, 100)  # floor(100/102) = 0
record("25. insufficient cash: BUY skipped, capital unchanged",
       nrow(r5$trade_history) == 0 &&
         all(r5$equity_curve$portfolio_value == 100))

# 26. Insufficient data for strategy warm-up -> all signals NA -> flat.
r6 <- execute_backtest(E1_ohlcv, rep(NA_character_, 5), E1_cap)
record("26. insufficient warm-up (all-NA signals) -> flat, no trades",
       nrow(r6$trade_history) == 0 && all(r6$equity_curve$portfolio_value == E1_cap))

# 27. Empty price data -> clear error.
empty_xts <- xts::xts(
  matrix(numeric(0), nrow = 0, ncol = 2,
         dimnames = list(NULL, c("Open", "Close"))),
  order.by = as.Date(character(0)))
record("27. empty price data rejected with an error",
       inherits(try(execute_backtest(empty_xts, character(0), E1_cap), silent = TRUE),
                "try-error"))

# 28. Invalid initial capital rejected.
record("28. invalid initial capital rejected (0, negative, NA, Inf)",
       inherits(try(execute_backtest(E1_ohlcv, E1_signals, 0), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(E1_ohlcv, E1_signals, -10), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(E1_ohlcv, E1_signals, NA_real_), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(E1_ohlcv, E1_signals, Inf), silent = TRUE), "try-error"))

# 29. Invalid transaction-cost models rejected (never silently accepted).
record("29. invalid cost models rejected",
       inherits(try(execute_backtest(E1_ohlcv, E1_signals, E1_cap, -0.01), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(E1_ohlcv, E1_signals, E1_cap, 1.5), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(E1_ohlcv, E1_signals, E1_cap, "x"), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(E1_ohlcv, E1_signals, E1_cap,
                                       list(rate = -1)), silent = TRUE), "try-error"))

# 30. NA / non-positive prices rejected.
bad_na <- E1_ohlcv; bad_na[3, "Close"] <- NA
bad_zero <- E1_ohlcv; bad_zero[2, "Open"] <- 0
record("30. NA and non-positive prices rejected",
       inherits(try(execute_backtest(bad_na, E1_signals, E1_cap), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(bad_zero, E1_signals, E1_cap), silent = TRUE), "try-error"))

# 31. Signals with mismatched index / length / values rejected.
mismatch_signals <- xts::xts(c("BUY", NA, NA, NA, NA), order.by = START + 1:5)
short_signals <- c("BUY", "SELL")
record("31. mismatched / invalid signals rejected",
       inherits(try(execute_backtest(E1_ohlcv, mismatch_signals, E1_cap), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(E1_ohlcv, short_signals, E1_cap), silent = TRUE), "try-error") &&
         inherits(try(execute_backtest(E1_ohlcv, c("HOLD", NA, NA, NA, NA), E1_cap), silent = TRUE), "try-error"))

# 32. Plain character-vector signals produce the identical result to xts.
sig_xts <- xts::xts(E1_signals, order.by = index(E1_ohlcv))
r_vec <- execute_backtest(E1_ohlcv, E1_signals, E1_cap)
r_xts <- execute_backtest(E1_ohlcv, sig_xts, E1_cap)
record("32. character-vector and xts signals are equivalent",
       identical(r_vec$trade_history, r_xts$trade_history) &&
         expect_equal_num(r_vec$final_value, r_xts$final_value))

# 33. Non-zero transaction cost is applied (extensibility; Review 1 uses 0).
# rate 0.001: q = floor(10000 / (102*1.001)) = 97;
# cash_after = 10000 - 97*102 - 97*102*0.001 = 96.106
r_cost <- execute_backtest(E1_ohlcv, E1_signals, E1_cap, transaction_cost = 0.001)
record("33. non-zero cost rate sizes shares and cash correctly (extensibility)",
       expect_equal_num(r_cost$trade_history$shares[1], 97) &&
         expect_equal_num(r_cost$trade_history$cash_after[1],
                          10000 - 97 * 102 - 97 * 102 * 0.001, tol = 1e-9) &&
         all(r_cost$equity_curve$cash >= 0))

# 34. Same inputs -> identical result object (determinism, no randomness).
r_again <- execute_backtest(E1_ohlcv, E1_signals, E1_cap)
record("34. execution is deterministic (two runs identical)",
       identical(r1, r_again) && identical(r3, execute_backtest(E3_ohlcv, E3_signals, E1_cap)))

# ===========================================================================
# Summary
# ===========================================================================
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d execution tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL EXECUTION TESTS PASSED\n")
quit(status = 0)