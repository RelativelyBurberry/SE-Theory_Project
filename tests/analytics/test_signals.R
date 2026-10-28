# AlphaForge Insight — Phase 4 signal-unit tests (comprehensive)
#
# DETERMINISTIC: no network access, no trades executed. Verifies that the
# pure signal helpers produce the documented semantics:
#
#   MA crossover (IMPLEMENTATION_PLAN.md § I, § H):
#     - BUY on bullish crossover (fast crosses above slow)
#     - SELL on bearish crossover (fast crosses below slow)
#     - NO repeated signals while condition persists
#     - NA otherwise; warm-up NAs never produce signals
#
#   RSI (Phase 4 crossing semantics):
#     - BUY when crossing INTO oversold (rsi[t-1] > oversold AND rsi[t] <= oversold)
#     - SELL when crossing INTO overbought (rsi[t-1] < overbought AND rsi[t] >= overbought)
#     - NO repeated signals while RSI remains below/above threshold
#     - NA otherwise; warm-up NAs never produce signals
#
# Trade execution (t+1 open) is explicitly OUT of scope until Phase 5.
#
# Run from the repository root:
#   Rscript tests/analytics/test_signals.R
#
# Exit code 0 = pass, 1 = fail. Requires: TTR, xts, zoo.

suppressPackageStartupMessages(source("backend/analytics/indicators.R"))
suppressPackageStartupMessages(source("backend/analytics/signals.R"))

results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

dts <- as.Date("2020-01-01") + 0:8

# ---------------------------------------------------------------------------
# MA crossover
# ---------------------------------------------------------------------------
# Fast crosses ABOVE slow at t=4 (dts[5]).
fast <- c(NA, 2, 3, 4, 5, 6, 7, 8, 9)
slow <- c(NA, 5, 5, 5, 4, 4, 4, 4, 4)
mf <- xts::xts(as.numeric(fast), order.by = dts)
ms <- xts::xts(as.numeric(slow), order.by = dts)
sig <- generate_ma_crossover_signals(mf, ms)
vals <- as.character(sig)
record("ma-signal: BUY exactly on bullish crossover (fast crosses above)",
       identical(vals[5], "BUY") && sum(!is.na(vals)) == 1)

# Strictly-above-afterward series: only the crossover day carries BUY.
fast2 <- c(NA, 2, 3, 4, 5, 5.5, 6, 7, 8)
slow2 <- c(NA, 5, 5, 5, 5, 4, 4, 4, 4)
sig2 <- generate_ma_crossover_signals(xts::xts(as.numeric(fast2), order.by = dts),
                                      xts::xts(as.numeric(slow2), order.by = dts))
vals2 <- as.character(sig2)
record("ma-signal: only the crossover day carries BUY (no persistence)",
       identical(which(vals2 == "BUY"), 6L) && sum(!is.na(vals2)) == 1)

# SELL on bearish crossover at t=6.
fast3 <- c(NA, 8, 8, 8, 8, 8, 3, 3, 3)
slow3 <- c(NA, 5, 5, 5, 5, 5, 5, 5, 5)
sig3 <- generate_ma_crossover_signals(xts::xts(as.numeric(fast3), order.by = dts),
                                      xts::xts(as.numeric(slow3), order.by = dts))
vals3 <- as.character(sig3)
record("ma-signal: SELL exactly on bearish crossover",
       identical(which(vals3 == "SELL"), 7L) && sum(!is.na(vals3)) == 1)

# Warm-up: NA fast/slow never produce signals themselves.
sig4 <- generate_ma_crossover_signals(xts::xts(c(NA, NA, NA, 1, 2, 3, 4, 5, 6), order.by = dts),
                                      xts::xts(c(NA, NA, NA, 9, 9, 9, 1, 1, 1), order.by = dts))
record("ma-signal: warm-up NAs produce NO signals",
       sum(!is.na(as.character(sig4))) == 1 && identical(as.character(sig4)[7], "BUY"))

# Index alignment retained.
record("ma-signal: output index matches input index",
       identical(format(index(sig)), format(dts)))

# Equality transition: fast[t] == slow[t] is NOT a crossover (strict
# inequality required on the current side, per IMPLEMENTATION_PLAN.md §H).
# fast meets slow at t=4 and stays equal afterwards -> no signals at all.
fast_eq <- c(NA, 2, 3, 4, 4, 4, 4, 4, 4)   # fast touches slow at t=4, stays equal
slow_eq <- c(NA, 5, 5, 4, 4, 4, 4, 4, 4)
sig_eq <- generate_ma_crossover_signals(xts::xts(as.numeric(fast_eq), order.by = dts),
                                        xts::xts(as.numeric(slow_eq), order.by = dts))
vals_eq <- as.character(sig_eq)
record("ma-signal: equality (fast == slow) is NOT a crossover (no signal)",
       all(is.na(vals_eq)))

# Equality followed by a rise IS a bullish transition (fast[t-1] <= slow[t-1]
# includes equality): BUY fires when fast then exceeds slow.
# Fixture (verified): fast == slow (4 == 4) at indices 4-5; fast (5) exceeds
# slow (4) at index 6 -> BUY at index 6.
fast_eq_up <- c(NA, 2, 3, 4, 4, 5, 6, 7, 8)
slow_eq_up <- c(NA, 5, 5, 4, 4, 4, 4, 4, 4)
sig_eq_up <- generate_ma_crossover_signals(xts::xts(as.numeric(fast_eq_up), order.by = dts),
                                           xts::xts(as.numeric(slow_eq_up), order.by = dts))
vals_eq_up <- as.character(sig_eq_up)
record("ma-signal: equality then rise IS a bullish crossover (BUY at first exceed)",
       identical(which(vals_eq_up == "BUY"), 6L) && sum(!is.na(vals_eq_up)) == 1)

# Repeated below state: fast stays below slow after a crossover -> only the
# crossover day itself may carry SELL, later below-state days are HOLD/NA.
fast_below <- c(NA, 8, 8, 3, 2, 2, 2, 2, 2)  # SELL at t=3 (crossing down)
slow_below <- c(NA, 5, 5, 5, 5, 5, 5, 5, 5)
sig_below <- generate_ma_crossover_signals(xts::xts(as.numeric(fast_below), order.by = dts),
                                           xts::xts(as.numeric(slow_below), order.by = dts))
vals_below <- as.character(sig_below)
record("ma-signal: repeated below-state does NOT produce repeated SELL",
       identical(which(vals_below == "SELL"), 4L) && sum(!is.na(vals_below)) == 1)

# ---------------------------------------------------------------------------
# RSI crossing semantics (Phase 4: no repeated signals)
# ---------------------------------------------------------------------------
# Phase 4 RSI uses CROSSING semantics:
#   BUY when:  rsi[t-1] > oversold  AND  rsi[t] <= oversold
#   SELL when: rsi[t-1] < overbought AND rsi[t] >= overbought

# Crossing into oversold: BUY only on the transition
rsi_oversold <- xts::xts(c(NA, 40, 35, 25, 20, 20, 20, 35, 50), order.by = dts)
rs_os <- generate_rsi_signals(rsi_oversold, oversold = 30, overbought = 70)
rv_os <- as.character(rs_os)
record("rsi-signal: BUY when crossing INTO oversold (rsi[t-1] > 30, rsi[t] <= 30)",
       identical(which(!is.na(rv_os) & rv_os == "BUY"), 4L))

# NO repeated BUY while RSI remains below oversold
record("rsi-signal: NO repeated BUY while RSI remains below oversold",
       sum(!is.na(rv_os) & rv_os == "BUY") == 1 && all(is.na(rv_os[5:6])))

# Crossing into overbought: SELL only on the transition
rsi_overbought <- xts::xts(c(NA, 50, 65, 75, 80, 80, 80, 65, 50), order.by = dts)
rs_ob <- generate_rsi_signals(rsi_overbought, oversold = 30, overbought = 70)
rv_ob <- as.character(rs_ob)
record("rsi-signal: SELL when crossing INTO overbought (rsi[t-1] < 70, rsi[t] >= 70)",
       identical(which(!is.na(rv_ob) & rv_ob == "SELL"), 4L))

# NO repeated SELL while RSI remains above overbought
record("rsi-signal: NO repeated SELL while RSI remains above overbought",
       sum(!is.na(rv_ob) & rv_ob == "SELL") == 1 && all(is.na(rv_ob[5:6])))

# Staying below oversold never generates signals
rsi_stay_low <- xts::xts(c(NA, 20, 20, 20, 20, 20, 20, 20, 20), order.by = dts)
rs_stay <- generate_rsi_signals(rsi_stay_low, oversold = 30, overbought = 70)
record("rsi-signal: staying below oversold does not repeatedly BUY",
       all(is.na(as.character(rs_stay))))

# Warm-up NAs produce no signals
rsi_warmup <- xts::xts(c(NA, NA, NA, 25, 50, 75, 40, 20, 80), order.by = dts)
rs_warmup <- generate_rsi_signals(rsi_warmup, oversold = 30, overbought = 70)
rv_warmup <- as.character(rs_warmup)
record("rsi-signal: warm-up NAs produce no signals",
       all(is.na(rv_warmup[1:3])))

# Threshold edge cases (exact equality)
rsi_edge <- xts::xts(c(NA, 35, 30, 25, 30, 35, 65, 70, 75), order.by = dts)
rs_edge <- generate_rsi_signals(rsi_edge, oversold = 30, overbought = 70)
rv_edge <- as.character(rs_edge)
record("rsi-signal: crossing to exact threshold produces signal",
       identical(which(rv_edge == "BUY"), 3L) && identical(which(rv_edge == "SELL"), 8L))

# Thresholds are NOT hard-coded (custom values work)
record("rsi-signal: thresholds are NOT hard-coded (custom values)",
       all(is.na(as.character(generate_rsi_signals(rsi_warmup, 10, 90)))))

# Validation: oversold >= overbought rejected
record("rsi-signal: oversold >= overbought rejected",
       inherits(try(generate_rsi_signals(rsi_warmup, 70, 30), silent = TRUE), "try-error"))

# Validation: thresholds outside (0,100) rejected
record("rsi-signal: thresholds outside (0,100) rejected",
       inherits(try(generate_rsi_signals(rsi_warmup, 0, 90), silent = TRUE), "try-error") &&
         inherits(try(generate_rsi_signals(rsi_warmup, 10, 100), silent = TRUE), "try-error"))

# Timestamp alignment retained
record("rsi-signal: output index matches input index",
       identical(format(index(rs_os)), format(dts)) &&
         identical(format(index(rs_ob)), format(dts)))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d signal tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL SIGNAL TESTS PASSED\n")
quit(status = 0)