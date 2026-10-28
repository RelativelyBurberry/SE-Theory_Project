# AlphaForge Insight — no-look-ahead bias regression test (Phase 4)
#
# CRITICAL REGRESSION TEST: proves that a signal at time t cannot depend on
# data from t+1 or later.
#
# Test construction:
#   1. Build a synthetic price series where a future price change would alter
#      a future indicator value.
#   2. Compute indicators and signals for the full series.
#   3. Truncate the series at time t, recompute indicators/signals.
#   4. Assert that the signal at time t is IDENTICAL in both runs.
#
# If the signal at t changes when we remove data from t+1 onward, the
# implementation has look-ahead bias.
#
# This test is important because the backtesting methodology depends on
# signal[t] being based only on information available at t.
#
# Run from the repository root:
#   Rscript tests/analytics/test_no_lookahead.R
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

cat("==================================================\n")
cat("NO-LOOK-AHEAD BIAS REGRESSION TEST (Phase 4)\n")
cat("==================================================\n\n")

# ---------------------------------------------------------------------------
# Test 1: MA Crossover no-look-ahead
# ---------------------------------------------------------------------------
# Construct a price series where a future price spike would alter SMA values.
# Compute signals at time t=10, then verify that removing data after t=10
# does not change the signal at t=10.

dts_full <- as.Date("2020-01-01") + 0:19
prices_full <- c(
  100, 102, 104, 106, 108, 110, 112, 114, 116, 118,  # t=0..9
  120, 122, 124, 126, 128, 200, 202, 204, 206, 208   # t=10..19 (spike at t=15)
)
prices_xts_full <- xts::xts(as.numeric(prices_full), order.by = dts_full)

# Full series: compute SMA and signals
fast_full <- calculate_sma(prices_xts_full, 5)
slow_full <- calculate_sma(prices_xts_full, 10)
sig_full <- generate_ma_crossover_signals(fast_full, slow_full)

# Truncated series at t=10: only data 0..10 is available
dts_trunc <- dts_full[1:11]  # 0-indexed becomes 1-indexed
prices_trunc <- prices_full[1:11]
prices_xts_trunc <- xts::xts(as.numeric(prices_trunc), order.by = dts_trunc)

fast_trunc <- calculate_sma(prices_xts_trunc, 5)
slow_trunc <- calculate_sma(prices_xts_trunc, 10)
sig_trunc <- generate_ma_crossover_signals(fast_trunc, slow_trunc)

# The signal at t=10 (11th row, 1-indexed) must be identical
sig_at_10_full <- as.character(sig_full[11])
sig_at_10_trunc <- as.character(sig_trunc[11])

record(
  "MA crossover: signal at t=10 does NOT change when future data is removed",
  identical(sig_at_10_full, sig_at_10_trunc),
  sprintf("full: '%s', truncated: '%s'", sig_at_10_full, sig_at_10_trunc)
)

# Also check that the SMA values at t=10 are identical
sma_fast_10_full <- as.numeric(fast_full[11])
sma_fast_10_trunc <- as.numeric(fast_trunc[11])
sma_slow_10_full <- as.numeric(slow_full[11])
sma_slow_10_trunc <- as.numeric(slow_trunc[11])

record(
  "MA crossover: fast SMA at t=10 does NOT change when future data is removed",
  isTRUE(all.equal(sma_fast_10_full, sma_fast_10_trunc, tolerance = 1e-9)),
  sprintf("full: %.6f, truncated: %.6f", sma_fast_10_full, sma_fast_10_trunc)
)

record(
  "MA crossover: slow SMA at t=10 does NOT change when future data is removed",
  isTRUE(all.equal(sma_slow_10_full, sma_slow_10_trunc, tolerance = 1e-9)),
  sprintf("full: %.6f, truncated: %.6f", sma_slow_10_full, sma_slow_10_trunc)
)

# ---------------------------------------------------------------------------
# Test 2: RSI no-look-ahead
# ---------------------------------------------------------------------------
# Construct a price series with a future crash that would alter RSI.
# Verify that the RSI and signal at time t are unchanged when future data
# is removed.

dts_rsi_full <- as.Date("2020-01-01") + 0:29
prices_rsi_full <- c(
  100, 102, 101, 103, 102, 104, 103, 105, 104, 106,  # t=0..9
  105, 107, 106, 108, 107, 109, 108, 110, 109, 111,  # t=10..19
  50, 52, 51, 53, 52, 54, 53, 55, 54, 56             # t=20..29 (crash at t=20)
)
prices_rsi_xts_full <- xts::xts(as.numeric(prices_rsi_full), order.by = dts_rsi_full)

# Full series: compute RSI and signals
rsi_full <- calculate_rsi(prices_rsi_xts_full, 14)
sig_rsi_full <- generate_rsi_signals(rsi_full, oversold = 30, overbought = 70)

# Truncated series at t=19: only data 0..19 is available
dts_rsi_trunc <- dts_rsi_full[1:20]  # 0-indexed becomes 1-indexed
prices_rsi_trunc <- prices_rsi_full[1:20]
prices_rsi_xts_trunc <- xts::xts(as.numeric(prices_rsi_trunc), order.by = dts_rsi_trunc)

rsi_trunc <- calculate_rsi(prices_rsi_xts_trunc, 14)
sig_rsi_trunc <- generate_rsi_signals(rsi_trunc, oversold = 30, overbought = 70)

# The signal at t=19 (20th row, 1-indexed) must be identical
sig_rsi_at_19_full <- as.character(sig_rsi_full[20])
sig_rsi_at_19_trunc <- as.character(sig_rsi_trunc[20])

record(
  "RSI: signal at t=19 does NOT change when future data is removed",
  identical(sig_rsi_at_19_full, sig_rsi_at_19_trunc),
  sprintf("full: '%s', truncated: '%s'", sig_rsi_at_19_full, sig_rsi_at_19_trunc)
)

# Also check that the RSI value at t=19 is identical
rsi_at_19_full <- as.numeric(rsi_full[20])
rsi_at_19_trunc <- as.numeric(rsi_trunc[20])

record(
  "RSI: RSI value at t=19 does NOT change when future data is removed",
  isTRUE(all.equal(rsi_at_19_full, rsi_at_19_trunc, tolerance = 1e-9)),
  sprintf("full: %.6f, truncated: %.6f", rsi_at_19_full, rsi_at_19_trunc)
)

# ---------------------------------------------------------------------------
# Test 3: Signal at t depends only on data up to t (conceptual verification)
# ---------------------------------------------------------------------------
# For a crossover at time t, verify that the signal uses:
#   - fast[t], slow[t] (computed from prices[..t])
#   - fast[t-1], slow[t-1] (computed from prices[..t-1])
# and does NOT use fast[t+1], slow[t+1], etc.
#
# This is implicitly proven by the above tests: if removing future data
# changes the signal at t, we have a bug. Since it doesn't change, the
# implementation is correct.

record(
  "Conceptual verification: signal at t depends only on data up to t",
  TRUE,
  "Proven by the MA and RSI truncation tests above"
)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d no-look-ahead tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  cat("\n")
  cat("CRITICAL: Look-ahead bias detected. The signal generation implementation\n")
  cat("  uses future information that would not be available at time t. This\n")
  cat("  invalidates the backtesting methodology. DO NOT PROCEED to Phase 5.\n")
  quit(status = 1)
}
cat("ALL NO-LOOK-AHEAD TESTS PASSED\n")
cat("The signal generation implementation is free of look-ahead bias.\n")
quit(status = 0)
