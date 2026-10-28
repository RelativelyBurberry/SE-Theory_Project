# AlphaForge Insight — Phase 3 indicator unit tests
#
# DETERMINISTIC: no network access. Fixture data only. Validates the SMA and
# RSI implementations against known values, warm-up/NA behaviour, timestamp
# alignment, and parameter validation.
#
# RSI convention under test: TTR::RSI default (Wilder / RMA smoothing).
# The exact values are pinned against a manual Wilder recursion computed in
# this file, so correctness does not rely on "TTR said so".
#
# Run from the repository root:
#   Rscript tests/analytics/test_indicators.R
#
# Exit code 0 = pass, 1 = fail. Requires: TTR, xts, zoo.

suppressPackageStartupMessages(source("backend/analytics/indicators.R"))

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
# Fixtures
# ---------------------------------------------------------------------------
dts <- as.Date("2020-01-01") + 0:29
prices_up <- as.numeric(1:30)                    # strictly increasing
prices_alt <- rep(c(10, 11), 15)                 # alternating up/down
prices_flat <- rep(25, 30)                       # constant
xts_up <- xts::xts(prices_up, order.by = dts)

# ---------------------------------------------------------------------------
# 1. SMA — known values
# ---------------------------------------------------------------------------
sma2 <- calculate_sma(c(1, 2, 3, 4, 5), 2)
record("SMA: known values c(1..5), n=2 -> c(NA,1.5,2.5,3.5,4.5)",
       expect_equal_num(sma2, c(NA, 1.5, 2.5, 3.5, 4.5)))
record("SMA: warm-up leaves exactly (period-1) leading NAs",
       sum(is.na(as.numeric(calculate_sma(prices_up, 20)))) == 19)

# 2. SMA — insufficient warm-up
sma_too_long <- calculate_sma(prices_up, 50)
record("SMA: period > series length -> all-NA aligned series",
       length(sma_too_long) == 30 && all(is.na(as.numeric(sma_too_long))))

# 3. SMA — index alignment on xts input
sma_xts <- calculate_sma(xts_up, 5)
record("SMA: keeps source timestamp index",
       identical(format(index(sma_xts)), format(dts)) &&
         sum(!is.na(as.numeric(sma_xts))) == 26)

# 4. SMA — NAs are never replaced with zeros
record("SMA: warm-up NAs are NA, not 0",
       any(is.na(as.numeric(sma2))) && !any(as.numeric(sma2)[1:1] == 0, na.rm = TRUE))

# 5. SMA — parameter validation
record("SMA: period 0 rejected", inherits(try(calculate_sma(prices_up, 0), silent = TRUE), "try-error"))
record("SMA: negative period rejected", inherits(try(calculate_sma(prices_up, -3), silent = TRUE), "try-error"))
record("SMA: non-integer period rejected", inherits(try(calculate_sma(prices_up, 2.5), silent = TRUE), "try-error"))
record("SMA: NA period rejected", inherits(try(calculate_sma(prices_up, NA_real_), silent = TRUE), "try-error"))

# ---------------------------------------------------------------------------
# 6. RSI — exact Wilder convention (manual reference recursion)
# ---------------------------------------------------------------------------
# Wilder's RSI (TTR::RSI default). Reference (matches TTR's exact seeding):
#   delta[t] = close[t] - close[t-1]
#   avg_gain = SMA(gains[1:n]); avg_loss = SMA(losses[1:n])
#   RSI[n+1] = f(avg_gain, avg_loss)           # seeded, no smoothing input
#   for t in (n+2)..(m+1):                     # uses delta[t-1]
#     avg_gain = (avg_gain*(n-1) + gains[t-1]) / n
#     avg_loss = (avg_loss*(n-1) + losses[t-1]) / n
#     RSI[t]   = f(avg_gain, avg_loss)
#   where f: 100 if loss==0; 0 if gain==0; else 100 - 100/(1+gain/loss).
# RSI values are defined from index n+1 onward (first n values NA).
wilder_rsi_reference <- function(closes, n) {
  deltas <- diff(as.numeric(closes))
  gains <- pmax(deltas, 0)
  losses <- pmax(-deltas, 0)
  m <- length(gains)
  out <- rep(NA_real_, length(closes))
  if (m < n) return(out)
  ag <- mean(gains[1:n])
  al <- mean(losses[1:n])
  rsi_at <- function(ag, al) {
    if (ag + al == 0) return(NA_real_)
    if (al == 0) return(100)
    if (ag == 0) return(0)
    100 - 100 / (1 + ag / al)
  }
  out[n + 1] <- rsi_at(ag, al)
  if (m >= n + 1) {
    for (i in (n + 2):(m + 1)) {
      ag <- (ag * (n - 1) + gains[i - 1]) / n
      al <- (al * (n - 1) + losses[i - 1]) / n
      out[i] <- rsi_at(ag, al)
    }
  }
  out
}

rsi_up <- calculate_rsi(prices_up, 14)
rsi_alt <- calculate_rsi(prices_alt, 14)

record("RSI: exact Wilder values match manual recursion (increasing series)",
       expect_equal_num(rsi_up, wilder_rsi_reference(prices_up, 14)))
record("RSI: exact Wilder values match manual recursion (alternating series)",
       expect_equal_num(rsi_alt, wilder_rsi_reference(prices_alt, 14)))

# 7. RSI — bounds and degenerate cases
record("RSI: all values within [0, 100]",
       all(is.na(as.numeric(rsi_alt)) | (as.numeric(rsi_alt) >= 0 & as.numeric(rsi_alt) <= 100)))
rsi_down <- calculate_rsi(rev(prices_up), 14)
record("RSI: strictly decreasing series -> RSI = 0 at the tail",
       tail(as.numeric(rsi_down), 1) == 0)
record("RSI: strictly increasing series -> RSI = 100 at the tail",
       tail(as.numeric(rsi_up), 1) == 100)
rsi_flat <- calculate_rsi(prices_flat, 14)
record("RSI: constant (flat) series -> NA (no up/down movement)",
       all(is.na(as.numeric(rsi_flat))))

# 8. RSI — warm-up / NA handling
record("RSI: first `period` values are NA (warm-up)",
       sum(is.na(as.numeric(rsi_up))) == 14)
record("RSI: warm-up NAs are NA, never 0",
       !any(as.numeric(rsi_up)[1:14] %in% 0))
rsi_too_long <- calculate_rsi(prices_up, 40)
record("RSI: period > series length -> all-NA aligned series",
       length(rsi_too_long) == 30 && all(is.na(as.numeric(rsi_too_long))))

# 9. RSI — timestamp alignment
rsi_xts <- calculate_rsi(xts_up, 14)
record("RSI: keeps source timestamp index",
       identical(format(index(rsi_xts)), format(dts)) &&
         !is.null(index(rsi_xts)) && length(index(rsi_xts)) == length(dts))

# 10. RSI — parameter validation
record("RSI: period 0 rejected", inherits(try(calculate_rsi(prices_up, 0), silent = TRUE), "try-error"))
record("RSI: non-integer period rejected", inherits(try(calculate_rsi(prices_up, 7.5), silent = TRUE), "try-error"))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d indicator tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL INDICATOR TESTS PASSED\n")
quit(status = 0)