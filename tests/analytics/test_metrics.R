# AlphaForge Insight — performance-metric unit tests (Phase 5)
#
# DETERMINISTIC: no network access, no database. Every metric is checked
# against a HAND-COMPUTED expected value (not merely "not NA").
#
# Conventions under test (documented in backend/analytics/metrics.R):
#   total_return  = V_T / V_0 - 1                          (decimal)
#   cagr          = (V_T / V_0)^(1/years) - 1,             (decimal)
#                   years = (end - start days) / 365.25
#   volatility    = sd(r_t) * sqrt(252)                    (decimal)
#   sharpe        = mean(excess)/sd(excess) * sqrt(252),
#                   excess = r_t - daily_rf, daily_rf = (1+0.02)^(1/252) - 1
#   sortino       = (mean(r_t) - daily_rf)/downside_dev * sqrt(252),
#                   downside_dev = sqrt(mean(min(r_t - daily_rf, 0)^2))
#   max_drawdown  = min(V_t / cummax(V) - 1)               (negative decimal)
#   win_rate      = profitable completed round trips / total completed
#   total_trades  = number of completed round trips
#
# Run from the repository root:
#   Rscript tests/analytics/test_metrics.R
#
# Exit code 0 = pass, 1 = fail. Requires: xts (via execution.R).

suppressPackageStartupMessages(source("backend/analytics/metrics.R"))
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
# Fixtures
# ---------------------------------------------------------------------------
# Daily-return fixture used for volatility / Sharpe / Sortino:
r_test <- c(0.02, -0.01, 0.015, 0.005, -0.02)
# Hand-computed reference values (verified independently in R):
RF_DAILY_REF <- 0.000078584942      # (1.02)^(1/252) - 1
VOL_REF      <- 0.2668145423        # sd(r_test) * sqrt(252)
SHARPE_REF   <- 1.8147309003        # mean(excess)/sd(excess) * sqrt(252)
SORTINO_REF  <- 3.0358338636        # (mean(r)-rf)/downside_dev * sqrt(252)

pv_known   <- c(10000, 11000)          # +10% total return
pv_loss    <- c(10000, 9000)           # -10% total return
pv_dd      <- c(100, 110, 90, 95, 80, 85)  # peak 110 -> trough 80 = -27.27%
pv_up      <- c(100, 101, 102, 103, 104)   # never declines -> MaxDD 0

# A hand-built completed-trade table (columns used: profit_loss, shares).
make_ct <- function(pnl) {
  data.frame(round_trip_id = seq_along(pnl),
             entry_date = as.Date("2024-01-01") + seq_along(pnl),
             entry_price = rep(100, length(pnl)),
             exit_date = as.Date("2024-02-01") + seq_along(pnl),
             exit_price = rep(110, length(pnl)),
             shares = rep(10, length(pnl)),
             profit_loss = as.numeric(pnl),
             return = as.numeric(pnl) / 1000,
             stringsAsFactors = FALSE)
}

cat("==================================================\n")
cat("PERFORMANCE METRIC UNIT TESTS (Phase 5)\n")
cat("==================================================\n\n")

# ===========================================================================
# 23-30: THE EIGHT METRICS
# ===========================================================================

# ---------- Total Return ----------
record("23a. total return: 10000 -> 11000 is +0.10 (decimal)",
       expect_equal_num(metric_total_return(pv_known, 10000), 0.10))
record("23b. total return: 10000 -> 9000 is -0.10 (decimal)",
       expect_equal_num(metric_total_return(pv_loss, 10000), -0.10))
record("23c. total return: flat curve is 0",
       expect_equal_num(metric_total_return(c(10000, 10000, 10000), 10000), 0))

# ---------- CAGR ----------
p1 <- as.Date("2020-01-01"); p2 <- as.Date("2021-01-01")  # 366 days / 365.25
record("24a. CAGR: 10000 -> 11000 over 2020-01-01..2021-01-01 = 0.099785182458",
       expect_equal_num(metric_cagr(pv_known, 10000, p1, p2), 0.099785182458,
                        tol = 1e-9))
record("24b. CAGR: flat curve is 0",
       expect_equal_num(metric_cagr(c(10000, 10000), 10000, p1, p2), 0, tol = 1e-9))
record("24c. CAGR: zero/negative duration returns NA (not Inf)",
       is.na(metric_cagr(pv_known, 10000, p2, p2)) &&   # years == 0
         is.na(metric_cagr(pv_known, 10000, p2, p1)))   # years < 0
record("24d. CAGR: non-positive final value returns NA",
       is.na(metric_cagr(c(10000, 0), 10000, p1, p2)) &&
         is.na(metric_cagr(c(10000, -5), 10000, p1, p2)))

# ---------- Volatility ----------
record("25a. volatility: sd(r_test) * sqrt(252) = 0.2668145423",
       expect_equal_num(metric_volatility(r_test), VOL_REF, tol = 1e-8))
record("25b. volatility: constant returns -> 0 (defined, not NA)",
       expect_equal_num(metric_volatility(c(0, 0, 0, 0)), 0, tol = 1e-12))
record("25c. volatility: fewer than 2 observations -> NA",
       is.na(metric_volatility(c(0.01))) &&
         is.na(metric_volatility(c(NA_real_, 0.01))))

# ---------- Sharpe ----------
record("26a. sharpe: hand-computed = 1.8147309003 (daily rf compounded)",
       expect_equal_num(metric_sharpe(r_test, 0.02), SHARPE_REF, tol = 1e-7))
record("26b. sharpe: constant excess (zero variance) -> NA, not Inf",
       is.na(metric_sharpe(rep(0, 10), 0.02)) &&
         is.na(metric_sharpe(rep(0.01, 10), 0.02)))
record("26c. sharpe: fewer than 2 observations -> NA",
       is.na(metric_sharpe(c(0.01), 0.02)) &&
         is.na(metric_sharpe(c(NA_real_, 0.01), 0.02)))
record("26d. sharpe uses the EXACT daily conversion, not the linear 0.02/252",
       {
         sharpe_simple <- {  # linear conversion kept here ONLY to prove it differs
           rf_simple <- 0.02 / 252
           exc <- r_test - rf_simple
           mean(exc) / stats::sd(exc) * sqrt(252)
         }
         !expect_equal_num(metric_sharpe(r_test, 0.02), sharpe_simple, tol = 1e-4)
       })

# ---------- Sortino ----------
record("27a. sortino: hand-computed = 3.0358338636 (MAR = daily rf)",
       expect_equal_num(metric_sortino(r_test, 0.02), SORTINO_REF, tol = 1e-7))
record("27b. sortino: zero downside deviation -> NA, not Inf",
       is.na(metric_sortino(c(0.01, 0.02, 0.015), 0.02)))
record("27c. sortino: fewer than 2 observations -> NA",
       is.na(metric_sortino(c(0.01), 0.02)))
record("27d. sortino: constant equity curve -> finite -15.874508 (all days downside)",
       expect_equal_num(metric_sortino(rep(0, 5), 0.02), -15.874508, tol = 1e-5))
record("27e. sortino: denominator is NOT the Sharpe denominator (downside only)",
       !isTRUE(all.equal(metric_sortino(r_test, 0.02),
                         metric_sharpe(r_test, 0.02))))

# ---------- Maximum Drawdown ----------
record("28a. max drawdown: peak 110 -> trough 80 = -0.2727272727 (decimal)",
       expect_equal_num(metric_max_drawdown(pv_dd), -0.272727272727, tol = 1e-9))
record("28b. max drawdown: never-declining curve -> 0",
       expect_equal_num(metric_max_drawdown(pv_up), 0, tol = 1e-12) &&
         expect_equal_num(metric_max_drawdown(rep(100, 5)), 0, tol = 1e-12))
record("28c. max drawdown: magnitude helper returns positive percentage",
       expect_equal_num(drawdown_magnitude_pct(-0.272727272727), 27.2727272727,
                        tol = 1e-9) &&
         expect_equal_num(drawdown_magnitude_pct(0), 0))
record("28d. max drawdown: NA input -> NA",
       is.na(metric_max_drawdown(c(100, NA, 90))) &&
         is.na(metric_max_drawdown(numeric(0))))

# ---------- Win Rate ----------
record("29a. win rate: mixed trades c(100,-50,250) = 2/3",
       expect_equal_num(metric_win_rate(make_ct(c(100, -50, 250))), 2 / 3,
                        tol = 1e-9))
record("29b. win rate: all winning -> 1.0",
       expect_equal_num(metric_win_rate(make_ct(c(1, 2, 3))), 1))
record("29c. win rate: all losing -> 0.0",
       expect_equal_num(metric_win_rate(make_ct(c(-1, -2))), 0))
record("29d. win rate: no completed trades -> NA (documented), not 0",
       is.na(metric_win_rate(make_ct(numeric(0)))) &&
         is.na(metric_win_rate(NULL)))

# ---------- Total Trades ----------
record("30a. total trades: 3 completed round trips -> 3 (not 6 executions)",
       expect_equal_num(metric_total_trades(make_ct(c(1, 2, 3))), 3) &&
         expect_equal_num(metric_total_trades(make_ct(numeric(0))), 0))

# ===========================================================================
# EDGE CASES AND INTEGRATION CHECKS
# ===========================================================================

# 31. Summary computation on a realistic equity curve (E1 from execution tests).
E1_ohlcv <- xts::xts(cbind(Open = c(100, 101, 102, 103, 104),
                           Close = c(110, 111, 112, 113, 114)),
                     order.by = as.Date("2024-01-01") + 0:4)
E1_signals <- c(NA_character_, "BUY", NA_character_, "SELL", NA_character_)
ex1 <- execute_backtest(E1_ohlcv, E1_signals, 10000)
sum1 <- compute_performance_metrics(ex1$equity_curve, ex1$completed_trades,
                                    10000, "2024-01-01", "2024-01-05")
record("31a. summary: final_value and total_return correct (10196 / +0.0196)",
       expect_equal_num(sum1$final_value, 10196) &&
         expect_equal_num(sum1$total_return, 0.0196))
record("31b. summary: win rate 1.0, total trades 1, max drawdown < 0",
       expect_equal_num(sum1$win_rate, 1) &&
         expect_equal_num(sum1$total_trades, 1) &&
         sum1$maximum_drawdown <= 0)

# 32. Constant equity curve: every metric is defined or documented-NA; never
#     Inf/NaN. (pv constant -> total return 0, cagr 0, vol 0, maxdd 0,
#     sharpe NA zero-variance, sortino finite negative, win rate NA no trades.)
flat_eq <- data.frame(
  date = as.Date("2024-01-01") + 0:9,
  cash = rep(10000, 10), shares = rep(0, 10), close = rep(100, 10),
  portfolio_value = rep(10000, 10),
  daily_return = c(NA_real_, rep(0, 9)),
  stringsAsFactors = FALSE)
sum_flat <- compute_performance_metrics(flat_eq, make_ct(numeric(0)),
                                        10000, "2024-01-01", "2024-01-10")
record("32. constant equity: total 0, cagr 0, vol 0, maxdd 0, sharpe NA, win NA, trades 0",
       expect_equal_num(sum_flat$total_return, 0, tol = 1e-12) &&
         expect_equal_num(sum_flat$cagr, 0, tol = 1e-12) &&
         expect_equal_num(sum_flat$volatility, 0, tol = 1e-12) &&
         expect_equal_num(sum_flat$maximum_drawdown, 0, tol = 1e-12) &&
         is.na(sum_flat$sharpe) &&
         !is.na(sum_flat$sortino) &&
         is.na(sum_flat$win_rate) &&
         expect_equal_num(sum_flat$total_trades, 0))

# 33. No Inf or NaN leaks into any normal summary value.
vals <- unlist(sum1); vals_flat <- unlist(sum_flat)
record("33. no Inf/NaN anywhere in summaries (finite or documented NA)",
       !any(is.infinite(vals)) && !any(is.nan(vals)) &&
         !any(is.infinite(vals_flat)) && !any(is.nan(vals_flat)))

# 34. Open final position is excluded from Win Rate and Total Trades.
E2_ohlcv <- E1_ohlcv
E2_signals <- c(NA_character_, "BUY", NA_character_, NA_character_, NA_character_)
ex2 <- execute_backtest(E2_ohlcv, E2_signals, 10000)
sum2 <- compute_performance_metrics(ex2$equity_curve, ex2$completed_trades,
                                    10000, "2024-01-01", "2024-01-05")
record("34. open position excluded: win rate NA, total trades 0, value marked",
       is.na(sum2$win_rate) &&
         expect_equal_num(sum2$total_trades, 0) &&
         expect_equal_num(sum2$final_value, 4 + 98 * 114))

# 35. Total-return is NOT the sum of daily returns (compound vs. cumulative).
#     Equity 10000 -> 11000 -> 9900: sum(daily) != total return.
pv_comp <- c(10000, 11000, 9900)
daily_comp <- c(NA, 0.10, -0.10)
record("35. total return is compounded, not the sum of daily returns",
       expect_equal_num(metric_total_return(pv_comp, 10000), -0.01) &&
         abs(sum(daily_comp, na.rm = TRUE) - (-0.01)) > 0.001 &&
         expect_equal_num(metric_total_return(pv_comp, 10000),
                          prod(1 + daily_comp[-1]) - 1, tol = 1e-12))

# ===========================================================================
# Summary
# ===========================================================================
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d metric tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL METRIC TESTS PASSED\n")
quit(status = 0)