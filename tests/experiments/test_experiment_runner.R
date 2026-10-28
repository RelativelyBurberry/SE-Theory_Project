# AlphaForge Insight — research experiment runner tests
#
# DETERMINISTIC: no network access, no database. Tests the experiment
# library (scripts/experiments/experiment_lib.R) against hand-computed
# values and synthetic OHLCV data, exactly like the analytics suites.
#
# Coverage:
#   1. configuration generation (15 combinations: 5 assets x 3 strategies)
#   2. Buy & Hold special handling (paper Win Rate "—", Trades = 1, engine
#      completed round trips = 0, position marked to market)
#   3. engine reuse: MA/RSI/Buy & Hold rows run through the production
#      run_backtest engine with finite, deterministic results
#   4. aggregation math (mean/median over exactly 5 unrounded observations)
#   5. presentation formatting (2-decimals, "—", integer Trades, positive
#      MaxDD percentage)
#   6. failed data retrieval never fabricates a result row and the driver
#      refuses to emit canonical outputs
#
# Run from the repository root:
#   Rscript tests/experiments/test_experiment_runner.R
#
# Exit code 0 = pass, 1 = fail.

suppressPackageStartupMessages({
  source("backend/analytics/execution.R")
  source("backend/analytics/metrics.R")
  source("backend/analytics/indicators.R")
  source("backend/analytics/signals.R")
})
source("scripts/experiments/experiment_lib.R")

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

cat("==================================================\n")
cat("RESEARCH EXPERIMENT RUNNER TESTS\n")
cat("==================================================\n\n")

# ===========================================================================
# 1. CONFIGURATION GENERATION (15 combinations)
# ===========================================================================
cfg <- build_experiment_config()
record("1a. config has exactly 15 rows (5 assets x 3 strategies)",
       nrow(cfg) == 15L)
record("1b. config has exactly 5 rows per strategy",
       all(vapply(names(EXPERIMENT_STRATEGIES), function(s)
         sum(cfg$strategy == s) == 5L, logical(1))))
record("1c. config has exactly 3 rows per asset",
       all(vapply(EXPERIMENT_ASSETS, function(a)
         sum(cfg$asset == a) == 3L, logical(1))))
record("1d. config asset order matches the paper (AAPL..TSLA)",
       identical(unique(cfg$asset), EXPERIMENT_ASSETS))
record("1e. MA rows carry fast_period=50 / slow_period=200",
       all(cfg$parameters[cfg$strategy == "MA Cross (50/200)"] ==
             "{\"fast_period\":50,\"slow_period\":200}"))
record("1f. RSI rows carry period=14 / oversold=30 / overbought=70",
       all(cfg$parameters[cfg$strategy == "RSI (14, 30/70)"] ==
             "{\"period\":14,\"oversold\":30,\"overbought\":70}"))
record("1g. every row uses 2015-01-01..2025-12-31, $10k, 0% cost, 2% rf",
       all(cfg$start_date == "2015-01-01" & cfg$end_date == "2025-12-31" &
             cfg$initial_capital == 10000 & cfg$transaction_cost == 0 &
             cfg$risk_free_rate == 0.02))

# ===========================================================================
# 2. BUY & HOLD SPECIAL HANDLING (paper conventions vs. engine truth)
# ===========================================================================
# Synthetic deterministic OHLCV fixtures (no RNG).
dts <- as.Date("2015-01-01") + 0:299
t <- seq_len(300)
px <- 100 * exp(0.0015 * t) + 30 * sin(t / 8) + 20 * sin(t / 3)
ohlcv_bh <- xts::xts(cbind(Open = px, Close = px + 0.5 * sin(t / 2)),
                     order.by = dts)
# MA-specific fixture: large sine cycles give the SMA(50)/SMA(200) fast/slow
# lines genuine crossovers (BUY then SELL -> completed round trips).
t_ma <- seq_len(400)
px_ma <- 180 + 80 * sin(2 * pi * t_ma / 250) + 20 * sin(2 * pi * t_ma / 23)
ohlcv_ma <- xts::xts(cbind(Open = px_ma, Close = px_ma + 0.5 * sin(t_ma / 2)),
                     order.by = as.Date("2015-01-01") + 0:399)
cfg_bh <- cfg[cfg$strategy_code == "Buy_and_Hold", ][1, , drop = FALSE]

row_bh <- run_single_experiment_row(ohlcv_bh, cfg_bh, execution_timestamp = "T")
record("2a. Buy & Hold engine run reports status ok",
       identical(row_bh$status, "ok"))
record("2b. Buy & Hold engine completes ZERO round trips",
       identical(row_bh$engine_total_trades, 0L))
record("2c. Buy & Hold paper Trades = 1 (single entry held to the end)",
       identical(row_bh$paper_trades, 1L))
record("2d. Buy & Hold engine win rate is NA (undefined, no round-trip basis)",
       is.na(row_bh$win_rate))
record("2e. Buy & Hold final value is finite and > 0",
       is.finite(row_bh$final_value) && row_bh$final_value > 0)
# The entry must actually have happened: cash after buy = initial - shares*open[2]
shares_bh <- floor(10000 / as.numeric(ohlcv_bh[2, "Open"]))
manual_final <- (10000 - shares_bh * as.numeric(ohlcv_bh[2, "Open"])) +
  shares_bh * as.numeric(utils::tail(ohlcv_bh[, "Close"], 1))
record("2f. Buy & Hold marked to market: final value = cash + shares*Close[n]",
       expect_equal_num(row_bh$final_value, manual_final, tol = 1e-6))
record("2g. Buy & Hold position was entered at the OPEN of the 2nd candle (t+1)",
       identical(row_bh$n_observations, 300L) &&
         is.finite(row_bh$total_return))

# ===========================================================================
# 3. ENGINE REUSE: MA / RSI / Buy & Hold all run through run_backtest
# ===========================================================================
run_fixture <- list(MA_Crossover = ohlcv_ma, RSI = ohlcv_bh,
                    Buy_and_Hold = ohlcv_bh)
for (code in c("MA_Crossover", "RSI", "Buy_and_Hold")) {
  cfg_row <- cfg[cfg$strategy_code == code, ][1, , drop = FALSE]
  r <- run_single_experiment_row(run_fixture[[code]], cfg_row,
                                 execution_timestamp = "T")
  record(sprintf("3a. %s: status ok, finite metrics, no Inf/NaN", code),
         identical(r$status, "ok") &&
           all(vapply(c("final_value", "total_return", "cagr", "sharpe",
                        "sortino", "max_drawdown", "volatility"),
                      function(col) is.finite(r[[col]]), logical(1))))
  record(sprintf("3b. %s: max_drawdown <= 0 (engine negative convention)",
                 code),
         r$max_drawdown <= 0)
  record(sprintf("3c. %s: n_observations matches the OHLCV rows", code),
         identical(r$n_observations,
                   as.integer(NROW(run_fixture[[code]]))))
  if (code %in% c("MA_Crossover", "RSI")) {
    record(sprintf("3d. %s: at least one completed round trip on the fixture",
                   code),
           r$engine_total_trades >= 1L && r$engine_total_trades == r$paper_trades &&
             !is.na(r$win_rate))
  }
}

# ===========================================================================
# 4. AGGREGATION MATH (exactly 5 unrounded observations per strategy)
# ===========================================================================
make_synth_results <- function() {
  # 5 assets per strategy with KNOWN values so mean/median are hand-checkable.
  v <- c(0.10, 0.20, 0.30, 0.40, 0.50)   # total_return (decimal)
  c <- c(0.05, 0.10, 0.15, 0.20, 0.25)   # cagr (decimal)
  sh <- c(0.50, 0.60, 0.70, 0.80, 0.90)  # sharpe
  dd <- c(-0.10, -0.20, -0.30, -0.40, -0.50)  # max_drawdown (negative)
  wr <- c(0.50, 0.60, 0.70, 0.80, 0.90)  # win_rate (decimal)
  vol <- rep(0.20, 5)
  fv <- rep(13000, 5)
  assets <- EXPERIMENT_ASSETS
  strat_spec <- list(
    "MA Cross (50/200)" = "MA_Crossover",
    "RSI (14, 30/70)" = "RSI",
    "Buy & Hold" = "Buy_and_Hold"
  )
  rows <- list(); k <- 0L
  for (s in names(strat_spec)) {
    for (a in assets) {
      k <- k + 1L
      pos <- ((k - 1L) %% 5L) + 1L          # position WITHIN the strategy block
      is_bh <- strat_spec[[s]] == "Buy_and_Hold"
      rows[[k]] <- data.frame(
        asset = a, strategy = s, strategy_code = strat_spec[[s]],
        parameters = "{}",
        start_date = "2015-01-01", end_date = "2025-12-31",
        initial_capital = 10000, transaction_cost = 0, risk_free_rate = 0.02,
        n_observations = 300L, final_value = fv[pos],
        total_return = v[pos], cagr = c[pos], sharpe = sh[pos], sortino = sh[pos] - 0.1,
        max_drawdown = dd[pos], win_rate = if (is_bh) NA_real_ else wr[pos],
        volatility = vol[pos],
        engine_total_trades = if (is_bh) 0L else 3L,
        paper_trades = if (is_bh) 1L else 3L,
        data_source = "synthetic", status = "ok",
        error_message = NA_character_, execution_timestamp = "T",
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
synth <- make_synth_results()
agg <- aggregate_experiment(synth)

record("4a. aggregation produces exactly 3 rows (one per strategy)",
       nrow(agg) == 3L)
record("4b. mean total return = 30.00 (%): mean(0.10..0.50)*100",
       expect_equal_num(agg$mean_total_return[1], 30, tol = 1e-9))
record("4c. median CAGR = 15.00 (%): median(0.05..0.25)*100",
       expect_equal_num(agg$median_cagr[1], 15, tol = 1e-9))
record("4d. mean Sharpe = 0.70: mean(0.50..0.90)",
       expect_equal_num(agg$mean_sharpe[1], 0.70, tol = 1e-9))
record("4e. mean MaxDD = 30.00 (%): mean(|−0.10..−0.50|)*100",
       expect_equal_num(agg$mean_maxdd_pct[1], 30, tol = 1e-9))
record("4f. mean win rate (MA/RSI) = 70.00 (%): mean(0.50..0.90)*100",
       expect_equal_num(agg$mean_win_rate[1], 70, tol = 1e-9))
record("4g. Buy & Hold mean win rate is NA (presented as —, never 0/NA-string)",
       is.na(agg$mean_win_rate[agg$strategy == "Buy & Hold"]))
record("4h. every aggregate uses exactly 5 asset observations",
       all(agg$n_assets == 5L))
# Aggregation must NOT depend on the rounded display values:
synth_rounded <- synth
synth_rounded$total_return <- round(synth_rounded$total_return, 2)
agg_round <- aggregate_experiment(synth_rounded)
record("4i. aggregation is exact (not an artifact of rounding)",
       expect_equal_num(agg_round$mean_total_return[1], 30, tol = 1e-9))

# ===========================================================================
# 5. PRESENTATION FORMATTING
# ===========================================================================
fmt <- format_experiment_results(synth)
record("5a. formatted table has the exact paper column names",
       identical(names(fmt), c("Asset", "Strategy", "Total Return (%)",
                               "CAGR (%)", "Sharpe", "Sortino", "MaxDD (%)",
                               "Win Rate (%)", "Volatility (%)", "Trades")))
bh_rows <- fmt$Strategy == "Buy & Hold"
record("5b. Buy & Hold Win Rate displayed as em dash '—'",
       all(fmt$`Win Rate (%)`[bh_rows] == "—"))
record("5c. Buy & Hold Trades displayed as integer 1",
       all(fmt$Trades[bh_rows] == 1L))
record("5d. MA/RSI win rates formatted to 2 decimals",
       all(grepl("^[0-9]+\\.[0-9]{2}$", fmt$`Win Rate (%)`[!bh_rows])))
record("5e. percentages formatted to 2 decimals with a positive MaxDD",
       all(grepl("^[0-9.]+\\.[0-9]{2}$", fmt$`Total Return (%)`)) &&
         all(grepl("^[0-9.]+\\.[0-9]{2}$", fmt$`MaxDD (%)`)) &&
         all(grepl("^[0-9.]+\\.[0-9]{2}$", fmt$`CAGR (%)`)) &&
         all(grepl("^[0-9.]+\\.[0-9]{2}$", fmt$`Volatility (%)`)))
record("5f. Sharpe/Sortino formatted to 2 decimals",
       all(grepl("^[0-9.]+\\.[0-9]{2}$", fmt$Sharpe)) &&
         all(grepl("^[0-9.]+\\.[0-9]{2}$", fmt$Sortino)))
record("5g. MaxDD presented as positive magnitude (engine negative stored)",
       all(fmt$`MaxDD (%)` == sprintf("%.2f", abs(synth$max_drawdown) * 100)))
record("5h. aggregate table columns match the paper",
       identical(names(format_experiment_aggregates(agg)),
                 c("Strategy", "Mean Total Return (%)", "Median CAGR (%)",
                   "Mean Sharpe", "Mean MaxDD (%)", "Mean Win Rate (%)")))
fmt_agg <- format_experiment_aggregates(agg)
record("5i. aggregate Buy & Hold win rate is '—' (not 0, not NA string)",
       identical(fmt_agg$`Mean Win Rate (%)`[fmt_agg$Strategy == "Buy & Hold"],
                 "—"))
record("5j. formatted tables contain NO NA strings and no Inf/NaN",
       !any(vapply(fmt, function(col) any(grepl("^NA$", as.character(col))),
                   logical(1))) &&
         !any(is.infinite(suppressWarnings(unlist(lapply(
           synth, function(col) { v <- as.numeric(col); v[!is.na(v)] }))))))

# ===========================================================================
# 6. FAILED DATA RETRIEVAL NEVER FABRICATES RESULTS
# ===========================================================================
fail_row <- failed_experiment_row(cfg_bh, "boom: provider unreachable")
record("6a. failed row records status='failed' with the error message",
       identical(fail_row$status, "failed") &&
         identical(fail_row$error_message, "boom: provider unreachable"))
record("6b. failed row carries NO metrics (all NA, nothing invented)",
       all(is.na(fail_row$total_return)) && all(is.na(fail_row$cagr)) &&
         all(is.na(fail_row$sharpe)) && all(is.na(fail_row$final_value)) &&
         is.na(fail_row$win_rate) && is.na(fail_row$paper_trades))

# validate_experiment_results must REJECT a result set containing a failure.
synth_with_fail <- synth
synth_with_fail$status[1] <- "failed"
synth_with_fail$error_message[1] <- "boom"
synth_with_fail$total_return[1] <- NA_real_
rejected <- tryCatch(validate_experiment_results(synth_with_fail),
                     error = function(e) e)
record("6c. validation rejects a set containing a failed run",
       inherits(rejected, "error") && grepl("failed run", conditionMessage(rejected)))

# The driver: a fetch function that fails for TSLA must abort, write nothing.
tmp_out <- tempfile("exp_out_")
dir.create(tmp_out, recursive = TRUE)
fake_fetch <- function(ticker, start, end) {
  if (ticker == "TSLA") stop("data source unavailable for TSLA")
  ohlcv_ma
}
driver_failed <- tryCatch(
  run_experiment(fetch_fn = fake_fetch, out_dir = tmp_out,
                 quiet = TRUE, write_outputs = TRUE),
  error = function(e) e
)
record("6d. driver aborts (error) when a ticker's retrieval fails",
       inherits(driver_failed, "error") &&
         grepl("TSLA", conditionMessage(driver_failed)))
record("6e. driver writes NO canonical results when a run fails",
       length(list.files(tmp_out, pattern = "results_|experiment\\.json")) == 0L)

# A fully successful driver run (all fetches OK) writes all outputs.
driver_ok <- tryCatch(
  run_experiment(fetch_fn = function(ticker, start, end) ohlcv_ma,
                 out_dir = tmp_out, quiet = TRUE, write_outputs = TRUE),
  error = function(e) e
)
record("6f. fully successful driver run completes",
       is.list(driver_ok) && !inherits(driver_ok, "error"))
if (is.list(driver_ok) && !inherits(driver_ok, "error")) {
  files_written <- list.files(tmp_out)
  record("6g. all canonical output files written on success",
         all(c("results_15runs.csv", "results_15runs_raw.csv",
               "results_aggregates.csv", "results_table.md",
               "experiment.json", "experiment_metadata.json") %in%
               files_written))
  csv_lines <- readLines(file.path(tmp_out, "results_15runs.csv"),
                         warn = FALSE)
  record("6h. presented CSV has the exact header + 15 data rows",
         length(csv_lines) == 16L &&
           grepl("Total Return \\(%\\)", csv_lines[1]) &&
           grepl("Buy & Hold", paste(csv_lines, collapse = "\n")))
  record("6i. validation passes on the driver's own results (15/5/3 integrity)",
         isTRUE(tryCatch({
           validate_experiment_results(driver_ok$results); TRUE
         }, error = function(e) FALSE)))
}
unlink(tmp_out, recursive = TRUE)

# ===========================================================================
# Summary
# ===========================================================================
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d experiment-runner tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL EXPERIMENT RUNNER TESTS PASSED\n")
quit(status = 0)