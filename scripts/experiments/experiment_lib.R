# AlphaForge Insight — research experiment library (PAPER.md section 9/10)
#
# Reproducible driver for the 15-run research backtesting experiment:
#   5 assets (AAPL, MSFT, GOOGL, AMZN, TSLA) x 3 strategies
#   (MA Crossover 50/200, RSI 14/30/70, Buy & Hold).
#
# The experiment REUSES the production Phase 3-6 pipeline and its established
# execution semantics — nothing here re-implements accounting, metrics or
# market-data retrieval:
#   - data:          backend/analytics/data_retrieval.R::fetch_ohlcv (Yahoo via
#                    quantmod, cached, sanitized)
#   - indicators:    backend/analytics/indicators.R (TTR SMA/RSI)
#   - signals:       backend/analytics/signals.R (MA crossover, RSI crossing,
#                    generate_buy_and_hold_signals)
#   - engine:        backend/analytics/execution.R::run_backtest (t+1-OPEN
#                    execution, cash/shares accounting, equity curve)
#   - metrics:       backend/analytics/metrics.R (8 Phase 5 metrics)
#
# Paper-specific presentation conventions (PAPER.md sections 10.1/10.2):
#   - MaxDD is stored internally as a NEGATIVE decimal (engine convention);
#     the paper table presents the MAGNITUDE as a positive percentage.
#   - Win Rate is based on COMPLETED BUY->SELL round trips only.
#   - Buy & Hold: the engine completes ZERO round trips (entry is held open
#     and marked to market), so the paper Win Rate is undefined and displayed
#     as "—"; the paper convention counts the single entry as Trades = 1.
#     This is a PRESENTATION rule for the Buy & Hold rows only — the engine's
#     completed-round-trip count for those rows is still 0 and is recorded.
#   - Percentages and Sharpe/Sortino are rounded to 2 decimals, Trades to an
#     integer, ONLY at presentation; all aggregation uses unrounded values.
#
# All functions are pure and deterministic (no network) except `run_experiment`
# which takes an injectable data-fetch function (defaults to the production
# fetch_ohlcv) so the driver is unit-testable without live data.

# ---------------------------------------------------------------------------
# Experiment constants (single source of truth; PAPER.md section 9)
# ---------------------------------------------------------------------------
EXPERIMENT_ASSETS        <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
EXPERIMENT_START         <- "2015-01-01"
EXPERIMENT_END           <- "2025-12-31"
EXPERIMENT_INITIAL_CAPITAL <- 10000
EXPERIMENT_TRANSACTION_COST <- 0
EXPERIMENT_RISK_FREE_RATE   <- 0.02

# Presentation labels (PAPER.md table) -> engine strategy code + parameters.
EXPERIMENT_STRATEGIES <- list(
  "MA Cross (50/200)" = list(
    code = "MA_Crossover",
    parameters = list(fast_period = 50L, slow_period = 200L)
  ),
  "RSI (14, 30/70)" = list(
    code = "RSI",
    parameters = list(period = 14L, oversold = 30, overbought = 70)
  ),
  "Buy & Hold" = list(
    code = "Buy_and_Hold",
    parameters = list()
  )
)

EXPERIMENT_DATA_SOURCE <- "Yahoo Finance via quantmod (src='yahoo')"
EXPERIMENT_CONVENTION  <- paste0(
  "signal generated at the close of t is executed at the OPEN of t+1 ",
  "(no look-ahead bias); signals on the final candle are never executed; ",
  "long-only single asset; completed BUY->SELL round trips count as trades; ",
  "open final position is marked to market but not counted as completed"
)

# ---------------------------------------------------------------------------
# Experiment configuration
# ---------------------------------------------------------------------------

#' Build the full 15-run experiment configuration (5 assets x 3 strategies).
#'
#' @return data.frame with one row per run:
#'   asset, strategy (paper label), strategy_code, parameters (JSON string),
#'   start_date, end_date, initial_capital, transaction_cost, risk_free_rate.
build_experiment_config <- function() {
  labels <- names(EXPERIMENT_STRATEGIES)
  rows <- list()
  k <- 0L
  for (asset in EXPERIMENT_ASSETS) {
    for (label in labels) {
      k <- k + 1L
      spec <- EXPERIMENT_STRATEGIES[[label]]
      rows[[k]] <- data.frame(
        asset = asset,
        strategy = label,
        strategy_code = spec$code,
        parameters = jsonlite::toJSON(spec$parameters, auto_unbox = TRUE),
        start_date = EXPERIMENT_START,
        end_date = EXPERIMENT_END,
        initial_capital = EXPERIMENT_INITIAL_CAPITAL,
        transaction_cost = EXPERIMENT_TRANSACTION_COST,
        risk_free_rate = EXPERIMENT_RISK_FREE_RATE,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Parse a strategy-parameters JSON string back into an R list.
#' @param json_str JSON string produced by build_experiment_config().
#' @return list.
parse_parameters <- function(json_str) {
  p <- jsonlite::fromJSON(json_str, simplifyVector = TRUE)
  if (is.null(p)) list() else p
}

#' Generate the signal series for one (asset, strategy) run.
#'
#' Uses ONLY the production signal generators:
#'   - MA_Crossover -> SMA(50)/SMA(200) crossover
#'   - RSI          -> RSI(14) crossing 30/70
#'   - Buy_and_Hold -> generate_buy_and_hold_signals
#'
#' @param ohlcv Canonical xts OHLCV from fetch_ohlcv.
#' @param strategy_code One of "MA_Crossover", "RSI", "Buy_and_Hold".
#' @param parameters Named list of strategy parameters.
#' @return xts of character signals aligned with `ohlcv`.
build_signals_for_strategy <- function(ohlcv, strategy_code, parameters = list()) {
  close <- ohlcv[, "Close"]
  if (strategy_code == "MA_Crossover") {
    fast <- calculate_sma(close, parameters$fast_period)
    slow <- calculate_sma(close, parameters$slow_period)
    generate_ma_crossover_signals(fast, slow)
  } else if (strategy_code == "RSI") {
    rsi <- calculate_rsi(close, parameters$period)
    generate_rsi_signals(rsi, parameters$oversold, parameters$overbought)
  } else if (strategy_code == "Buy_and_Hold") {
    generate_buy_and_hold_signals(close)
  } else {
    stop(sprintf("Unknown strategy_code for the experiment: '%s'.", strategy_code))
  }
}

# ---------------------------------------------------------------------------
# Single-run execution (production engine)
# ---------------------------------------------------------------------------

#' Run ONE experiment row through the production engine.
#'
#' @param ohlcv Canonical xts OHLCV (from fetch_ohlcv).
#' @param cfg One row of build_experiment_config() (a 1-row data.frame).
#' @param execution_timestamp UTC timestamp string for the run (metadata).
#' @return 1-row data.frame with status "ok" and BOTH the raw engine metrics
#'   and the paper-convention fields:
#'   asset, strategy, strategy_code, parameters, start_date, end_date,
#'   initial_capital, transaction_cost, risk_free_rate, n_observations,
#'   final_value, total_return, cagr, sharpe, sortino, max_drawdown, win_rate,
#'   volatility, engine_total_trades, paper_trades, data_source, status,
#'   error_message, execution_timestamp.
run_single_experiment_row <- function(ohlcv, cfg, execution_timestamp = "") {
  if (!inherits(ohlcv, "xts")) {
    stop("ohlcv must be an xts object.")
  }
  if (NROW(ohlcv) < 2L) {
    stop(sprintf("Insufficient data for %s (%d rows).", cfg$asset, NROW(ohlcv)))
  }

  params <- parse_parameters(cfg$parameters)
  signals <- build_signals_for_strategy(ohlcv, cfg$strategy_code, params)

  result <- run_backtest(
    ohlcv = ohlcv,
    signals = signals,
    initial_capital = cfg$initial_capital,
    start_date = cfg$start_date,
    end_date = cfg$end_date,
    ticker = cfg$asset,
    strategy = cfg$strategy_code,
    parameters = params,
    transaction_cost = cfg$transaction_cost,
    risk_free_rate = cfg$risk_free_rate
  )

  s <- result$summary
  # Paper convention: Buy & Hold reports the single entry as 1 trade; the
  # engine's completed-round-trip count stays 0 and is recorded separately.
  if (cfg$strategy_code == "Buy_and_Hold") {
    paper_trades <- 1L
  } else {
    paper_trades <- as.integer(s$total_trades)
  }

  data.frame(
    asset = cfg$asset,
    strategy = cfg$strategy,
    strategy_code = cfg$strategy_code,
    parameters = cfg$parameters,
    start_date = format(as.Date(cfg$start_date), "%Y-%m-%d"),
    end_date = format(as.Date(cfg$end_date), "%Y-%m-%d"),
    initial_capital = as.numeric(cfg$initial_capital),
    transaction_cost = as.numeric(cfg$transaction_cost),
    risk_free_rate = as.numeric(cfg$risk_free_rate),
    n_observations = NROW(ohlcv),
    final_value = as.numeric(s$final_value),
    total_return = as.numeric(s$total_return),
    cagr = as.numeric(s$cagr),
    sharpe = as.numeric(s$sharpe),
    sortino = as.numeric(s$sortino),
    max_drawdown = as.numeric(s$maximum_drawdown),
    win_rate = as.numeric(s$win_rate),
    volatility = as.numeric(s$volatility),
    engine_total_trades = as.integer(s$total_trades),
    paper_trades = as.integer(paper_trades),
    data_source = EXPERIMENT_DATA_SOURCE,
    status = "ok",
    error_message = NA_character_,
    execution_timestamp = execution_timestamp,
    stringsAsFactors = FALSE
  )
}

#' Record a failed experiment row (data retrieval or engine error).
#'
#' A failed row carries NO metric values (all NA) so a failure can never be
#' mistaken for a real result. `run_experiment()` refuses to emit the
#' canonical results files when any row is failed.
#'
#' @param cfg One row of build_experiment_config().
#' @param message The error message.
#' @return 1-row data.frame with status "failed" and NA metrics.
failed_experiment_row <- function(cfg, message) {
  data.frame(
    asset = cfg$asset,
    strategy = cfg$strategy,
    strategy_code = cfg$strategy_code,
    parameters = cfg$parameters,
    start_date = format(as.Date(cfg$start_date), "%Y-%m-%d"),
    end_date = format(as.Date(cfg$end_date), "%Y-%m-%d"),
    initial_capital = as.numeric(cfg$initial_capital),
    transaction_cost = as.numeric(cfg$transaction_cost),
    risk_free_rate = as.numeric(cfg$risk_free_rate),
    n_observations = NA_integer_,
    final_value = NA_real_,
    total_return = NA_real_,
    cagr = NA_real_,
    sharpe = NA_real_,
    sortino = NA_real_,
    max_drawdown = NA_real_,
    win_rate = NA_real_,
    volatility = NA_real_,
    engine_total_trades = NA_integer_,
    paper_trades = NA_integer_,
    data_source = EXPERIMENT_DATA_SOURCE,
    status = "failed",
    error_message = as.character(message),
    execution_timestamp = NA_character_,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Validation of the experiment result set
# ---------------------------------------------------------------------------

#' Sanity checks for the 15-run result set (PAPER.md section 10).
#'
#' @param results data.frame with one row per run (raw, from
#'   run_single_experiment_row / failed_experiment_row).
#' @return Invisible list(ok = TRUE, messages = character()) or throws with
#'   a descriptive error. Never silently passes an anomalous result set.
validate_experiment_results <- function(results) {
  problems <- character(0)
  if (nrow(results) != 15L) {
    problems <- c(problems, sprintf("expected 15 rows, got %d", nrow(results)))
  }
  if (!all(results$status == "ok")) {
    problems <- c(problems, sprintf(
      "%d failed run(s): %s",
      sum(results$status != "ok"),
      paste(results$asset[results$status != "ok"],
            results$strategy[results$status != "ok"], sep = "/", collapse = ", ")
    ))
  }
  for (str in names(EXPERIMENT_STRATEGIES)) {
    n <- sum(results$strategy == str)
    if (n != 5L) {
      problems <- c(problems, sprintf("strategy '%s' has %d rows (expected 5)", str, n))
    }
  }
  for (asset in EXPERIMENT_ASSETS) {
    n <- sum(results$asset == asset)
    if (n != 3L) {
      problems <- c(problems, sprintf("asset '%s' has %d rows (expected 3)", asset, n))
    }
  }
  if (length(problems) > 0) {
    stop(paste(c("experiment result validation failed:", problems), collapse = "\n  - "))
  }

  # Metric sanity: no Inf/NaN anywhere; no NA in metrics that must be defined
  # (MA/RSI win rates must be defined; all values finite).
  metric_cols <- c("total_return", "cagr", "sharpe", "sortino",
                   "max_drawdown", "volatility")
  for (col in metric_cols) {
    v <- results[[col]]
    if (any(is.infinite(v)) || any(is.nan(v))) {
      problems <- c(problems, sprintf("column '%s' contains Inf/NaN", col))
    }
    if (anyNA(v)) {
      problems <- c(problems, sprintf("column '%s' contains NA", col))
    }
  }
  # Win rate: defined (numeric) for MA/RSI; NA allowed ONLY for Buy & Hold.
  bh <- results$strategy_code == "Buy_and_Hold"
  wr_non_bh <- results$win_rate[!bh]
  wr_bh <- results$win_rate[bh]
  if (anyNA(wr_non_bh) || any(!is.finite(wr_non_bh))) {
    problems <- c(problems, "win_rate must be defined for MA/RSI runs")
  }
  if (any(!is.na(wr_bh))) {
    problems <- c(problems, "win_rate must be NA (undefined) for Buy & Hold runs")
  }
  # Buy & Hold: paper trades == 1; engine completed trades == 0.
  if (any(results$paper_trades[bh] != 1L)) {
    problems <- c(problems, "Buy & Hold rows must report paper Trades = 1")
  }
  if (any(results$engine_total_trades[bh] != 0L)) {
    problems <- c(problems, "Buy & Hold rows must have 0 completed round trips")
  }
  # Non-Buy & Hold: engine completed trades == paper trades.
  if (any(results$engine_total_trades[!bh] != results$paper_trades[!bh])) {
    problems <- c(problems, "paper trades must equal completed round trips for MA/RSI runs")
  }
  # Final values finite and positive.
  if (any(results$final_value <= 0) || any(!is.finite(results$final_value))) {
    problems <- c(problems, "final_value must be finite and positive for every run")
  }
  # MaxDD sign convention: internal maximum_drawdown <= 0.
  if (any(results$max_drawdown > 0, na.rm = TRUE)) {
    problems <- c(problems, "max_drawdown must be <= 0 (engine convention)")
  }

  if (length(problems) > 0) {
    stop(paste(c("experiment metric validation failed:", problems), collapse = "\n  - "))
  }
  invisible(list(ok = TRUE, messages = character(0)))
}

# ---------------------------------------------------------------------------
# Aggregation (PAPER.md section 10.2) — uses UNROUNDED asset-level values
# ---------------------------------------------------------------------------

#' Aggregate the five asset-level observations per strategy.
#'
#' @param results Raw results data.frame (validated, 15 rows).
#' @return 3-row data.frame with UNROUNDED values:
#'   strategy, mean_total_return, median_cagr, mean_sharpe, mean_maxdd_pct,
#'   mean_win_rate (NA for Buy & Hold => "—" at presentation).
aggregate_experiment <- function(results) {
  validate_experiment_results(results)
  strategies <- names(EXPERIMENT_STRATEGIES)
  out <- lapply(strategies, function(str) {
    sub <- results[results$strategy == str, , drop = FALSE]
    # MaxDD magnitude (positive percentage) is the presentation convention.
    maxdd_pct <- abs(sub$max_drawdown) * 100
    data.frame(
      strategy = str,
      mean_total_return = mean(sub$total_return) * 100,
      median_cagr = stats::median(sub$cagr) * 100,
      mean_sharpe = mean(sub$sharpe),
      mean_maxdd_pct = mean(maxdd_pct),
      mean_win_rate = if (str == "Buy & Hold") NA_real_ else mean(sub$win_rate) * 100,
      n_assets = nrow(sub),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# Presentation formatting (PAPER.md sections 10.1/10.2)
# ---------------------------------------------------------------------------

pct2 <- function(x) sprintf("%.2f", x)   # percentages, 2 decimals

#' Format the raw 15-run results for the paper table.
#'
#' @param results Raw results data.frame (validated).
#' @return data.frame with the exact paper columns:
#'   Asset, Strategy, Total Return (%), CAGR (%), Sharpe, Sortino, MaxDD (%),
#'   Win Rate (%), Volatility (%), Trades.
format_experiment_results <- function(results) {
  validate_experiment_results(results)
  bh <- results$strategy_code == "Buy_and_Hold"
  win_rate_display <- ifelse(bh, "—", pct2(results$win_rate * 100))
  data.frame(
    Asset = results$asset,
    Strategy = results$strategy,
    "Total Return (%)" = pct2(results$total_return * 100),
    "CAGR (%)" = pct2(results$cagr * 100),
    Sharpe = sprintf("%.2f", results$sharpe),
    Sortino = sprintf("%.2f", results$sortino),
    "MaxDD (%)" = pct2(abs(results$max_drawdown) * 100),
    "Win Rate (%)" = win_rate_display,
    "Volatility (%)" = pct2(results$volatility * 100),
    Trades = as.integer(results$paper_trades),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Format the aggregate statistics for the paper table.
#'
#' @param agg Unrounded aggregation from aggregate_experiment().
#' @return data.frame with the exact paper columns:
#'   Strategy, Mean Total Return (%), Median CAGR (%), Mean Sharpe,
#'   Mean MaxDD (%), Mean Win Rate (%).
format_experiment_aggregates <- function(agg) {
  win_rate_display <- ifelse(is.na(agg$mean_win_rate), "—", pct2(agg$mean_win_rate))
  data.frame(
    Strategy = agg$strategy,
    "Mean Total Return (%)" = pct2(agg$mean_total_return),
    "Median CAGR (%)" = pct2(agg$median_cagr),
    "Mean Sharpe" = sprintf("%.2f", agg$mean_sharpe),
    "Mean MaxDD (%)" = pct2(agg$mean_maxdd_pct),
    "Mean Win Rate (%)" = win_rate_display,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Output writers (CSV / JSON / Markdown)
# ---------------------------------------------------------------------------

#' Write all experiment outputs into `out_dir`.
#'
#' Canonical output files (written ONLY when all 15 runs succeeded):
#'   results_15runs.csv        presented paper table (2 decimals, "—", integers)
#'   results_15runs_raw.csv    full-precision raw metrics + internals
#'   results_aggregates.csv    presented 3-row aggregate table
#'   results_table.md          human-readable Markdown (both tables + metadata)
#'   experiment.json           full machine-readable record (raw + presented)
#'   experiment_metadata.json  experiment configuration metadata
#'
#' @param results Raw 15-row results data.frame.
#' @param agg Unrounded aggregation (aggregate_experiment output).
#' @param metadata Named list of experiment metadata.
#' @param out_dir Output directory (created if missing).
#' @return Invisible character vector of written file paths.
write_experiment_outputs <- function(results, agg, metadata, out_dir) {
  validate_experiment_results(results)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  fmt <- format_experiment_results(results)
  fmt_agg <- format_experiment_aggregates(agg)

  # Preserve full precision in the machine-readable raw CSV/JSON.
  old_digits <- options(digits = 15)
  on.exit(options(old_digits), add = TRUE)

  csv_path <- file.path(out_dir, "results_15runs.csv")
  utils::write.csv(fmt, csv_path, row.names = FALSE)

  raw_csv_path <- file.path(out_dir, "results_15runs_raw.csv")
  utils::write.csv(results, raw_csv_path, row.names = FALSE, na = "")

  agg_csv_path <- file.path(out_dir, "results_aggregates.csv")
  utils::write.csv(fmt_agg, agg_csv_path, row.names = FALSE)

  md_path <- file.path(out_dir, "results_table.md")
  write_markdown_report(fmt, fmt_agg, metadata, md_path)

  # JSON: full precision numbers (jsonlite digits = NA keeps doubles intact).
  json_payload <- list(
    metadata = metadata,
    config = build_experiment_config(),
    results_raw = results,
    results_presented = fmt,
    aggregates_raw = agg,
    aggregates_presented = fmt_agg
  )
  json_path <- file.path(out_dir, "experiment.json")
  jsonlite::write_json(json_payload, json_path, auto_unbox = TRUE,
                       digits = NA, pretty = TRUE)

  meta_path <- file.path(out_dir, "experiment_metadata.json")
  jsonlite::write_json(metadata, meta_path, auto_unbox = TRUE,
                       digits = NA, pretty = TRUE)

  invisible(c(csv_path, raw_csv_path, agg_csv_path, md_path, json_path, meta_path))
}

#' Render the human-readable Markdown report (both paper tables + metadata).
#' @param fmt Presented 15-row table (format_experiment_results output).
#' @param fmt_agg Presented aggregate table (format_experiment_aggregates output).
#' @param metadata Named list of experiment metadata.
#' @param md_path Output .md path.
write_markdown_report <- function(fmt, fmt_agg, metadata, md_path) {
  md_table <- function(df, align_cols) {
    header <- paste(names(df), collapse = " | ")
    separator <- paste(align_cols, collapse = " | ")
    lines <- c(paste0("| ", header, " |"),
               paste0("| ", separator, " |"))
    for (i in seq_len(nrow(df))) {
      vals <- vapply(seq_len(ncol(df)), function(j) as.character(df[i, j]),
                     character(1))
      lines <- c(lines, paste0("| ", paste(vals, collapse = " | "), " |"))
    }
    lines
  }
  # Paper-style alignment: right-align all metric columns.
  n_metrics_10 <- c(":---", rep("---:", 9))
  n_metrics_6 <- c(":---", rep("---:", 5))

  con <- file(md_path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(c(
    "# AlphaForge Insight — Research Experiment Results",
    "",
    sprintf("- **Data source:** %s", metadata$data_source),
    sprintf("- **Date range:** %s to %s (n observations per ticker recorded)",
            metadata$start_date, metadata$end_date),
    sprintf("- **Initial capital:** $%.2f", metadata$initial_capital),
    sprintf("- **Transaction cost:** %.0f%%", metadata$transaction_cost * 100),
    sprintf("- **Risk-free rate:** %.2f%%", metadata$risk_free_rate * 100),
    sprintf("- **Execution timestamp (UTC):** %s", metadata$execution_timestamp),
    sprintf("- **Implementation commit:** %s", metadata$commit_hash),
    sprintf("- **Engine versions:** R %s; quantmod %s; TTR %s; xts %s",
            metadata$r_version, metadata$quantmod_version,
            metadata$ttr_version, metadata$xts_version),
    sprintf("- **Execution convention:** %s", metadata$execution_convention),
    "",
    "## 10.1 Per-Asset Metrics by Strategy",
    "",
    md_table(fmt, n_metrics_10),
    "",
    "## 10.2 Aggregate Statistics Across Assets (per Strategy)",
    "",
    md_table(fmt_agg, n_metrics_6),
    ""
  ), con)
  invisible(md_path)
}

# ---------------------------------------------------------------------------
# Metadata
# ---------------------------------------------------------------------------

#' Build experiment metadata (configuration + environment fingerprint).
#' @param execution_timestamp UTC timestamp of execution.
#' @param n_observations Named integer vector (ticker -> rows).
#' @return Named list.
build_experiment_metadata <- function(execution_timestamp, n_observations = NULL) {
  commit <- tryCatch(
    trimws(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[1]),
    error = function(e) "unknown"
  )
  if (length(commit) == 0L || is.na(commit) || !nzchar(commit)) commit <- "unknown"

  list(
    experiment = "15-run research backtest (PAPER.md sections 9-10)",
    assets = EXPERIMENT_ASSETS,
    strategies = names(EXPERIMENT_STRATEGIES),
    start_date = EXPERIMENT_START,
    end_date = EXPERIMENT_END,
    initial_capital = EXPERIMENT_INITIAL_CAPITAL,
    transaction_cost = EXPERIMENT_TRANSACTION_COST,
    risk_free_rate = EXPERIMENT_RISK_FREE_RATE,
    data_source = EXPERIMENT_DATA_SOURCE,
    execution_timestamp = execution_timestamp,
    commit_hash = commit,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    quantmod_version = as.character(utils::packageVersion("quantmod")),
    ttr_version = as.character(utils::packageVersion("TTR")),
    xts_version = as.character(utils::packageVersion("xts")),
    n_observations = n_observations,
    execution_convention = EXPERIMENT_CONVENTION
  )
}

# ---------------------------------------------------------------------------
# Main driver
# ---------------------------------------------------------------------------

#' Execute the full 15-run experiment.
#'
#' @param fetch_fn Data-fetch function signature (ticker, start, end) -> xts.
#'   Defaults to the production backend/analytics/data_retrieval.R::fetch_ohlcv.
#' @param out_dir Output directory for the canonical result files.
#' @param quiet Suppress per-run progress output.
#' @param write_outputs Write the canonical output files (TRUE for the real
#'   experiment; tests may disable to avoid file side effects).
#' @return Invisible list(metadata, results, aggregates, presented,
#'   aggregates_presented, files). Stops with a descriptive error when any of
#'   the 15 runs fails (a failed run NEVER yields a fabricated result row).
run_experiment <- function(fetch_fn = fetch_ohlcv,
                           out_dir = file.path("scripts", "experiments", "output"),
                           quiet = FALSE,
                           write_outputs = TRUE) {
  if (!is.function(fetch_fn)) stop("fetch_fn must be a function.")
  config <- build_experiment_config()
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

  n_obs <- stats::setNames(rep(NA_integer_, length(EXPERIMENT_ASSETS)),
                           EXPERIMENT_ASSETS)
  rows <- vector("list", nrow(config))
  for (i in seq_len(nrow(config))) {
    cfg <- config[i, ]
    label <- sprintf("%s | %s", cfg$asset, cfg$strategy)
    if (!quiet) cat(sprintf("[%02d/15] %s ... ", i, label))
    rows[[i]] <- tryCatch(
      {
        ohlcv <- fetch_fn(cfg$asset, cfg$start_date, cfg$end_date)
        n_obs[cfg$asset] <- as.integer(NROW(ohlcv))
        run_single_experiment_row(ohlcv, cfg, execution_timestamp = ts)
      },
      error = function(e) {
        failed_experiment_row(cfg, conditionMessage(e))
      }
    )
    ok <- identical(rows[[i]]$status, "ok")
    if (!quiet) {
      if (ok) {
        cat(sprintf("OK (final $%.2f)\n", rows[[i]]$final_value))
      } else {
        cat(sprintf("FAILED: %s\n", rows[[i]]$error_message))
      }
    }
  }

  results <- do.call(rbind, rows)
  rownames(results) <- NULL

  # Validate: throws when the set is anomalous or any row failed.
  validate_experiment_results(results)

  agg <- aggregate_experiment(results)
  metadata <- build_experiment_metadata(ts, n_observations = n_obs)

  files <- NULL
  if (write_outputs) {
    files <- write_experiment_outputs(results, agg, metadata, out_dir)
  }

  invisible(list(
    metadata = metadata,
    results = results,
    aggregates = agg,
    presented = format_experiment_results(results),
    aggregates_presented = format_experiment_aggregates(agg),
    files = files
  ))
}