# AlphaForge Insight — News-Aware Forecasting Experiment (Phase H.3B)
#
# Runs comparative experiments:
#   1. Baseline: market-only features (existing pipeline)
#   2. News-enhanced: market features + real historical news features
#
# CRITICAL TEMPORAL CONSTRAINTS:
#   - Uses identical temporal train/test splits for both conditions
#   - News features respect 4 PM ET decision cutoff (no future information)
#   - After-hours/weekend news maps to next trading day
#   - Threshold computed from training data only (no test leakage)
#
# LIMITATIONS:
#   - Limited to 3 tickers and ~6 months of news data
#   - News sample is small and manually curated
#   - Results should NOT be generalized beyond this sample
#   - No claims of forecasting improvement unless out-of-sample data supports it

library(xts)

# Set ML directory globally
.ML_DIR <- file.path(getwd(), "backend", "analytics", "ml")

# =============================================================================
# CONFIGURATION
# =============================================================================

PHASE_H3B_CONFIG <- list(
  # Experiment settings (limited scope for MVP)
  tickers = c("AAPL", "MSFT", "GOOGL"),
  start_date = "2024-01-01",
  end_date = "2024-06-30",
  horizon = 5,
  n_folds = 3,
  test_frac = 0.15,
  seed = 123,
  
  # Threshold settings (LEAKAGE-FREE)
  threshold_method = "percentile",
  train_percentile = 0.20,
  
  # Trading settings
  initial_capital = 10000,
  transaction_cost = 0.001,
  
  # Output
  output_dir = "research/results"
)

# =============================================================================
# NEWS-AWARE FEATURE INTEGRATION
# =============================================================================

#' Build combined market + news features with leakage prevention.
#'
#' @param ohlcv xts OHLCV data.
#' @param ticker Ticker symbol.
#' @param news_df data.frame of news articles (can be real or synthetic).
#' @param market_groups Feature groups for market features (NULL = all).
#' @return list(features = xts, feature_names, metadata).
build_news_aware_features <- function(ohlcv, ticker, news_df = NULL, 
                                      market_groups = NULL) {
  
  ml_dir <- .ML_DIR
  source(file.path(ml_dir, "features.R"), local = TRUE)
  source(file.path(ml_dir, "news_synthetic.R"), local = TRUE)
  source(file.path(ml_dir, "news_pipeline.R"), local = TRUE)
  
  # Build market features (baseline)
  market_result <- build_features(ohlcv, groups = market_groups)
  
  # Build news features if news data provided
  if (!is.null(news_df) && nrow(news_df) > 0) {
    news_result <- build_news_features(news_df, ohlcv, ticker)
    
    # Combine
    combined_features <- cbind(market_result$features, news_result$features)
    
    metadata <- list(
      ticker = ticker,
      n_market_features = ncol(market_result$features),
      n_news_features = ncol(news_result$features),
      n_total_features = ncol(combined_features),
      has_news = TRUE,
      news_is_synthetic = news_result$metadata$is_synthetic,
      news_coverage = news_result$metadata$n_sessions_with_news / nrow(ohlcv)
    )
  } else {
    # Market-only baseline
    combined_features <- market_result$features
    
    metadata <- list(
      ticker = ticker,
      n_market_features = ncol(market_result$features),
      n_news_features = 0,
      n_total_features = ncol(combined_features),
      has_news = FALSE,
      news_is_synthetic = NA,
      news_coverage = 0
    )
  }
  
  list(
    features = combined_features,
    feature_names = colnames(combined_features),
    metadata = metadata,
    market_features = market_result
  )
}

# =============================================================================
# LEAKAGE-FREE EVALUATION WITH NEWS FEATURES
# =============================================================================

#' Evaluate model with optional news features.
#'
#' @param ds Dataset list.
#' @param model_key Model to evaluate.
#' @param config Configuration list.
#' @return list with fold results and predictions.
evaluate_model_with_news <- function(ds, model_key, config = PHASE_H3B_CONFIG) {
  
  ml_dir <- .ML_DIR
  source(file.path(ml_dir, "preprocessing.R"), local = TRUE)
  source(file.path(ml_dir, "regression_models.R"), local = TRUE)
  source(file.path(ml_dir, "regression_evaluation.R"), local = TRUE)
  source(file.path(ml_dir, "run_validated_regression_experiments.R"), local = TRUE)
  
  # Generate walk-forward splits
  splits <- walk_forward_splits(ds$n, n_folds = config$n_folds, 
                                test_frac = config$test_frac,
                                embargo = ds$horizon)
  
  folds <- list()
  all_preds <- c()
  all_actuals <- c()
  all_dates <- c()
  all_fold_ids <- c()
  all_thresholds <- list()
  
  for (i in seq_along(splits)) {
    fold_split <- splits[[i]]
    train_idx <- fold_split$train
    test_idx <- fold_split$test
    
    cat(sprintf("    Fold %d: train=%d, test=%d\n", i, length(train_idx), length(test_idx)))
    
    # Fit preprocessing on training data only
    spec <- fit_preprocessing(ds$X_raw[train_idx, , drop = FALSE])
    X_imp <- apply_preprocessing(ds$X_raw, spec)
    
    # Scaling for models that need it
    needs_scaling <- isTRUE(REGRESSION_MODELS[[model_key]]$needs_scaling)
    scaler <- if (needs_scaling) fit_scaler(X_imp[train_idx, , drop = FALSE]) else NULL
    
    mk_scaled <- function(idx) {
      M <- X_imp[idx, , drop = FALSE]
      if (!is.null(scaler)) apply_scaler(M, scaler) else M
    }
    
    X_train <- mk_scaled(train_idx)
    X_test <- mk_scaled(test_idx)
    y_train <- ds$y[train_idx]
    y_test <- ds$y[test_idx]
    dates_train <- ds$dates[train_idx]
    dates_test <- ds$dates[test_idx]
    
    # Train model
    model_result <- tryCatch({
      if (model_key == "zero_return") {
        handle <- train_zero_return_baseline()
      } else if (model_key == "historical_mean") {
        handle <- train_historical_mean_baseline(y_train, use_expanding = FALSE)
      } else if (model_key == "random_forest") {
        handle <- train_random_forest_regression(X_train, y_train, 
                                                 REGRESSION_MODELS[[model_key]]$defaults)
      } else if (model_key == "xgboost") {
        handle <- train_xgboost_regression(X_train, y_train,
                                           REGRESSION_MODELS[[model_key]]$defaults)
      } else {
        stop(sprintf("Unknown model key: %s", model_key))
      }
      list(success = TRUE, handle = handle)
    }, error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    })
    
    if (!model_result$success) {
      folds[[i]] <- list(
        fold = i,
        success = FALSE,
        error = model_result$error,
        n_train = length(train_idx),
        n_test = length(test_idx)
      )
      next
    }
    
    handle <- model_result$handle
    
    # Get predictions
    train_preds <- tryCatch(handle$predict(X_train), error = function(e) rep(NA_real_, length(train_idx)))
    test_preds <- tryCatch(handle$predict(X_test), error = function(e) rep(NA_real_, length(test_idx)))
    
    # LEAKAGE-FREE threshold from training data
    threshold <- compute_train_threshold(
      train_predictions = train_preds,
      train_actuals = y_train,
      method = config$threshold_method,
      percentile = config$train_percentile
    )
    
    all_thresholds[[i]] <- threshold
    
    # Compute forecast metrics
    fm <- forecast_metrics(test_preds, y_test)
    
    # Store fold results
    folds[[i]] <- list(
      fold = i,
      success = TRUE,
      n_train = length(train_idx),
      n_test = length(test_idx),
      dates_train = as.character(range(dates_train)),
      dates_test = as.character(range(dates_test)),
      predictions = as.numeric(test_preds),
      actuals = as.numeric(y_test),
      threshold = threshold,
      metrics = fm
    )
    
    # Aggregate
    all_preds <- c(all_preds, test_preds)
    all_actuals <- c(all_actuals, y_test)
    all_dates <- c(all_dates, as.character(dates_test))
    all_fold_ids <- c(all_fold_ids, rep(i, length(test_idx)))
  }
  
  # Aggregate metrics
  aggregate_metrics <- forecast_metrics(all_preds, all_actuals)
  
  list(
    model_key = model_key,
    model_label = REGRESSION_MODELS[[model_key]]$label,
    folds = folds,
    aggregate_metrics = aggregate_metrics,
    all_predictions = data.frame(
      date = all_dates,
      fold = all_fold_ids,
      predicted = all_preds,
      actual = all_actuals,
      stringsAsFactors = FALSE
    ),
    threshold_provenance = all_thresholds,
    config = config
  )
}

# =============================================================================
# COMPARATIVE EXPERIMENT
# =============================================================================

#' Run comparative experiment for one ticker.
#'
#' Runs both baseline (market-only) and news-enhanced conditions.
#'
#' @param ticker Ticker symbol.
#' @param config Configuration list.
#' @return list with both conditions' results.
run_comparative_experiment <- function(ticker, config = PHASE_H3B_CONFIG) {
  
  cat(sprintf("\n=== COMPARATIVE EXPERIMENT: %s ===\n", ticker))
  cat(sprintf("Period: %s to %s | Horizon: %d days\n", 
              config$start_date, config$end_date, config$horizon))
  
  set.seed(config$seed)
  
  ml_dir <- .ML_DIR
  source(file.path(ml_dir, "preprocessing.R"), local = TRUE)
  source(file.path(ml_dir, "features.R"), local = TRUE)
  source(file.path(ml_dir, "regression_target.R"), local = TRUE)
  source(file.path(ml_dir, "run_validated_regression_experiments.R"), local = TRUE)
  source(file.path(ml_dir, "news_real_historical.R"), local = TRUE)
  
  # Fetch market data
  analytics_dir <- dirname(ml_dir)
  source(file.path(analytics_dir, "data_retrieval.R"), local = TRUE)
  
  ohlcv <- tryCatch({
    fetch_ohlcv(ticker, config$start_date, config$end_date)
  }, error = function(e) {
    stop(sprintf("Failed to fetch data for %s: %s", ticker, conditionMessage(e)))
  })
  
  cat(sprintf("Fetched %d rows from %s to %s\n",
              nrow(ohlcv), min(zoo::index(ohlcv)), max(zoo::index(ohlcv))))
  
  # Get real news data
  real_news <- get_real_historical_news()
  ticker_news <- real_news[real_news$ticker == ticker, ]
  cat(sprintf("Real news events for %s: %d\n", ticker, nrow(ticker_news)))
  
  # Build baseline features (market-only)
  cat("\n[BUILDING BASELINE FEATURES]\n")
  baseline_feats <- build_news_aware_features(ohlcv, ticker, news_df = NULL)
  cat(sprintf("  Market features: %d\n", baseline_feats$metadata$n_market_features))
  
  # Build news-enhanced features
  cat("\n[BUILDING NEWS-ENHANCED FEATURES]\n")
  news_feats <- build_news_aware_features(ohlcv, ticker, news_df = ticker_news)
  cat(sprintf("  Market features: %d\n", news_feats$metadata$n_market_features))
  cat(sprintf("  News features: %d\n", news_feats$metadata$n_news_features))
  cat(sprintf("  News coverage: %.1f%% of sessions\n", 
              news_feats$metadata$news_coverage * 100))
  
  # Generate targets
  targets <- generate_forward_return_target(as.numeric(ohlcv[, "Close"]), config$horizon)
  y <- targets$targets
  valid <- targets$valid & !is.na(y)
  
  # Run experiments for both conditions
  results <- list()
  
  for (condition in c("baseline", "news_enhanced")) {
    cat(sprintf("\n[%s CONDITION]\n", toupper(condition)))
    
    # Select feature set
    if (condition == "baseline") {
      X_raw <- baseline_feats$features
      feature_info <- baseline_feats$metadata
    } else {
      X_raw <- news_feats$features
      feature_info <- news_feats$metadata
    }
    
    # Filter valid rows
    non_na_frac <- 1 - rowSums(is.na(as.matrix(X_raw))) / ncol(X_raw)
    keep <- valid & (non_na_frac >= 0.7)
    
    X_raw <- X_raw[keep, ]
    y_cond <- y[keep]
    dates <- as.Date(zoo::index(X_raw))
    
    ds <- list(
      ohlcv = ohlcv,
      ticker = ticker,
      features_xts = X_raw,
      dates = dates,
      X_raw = as.matrix(X_raw),
      y = y_cond,
      n = nrow(X_raw),
      p = ncol(X_raw),
      feature_names = colnames(X_raw),
      horizon = config$horizon,
      feature_info = feature_info
    )
    
    cat(sprintf("  Usable rows: %d | Features: %d\n", ds$n, ds$p))
    
    # Evaluate models
    models_to_run <- c("zero_return", "historical_mean", "random_forest", "xgboost")
    
    condition_results <- list()
    
    for (model_key in models_to_run) {
      cat(sprintf("\n  [%s] Evaluating...\n", model_key))
      
      result <- tryCatch({
        evaluate_model_with_news(ds, model_key, config)
      }, error = function(e) {
        cat(sprintf("    ERROR: %s\n", conditionMessage(e)))
        list(model_key = model_key, error = conditionMessage(e), success = FALSE)
      })
      
      condition_results[[model_key]] <- result
    }
    
    results[[condition]] <- condition_results
  }
  
  # Build comparison table
  comparison_rows <- list()
  
  for (condition in c("baseline", "news_enhanced")) {
    for (model_key in names(results[[condition]])) {
      r <- results[[condition]][[model_key]]
      
      if (!is.null(r$error)) {
        comparison_rows[[length(comparison_rows) + 1]] <- data.frame(
          Condition = condition,
          Model = model_key,
          MAE = NA,
          RMSE = NA,
          IC = NA,
          DirAcc = NA,
          N_Obs = NA,
          stringsAsFactors = FALSE
        )
      } else {
        m <- r$aggregate_metrics
        comparison_rows[[length(comparison_rows) + 1]] <- data.frame(
          Condition = condition,
          Model = model_key,
          MAE = m$mae,
          RMSE = m$rmse,
          IC = m$ic,
          DirAcc = m$directional_accuracy,
          N_Obs = m$n_obs,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  comparison <- do.call(rbind, comparison_rows)
  comparison <- comparison[order(comparison$Condition, comparison$RMSE), ]
  
  cat("\n=== COMPARATIVE RESULTS ===\n")
  print(comparison)
  
  # Compute delta (news_enhanced - baseline)
  cat("\n=== DELTA (NEWS - BASELINE) ===\n")
  
  delta_rows <- list()
  for (model_key in models_to_run) {
    baseline_rmse <- results$baseline[[model_key]]$aggregate_metrics$rmse
    news_rmse <- results$news_enhanced[[model_key]]$aggregate_metrics$rmse
    
    baseline_mae <- results$baseline[[model_key]]$aggregate_metrics$mae
    news_mae <- results$news_enhanced[[model_key]]$aggregate_metrics$mae
    
    baseline_ic <- results$baseline[[model_key]]$aggregate_metrics$ic
    news_ic <- results$news_enhanced[[model_key]]$aggregate_metrics$ic
    
    delta_rows[[length(delta_rows) + 1]] <- data.frame(
      Model = model_key,
      Delta_MAE = news_mae - baseline_mae,
      Delta_RMSE = news_rmse - baseline_rmse,
      Delta_IC = news_ic - baseline_ic,
      Interpretation = ifelse(news_rmse < baseline_rmse, "News BETTER", 
                              ifelse(news_rmse > baseline_rmse, "News WORSE", "No change")),
      stringsAsFactors = FALSE
    )
  }
  
  delta <- do.call(rbind, delta_rows)
  print(delta)
  
  # Save results
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(config$output_dir, 
                           sprintf("news_aware_experiment_%s_%s.rds", ticker, timestamp))
  
  final_result <- list(
    config = config,
    ticker = ticker,
    n_news_events = nrow(ticker_news),
    news_coverage = news_feats$metadata$news_coverage,
    baseline_results = results$baseline,
    news_enhanced_results = results$news_enhanced,
    comparison = comparison,
    delta = delta,
    timestamp = Sys.time()
  )
  
  saveRDS(final_result, output_file)
  cat(sprintf("\nResults saved to: %s\n", output_file))
  
  invisible(final_result)
}

# =============================================================================
# MULTI-TICKER EXPERIMENT
# =============================================================================

#' Run comparative experiment across all configured tickers.
#'
#' @param config Configuration list.
#' @return list with results for all tickers.
run_multi_ticker_news_experiment <- function(config = PHASE_H3B_CONFIG) {
  
  cat("\n=== MULTI-TICKER NEWS-AWARE EXPERIMENT ===\n")
  cat(sprintf("Tickers: %s\n", paste(config$tickers, collapse = ", ")))
  cat(sprintf("Period: %s to %s\n", config$start_date, config$end_date))
  
  results <- list()
  
  for (ticker in config$tickers) {
    tryCatch({
      results[[ticker]] <- run_comparative_experiment(ticker, config)
    }, error = function(e) {
      cat(sprintf("\nERROR for %s: %s\n", ticker, conditionMessage(e)))
      results[[ticker]] <- list(error = conditionMessage(e), success = FALSE)
    })
  }
  
  # Aggregate summary
  cat("\n=== AGGREGATE SUMMARY ===\n")
  
  summary_rows <- list()
  for (ticker in names(results)) {
    if (!is.null(results[[ticker]]$delta)) {
      for (model in unique(results[[ticker]]$delta$Model)) {
        row <- results[[ticker]]$delta[results[[ticker]]$delta$Model == model, ]
        summary_rows[[length(summary_rows) + 1]] <- data.frame(
          Ticker = ticker,
          Model = model,
          Delta_RMSE = row$Delta_RMSE,
          Interpretation = row$Interpretation,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(summary_rows) > 0) {
    summary_df <- do.call(rbind, summary_rows)
    print(summary_df)
  }
  
  # Save aggregate
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(config$output_dir, 
                           sprintf("multi_ticker_news_experiment_%s.rds", timestamp))
  
  saveRDS(results, output_file)
  cat(sprintf("\nMulti-ticker results saved to: %s\n", output_file))
  
  invisible(results)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

if (!interactive() && sys.nframe() == 0) {
  # Run single ticker experiment when executed directly
  result <- run_comparative_experiment("AAPL", PHASE_H3B_CONFIG)
}
