# AlphaForge Insight — Walk-forward evaluation for time-series forecasting (Phase H)
#
# Implements a rigorous out-of-sample evaluation framework:
#   1. Walk-forward splits with embargo to prevent leakage
#   2. All preprocessing/feature selection fit on training folds only
#   3. Identical test dates for all models
#   4. Forecast metrics: MAE, RMSE, directional accuracy, IC
#   5. Trading metrics via existing execution engine
#
# Alignment convention:
#   - Prediction at row t forecasts the return from Close[t] to Close[t+horizon].
#   - Execution can only occur at Open[t+1] or later.
#   - The realized return from Open[t+1] to Close[t+horizon] differs from the
#     forecast target (Close[t] to Close[t+horizon]) by one day of price movement.

library(xts)

# =============================================================================
# FORECAST METRICS
# =============================================================================

#' Compute forecast metrics for regression predictions.
#'
#' @param predicted Numeric vector of predicted returns.
#' @param actual Numeric vector of actual returns (same length).
#' @return list(mae, rmse, directional_accuracy, ic = correlation, n_obs).
forecast_metrics <- function(predicted, actual) {
  predicted <- as.numeric(predicted)
  actual <- as.numeric(actual)
  
  # Remove NA pairs.
  ok <- !is.na(predicted) & !is.na(actual)
  if (sum(ok) < 2) {
    return(list(
      mae = NA_real_,
      rmse = NA_real_,
      directional_accuracy = NA_real_,
      ic = NA_real_,
      n_obs = sum(ok)
    ))
  }
  
  pred <- predicted[ok]
  act <- actual[ok]
  
  # MAE: Mean Absolute Error.
  mae <- mean(abs(pred - act))
  
  # RMSE: Root Mean Squared Error.
  rmse <- sqrt(mean((pred - act) ^ 2))
  
  # Directional accuracy: proportion of predictions with correct sign.
  correct_sign <- (pred > 0 & act > 0) | (pred < 0 & act < 0) | (pred == 0 & act == 0)
  # For zero returns, count as correct only if both are zero.
  # For non-zero actual, check if prediction has same sign.
  correct_direction <- sign(pred) == sign(act)
  # Handle zero cases: if actual is zero, any prediction is "correct" by convention.
  correct_direction[act == 0] <- TRUE
  directional_accuracy <- mean(correct_direction)
  
  # IC (Information Coefficient): Pearson correlation between predicted and actual.
  # Edge case: if predictions are constant (zero variance), correlation is undefined.
  ic <- tryCatch({
    if (var(pred) < .Machine$double.eps || var(act) < .Machine$double.eps) {
      NA_real_  # Correlation undefined for constant sequences
    } else {
      cor(pred, act, method = "pearson", use = "complete.obs")
    }
  }, error = function(e) NA_real_)
  
  list(
    mae = mae,
    rmse = rmse,
    directional_accuracy = directional_accuracy,
    ic = ic,
    n_obs = sum(ok)
  )
}

# =============================================================================
# WALK-FORWARD EVALUATION
# =============================================================================

#' Run walk-forward evaluation for a single regression model.
#'
#' For each fold:
#'   1. Fit preprocessing on training rows only
#'   2. Fit model on training data
#'   3. Predict on test rows
#'   4. Compute forecast metrics
#'
#' @param ds assemble_regression_dataset() output.
#' @param model_key One of REGRESSION_MODEL_KEYS.
#' @param hyperparams Model hyperparameters (list).
#' @param n_folds Number of walk-forward folds (default 3).
#' @param test_frac Fraction of data per test fold (default 0.1).
#' @param initial_capital Trading capital for backtest.
#' @param return_threshold Minimum predicted return to trigger BUY/SELL signal.
#' @return list(model_key, folds = list of fold results, aggregate_metrics).
evaluate_regression_model_wf <- function(ds, model_key, hyperparams = list(),
                                         n_folds = 3L, test_frac = 0.1,
                                         initial_capital = 10000,
                                         return_threshold = 0.02) {
  # Source dependencies.
  ml_dir <- get("ml_dir", envir = parent.frame())
  if (is.null(ml_dir)) {
    ml_dir <- tryCatch(get(".ML_DIR", envir = globalenv()), error = function(e) NULL)
    if (is.null(ml_dir)) {
      stop("ml_dir not found. Set .ML_DIR globally or pass ml_dir in parent frame.")
    }
  }
  source(file.path(ml_dir, "preprocessing.R"), local = TRUE)
  source(file.path(ml_dir, "regression_models.R"), local = TRUE)
  
  # Generate walk-forward splits.
  splits <- walk_forward_splits(ds$n, n_folds = n_folds, test_frac = test_frac,
                                embargo = ds$horizon)
  
  folds <- list()
  all_preds <- c()
  all_actuals <- c()
  all_dates <- c()
  all_fold_ids <- c()
  
  for (i in seq_along(splits)) {
    fold_split <- splits[[i]]
    train_idx <- fold_split$train
    test_idx <- fold_split$test
    
    # Fit preprocessing on training data only.
    spec <- fit_preprocessing(ds$X_raw[train_idx, , drop = FALSE])
    X_imp <- apply_preprocessing(ds$X_raw, spec)
    
    # Scaling for LSTM.
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
    dates_test <- ds$dates[test_idx]
    
    # Train model.
    model_result <- tryCatch({
      if (model_key == "zero_return") {
        handle <- train_zero_return_baseline()
      } else if (model_key == "historical_mean") {
        handle <- train_historical_mean_baseline(y_train, use_expanding = FALSE)
      } else if (model_key == "random_forest") {
        handle <- train_random_forest_regression(X_train, y_train, hyperparams)
      } else if (model_key == "xgboost") {
        handle <- train_xgboost_regression(X_train, y_train, hyperparams)
      } else if (model_key == "lstm") {
        handle <- train_lstm_regression(X_train, y_train, hyperparams)
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
    
    # Predict on test set.
    predictions <- tryCatch(
      handle$predict(X_test),
      error = function(e) rep(NA_real_, length(test_idx))
    )
    
    # Compute forecast metrics.
    fm <- forecast_metrics(predictions, y_test)
    
    # Store fold results.
    folds[[i]] <- list(
      fold = i,
      success = TRUE,
      n_train = length(train_idx),
      n_test = length(test_idx),
      dates = as.character(dates_test),
      predictions = as.numeric(predictions),
      actuals = as.numeric(y_test),
      metrics = fm
    )
    
    # Aggregate for overall metrics.
    all_preds <- c(all_preds, predictions)
    all_actuals <- c(all_actuals, y_test)
    all_dates <- c(all_dates, as.character(dates_test))
    all_fold_ids <- c(all_fold_ids, rep(i, length(test_idx)))
  }
  
  # Aggregate metrics across all folds.
  aggregate_metrics <- forecast_metrics(all_preds, all_actuals)
  
  list(
    model_key = model_key,
    model_label = REGRESSION_MODELS[[model_key]]$label,
    hyperparams = hyperparams,
    folds = folds,
    aggregate_metrics = aggregate_metrics,
    all_predictions = data.frame(
      date = all_dates,
      fold = all_fold_ids,
      predicted = all_preds,
      actual = all_actuals,
      stringsAsFactors = FALSE
    )
  )
}

#' Run full regression experiment comparing all models.
#'
#' @param ds assemble_regression_dataset() output.
#' @param models Character vector of model keys (NULL = all).
#' @param n_folds Number of walk-forward folds.
#' @param test_frac Fraction of data per test fold.
#' @param seed Random seed for reproducibility.
#' @return list(config, results, comparison_table).
run_regression_experiment <- function(ds, models = NULL, n_folds = 3L,
                                      test_frac = 0.1, seed = 42) {
  set.seed(seed)
  
  model_keys <- if (is.null(models)) REGRESSION_MODEL_KEYS else {
    models <- unique(tolower(as.character(models)))
    models[models %in% REGRESSION_MODEL_KEYS]
  }
  
  if (length(model_keys) == 0) {
    stop("No valid regression model keys provided.")
  }
  
  results <- list()
  for (key in model_keys) {
    message(sprintf("[regression] Running model: %s", key))
    hp <- REGRESSION_MODELS[[key]]$defaults %||% list()
    results[[key]] <- evaluate_regression_model_wf(
      ds, key, hp, n_folds = n_folds, test_frac = test_frac
    )
  }
  
  # Build comparison table.
  rows <- lapply(names(results), function(key) {
    r <- results[[key]]
    m <- r$aggregate_metrics
    data.frame(
      Model = r$model_label,
      MAE = m$mae,
      RMSE = m$rmse,
      Directional_Accuracy = m$directional_accuracy,
      IC = m$ic,
      N_Obs = m$n_obs,
      stringsAsFactors = FALSE
    )
  })
  
  comparison <- do.call(rbind, rows)
  comparison <- comparison[order(comparison$RMSE), ]
  
  list(
    config = list(
      ticker = ds$ticker,
      horizon = ds$horizon,
      n_folds = n_folds,
      test_frac = test_frac,
      seed = seed,
      n_total = ds$n,
      n_features = ds$p
    ),
    results = results,
    comparison = comparison
  )
}
