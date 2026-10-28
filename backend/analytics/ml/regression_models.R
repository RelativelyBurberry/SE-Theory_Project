# AlphaForge Insight — Regression models for time-series forecasting (Phase H)
#
# Implements regression models for 5-day forward return prediction:
#   - Zero-return naive baseline
#   - Historical-mean baseline (training-data mean, optionally expanding)
#   - Random Forest regression
#   - XGBoost regression
#   - LSTM regression
#
# All models share the same prediction contract:
#   train_fn(X_train, y_train, ...) -> handle
#   predict_fn(handle, X_new) -> numeric vector of predicted returns
#
# Preprocessing: Same as classification pipeline (fit on train only).
# Scaling: LSTM uses scaled features; tree models use raw features.

library(xts)

# =============================================================================
# BASELINE MODELS
# =============================================================================

#' Zero-return naive baseline.
#'
#' Predicts 0 for every observation (random walk assumption: expected return = 0).
#' This is the simplest possible baseline.
#'
#' @return list(model = "zero_return", predict = function(X) rep(0, nrow(X))).
train_zero_return_baseline <- function() {
  list(
    model = "zero_return",
    predict = function(X) rep(0, nrow(X))
  )
}

#' Historical-mean baseline.
#'
#' Predicts the mean of training-period forward returns for all test observations.
#' Optionally uses an expanding window (mean of all past returns up to time t).
#'
#' IMPORTANT: Never uses test or future data. The expanding mean uses only
#' rows 1:t-1 when predicting for row t.
#'
#' @param y_train Numeric vector of training targets.
#' @param use_expanding If TRUE, use expanding mean for each prediction;
#'   if FALSE (default), use fixed training mean for all predictions.
#' @param min_obs Minimum observations required for expanding mean (default 20).
#' @return list(model = "historical_mean", mean = training_mean, predict = fn).
train_historical_mean_baseline <- function(y_train, use_expanding = FALSE, min_obs = 20) {
  y_train <- as.numeric(y_train)
  y_train <- y_train[!is.na(y_train)]
  
  if (length(y_train) < min_obs) {
    stop(sprintf(
      "Historical mean baseline requires at least %d training observations (got %d).",
      min_obs, length(y_train)
    ))
  }
  
  training_mean <- mean(y_train, na.rm = TRUE)
  
  if (!use_expanding) {
    # Fixed training mean for all predictions.
    list(
      model = "historical_mean",
      mean = training_mean,
      use_expanding = FALSE,
      predict = function(X) rep(training_mean, nrow(X))
    )
  } else {
    # Expanding mean: caller must provide full target vector with train + test.
    # For walk-forward evaluation, this is handled separately.
    list(
      model = "historical_mean_expanding",
      mean = training_mean,
      use_expanding = TRUE,
      min_obs = min_obs,
      predict = function(X) {
        # For standalone prediction, fall back to training mean.
        # Expanding mean requires context of all past predictions.
        rep(training_mean, nrow(X))
      }
    )
  }
}

# =============================================================================
# TREE-BASED REGRESSION MODELS
# =============================================================================

#' Random Forest regression model.
#'
#' Uses randomForest package with regression mode (not classification).
#' No scaling needed for tree models.
#'
#' @param X_train Training feature matrix.
#' @param y_train Numeric vector of training targets.
#' @param hyperparams list(ntree = 300, mtry = NULL, nodesize = 5).
#' @return list(model, scaler = NULL, feature_names, needs_scaling = FALSE,
#'   predict = function(X_new) -> numeric vector).
train_random_forest_regression <- function(X_train, y_train, hyperparams = list()) {
  hp <- hyperparams
  ntree <- as.integer(hp$ntree %||% 300)
  nodesize <- as.integer(hp$nodesize %||% 5)
  mtry <- if (!is.null(hp$mtry)) as.integer(hp$mtry) else NULL
  
  y_train <- as.numeric(y_train)
  ok <- !is.na(y_train)
  if (sum(ok) < 10) {
    stop("RF regression requires at least 10 non-NA training observations.")
  }
  
  X_train <- X_train[ok, , drop = FALSE]
  y_train <- y_train[ok]
  
  df <- as.data.frame(X_train)
  names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- y_train
  
  form <- stats::as.formula(paste(".y ~", paste(names(df)[names(df) != ".y"], collapse = " + ")))
  
  rf_args <- list(formula = form, data = df, ntree = ntree, nodesize = nodesize)
  if (!is.null(mtry)) rf_args$mtry <- mtry
  
  fit <- do.call(randomForest::randomForest, rf_args)
  
  feature_names <- colnames(X_train)
  importance <- sort(randomForest::importance(fit)[, "IncNodePurity", drop = TRUE], decreasing = TRUE)
  attr(fit, "af_importance") <- importance
  
  list(
    model = fit,
    scaler = NULL,
    feature_names = feature_names,
    needs_scaling = FALSE,
    predict = function(X_new) {
      dn <- as.data.frame(X_new)
      names(dn) <- make.names(feature_names, unique = TRUE)
      as.numeric(stats::predict(fit, newdata = dn))
    }
  )
}

#' XGBoost regression model.
#'
#' Uses xgboost package with reg:squarederror objective.
#' No scaling needed for tree models.
#'
#' @param X_train Training feature matrix.
#' @param y_train Numeric vector of training targets.
#' @param hyperparams list(max_depth, learning_rate, n_estimators, subsample, colsample_bytree).
#' @return list(model, scaler = NULL, feature_names, needs_scaling = FALSE, predict).
train_xgboost_regression <- function(X_train, y_train, hyperparams = list()) {
  hp <- hyperparams
  max_depth <- as.integer(hp$max_depth %||% 3)
  eta <- as.numeric(hp$learning_rate %||% 0.05)
  nrounds <- as.integer(hp$n_estimators %||% 200)
  subsample <- as.numeric(hp$subsample %||% 0.8)
  colsample <- as.numeric(hp$colsample_bytree %||% 0.8)
  
  y_train <- as.numeric(y_train)
  ok <- !is.na(y_train)
  if (sum(ok) < 10) {
    stop("XGBoost regression requires at least 10 non-NA training observations.")
  }
  
  X_train <- X_train[ok, , drop = FALSE]
  y_train <- y_train[ok]
  
  params <- list(
    objective = "reg:squarederror",
    max_depth = max_depth,
    eta = eta,
    subsample = subsample,
    colsample_bytree = colsample
  )
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  fit <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = 0)
  
  feature_names <- colnames(X_train)
  importance <- xgboost::xgb.importance(model = fit)
  imp_vec <- if (!is.null(importance) && nrow(importance) > 0) {
    v <- setNames(importance$Gain, importance$Feature)
    v[feature_names]
  } else setNames(rep(0, length(feature_names)), feature_names)
  imp_vec[is.na(imp_vec)] <- 0
  attr(fit, "af_importance") <- sort(imp_vec, decreasing = TRUE)
  attr(fit, "af_feature_names") <- feature_names
  
  list(
    model = fit,
    scaler = NULL,
    feature_names = feature_names,
    needs_scaling = FALSE,
    predict = function(X_new) {
      dtest <- xgboost::xgb.DMatrix(data = as.matrix(X_new))
      as.numeric(stats::predict(fit, dtest))
    }
  )
}

# =============================================================================
# LSTM REGRESSION MODEL
# =============================================================================

#' Train LSTM regression model for time-series forecasting.
#'
#' Architecture: LSTM → Dropout → Dense(1) with linear activation.
#' Uses MSE loss (regression), not cross-entropy.
#'
#' @param X_train_scaled Preprocessed AND scaled training matrix.
#' @param y_train Numeric vector of training targets.
#' @param hyperparams list(sequence_length, units, learning_rate, epochs, batch_size, dropout).
#' @param X_val_scaled Optional validation matrix for early stopping.
#' @param y_val Optional validation targets.
#' @return list(model, sequence_length, feature_names, needs_scaling = TRUE, predict).
train_lstm_regression <- function(X_train_scaled, y_train, hyperparams = list(),
                                  X_val_scaled = NULL, y_val = NULL) {
  # Check Keras availability.
  ok <- requireNamespace("keras", quietly = TRUE)
  if (!ok) {
    stop("The 'keras' R package is required for LSTM regression but is not installed.")
  }
  
  keras_avail <- tryCatch(keras::is_keras_available(), error = function(e) FALSE)
  if (!isTRUE(keras_avail)) {
    stop("LSTM requires the keras/TensorFlow runtime. Run keras::install_keras() first.")
  }
  
  hp <- hyperparams
  L <- as.integer(hp$sequence_length %||% 20L)
  units <- as.integer(hp$units %||% 32)
  lr <- as.numeric(hp$learning_rate %||% 0.001)
  epochs <- as.integer(hp$epochs %||% 20)
  batch <- as.integer(hp$batch_size %||% 32)
  dropout <- as.numeric(hp$dropout %||% 0.2)
  
  # Source sequence builder.
  ml_dir <- get("ml_dir", envir = parent.frame())
  if (is.null(ml_dir)) {
    # Fallback: use the global ML_DIR
    ml_dir <- tryCatch(get(".ML_DIR", envir = globalenv()), error = function(e) NULL)
    if (is.null(ml_dir)) {
      stop("ml_dir not found. Set .ML_DIR globally or pass ml_dir in parent frame.")
    }
  }
  source(file.path(ml_dir, "lstm.R"), local = TRUE)
  
  # Build sequences.
  seqs <- build_sequences(X_train_scaled, L)
  if (length(seqs$start) < 10) {
    stop(sprintf(
      "LSTM regression needs at least 10 full %d-day windows in training data (got %d).",
      L, length(seqs$start)
    ))
  }
  
  # Align targets to sequence ends.
  y_at <- as.numeric(y_train)[seqs$start]
  keep <- which(!is.na(y_at))
  
  if (length(keep) < 10) {
    stop("LSTM regression needs at least 10 non-NA target values aligned to sequences.")
  }
  
  Xseq <- seq_subset(seqs$data, keep)
  yseq <- as.numeric(y_at[keep])
  
  # Set seed for reproducibility.
  set.seed(42)
  tryCatch(tensorflow::tf$random$set_seed(42L), error = function(e) NULL)
  
  # Build model: LSTM → Dropout → Dense(1) with linear activation.
  model <- keras::keras_model_sequential(name = "af_lstm_reg", input_shape = c(L, ncol(X_train_scaled)))
  
  # Keras 3 compatibility patch (same as classification LSTM).
  class(model) <- c(class(model),
                    "keras.engine.sequential.Sequential",
                    "keras.models.Sequential",
                    "keras.engine.training.Model")
  
  model <- model |>
    keras::layer_lstm(units = units, return_sequences = FALSE) |>
    keras::layer_dropout(rate = dropout) |>
    keras::layer_dense(units = 16, activation = "relu") |>
    keras::layer_dense(units = 1, activation = "linear")  # Regression: single output
  
  keras::compile(model,
                 optimizer = keras::optimizer_adam(learning_rate = lr),
                 loss = "mse",
                 metrics = "mae")
  
  fit_args <- list(model, Xseq, yseq, epochs = epochs, batch_size = batch,
                   verbose = 0, shuffle = FALSE)
  
  # Optional validation for early stopping.
  if (!is.null(X_val_scaled) && !is.null(y_val)) {
    vseq <- build_sequences(X_val_scaled, L)
    y_at_v <- as.numeric(y_val)[vseq$start]
    keep_v <- !is.na(y_at_v)
    if (sum(keep_v) > 0) {
      fit_args$validation_data <- list(seq_subset(vseq$data, which(keep_v)),
                                       as.numeric(y_at_v[keep_v]))
    }
  }
  
  do.call(keras::fit, fit_args)
  
  # Prediction function: returns predictions for all rows.
  # Rows before the first full window get the last valid prediction (or 0).
  predict_fn <- function(X_new_scaled) {
    n <- nrow(X_new_scaled)
    preds <- rep(0, n)  # Default for rows without full sequences.
    
    if (n >= L) {
      s <- build_sequences(X_new_scaled, L)
      if (length(s$start) > 0) {
        p <- as.numeric(stats::predict(model, s$data, verbose = 0))
        preds[s$start] <- p
        # Backfill: use the first prediction for earlier rows.
        if (s$start[1] > 1) {
          preds[1:(s$start[1] - 1)] <- p[1]
        }
      }
    }
    
    preds
  }
  
  list(
    model = model,
    sequence_length = L,
    feature_names = colnames(X_train_scaled),
    needs_scaling = TRUE,
    predict = predict_fn
  )
}

# =============================================================================
# REGISTRY
# =============================================================================

#' Registry of regression models with metadata.
REGRESSION_MODELS <- list(
  zero_return = list(
    label = "Zero-Return Naive Baseline",
    type = "baseline",
    needs_scaling = FALSE,
    train_fn = "train_zero_return_baseline"
  ),
  historical_mean = list(
    label = "Historical-Mean Baseline",
    type = "baseline",
    needs_scaling = FALSE,
    train_fn = "train_historical_mean_baseline"
  ),
  random_forest = list(
    label = "Random Forest Regression",
    type = "tree",
    needs_scaling = FALSE,
    train_fn = "train_random_forest_regression",
    defaults = list(ntree = 300, mtry = NULL, nodesize = 5)
  ),
  xgboost = list(
    label = "XGBoost Regression",
    type = "boosting",
    needs_scaling = FALSE,
    train_fn = "train_xgboost_regression",
    defaults = list(max_depth = 3, learning_rate = 0.05, n_estimators = 200,
                    subsample = 0.8, colsample_bytree = 0.8)
  ),
  lstm = list(
    label = "LSTM Regression",
    type = "sequence",
    needs_scaling = TRUE,
    train_fn = "train_lstm_regression",
    defaults = list(sequence_length = 20, units = 32, learning_rate = 0.001,
                    epochs = 20, batch_size = 32, dropout = 0.2)
  )
)

REGRESSION_MODEL_KEYS <- names(REGRESSION_MODELS)
