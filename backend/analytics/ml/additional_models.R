# AlphaForge Insight — Additional Time-Series Models (Phase H.6)
#
# Implements focused set of time-series forecasting models:
#   - Elastic Net regression (regularized linear model)
#   - ARIMA (auto-regressive integrated moving average)
#   - ETS (exponential smoothing)
#   - GRU (gated recurrent unit - simplified LSTM)
#
# All models follow the same prediction contract as existing regression models.

library(xts)
library(zoo)

# =============================================================================
# ELASTIC NET REGRESSION
# =============================================================================

#' Train Elastic Net regression model.
#'
#' Regularized linear regression combining L1 (lasso) and L2 (ridge) penalties.
#' Good baseline for feature importance and robust with limited signal.
#'
#' @param X_train Training feature matrix.
#' @param y_train Numeric vector of training targets.
#' @param hyperparams list(alpha = 1.0, lambda = NULL).
#'   - alpha: mixing parameter (1 = lasso, 0 = ridge, 0.5 = elastic net)
#'   - lambda: regularization strength (NULL = cross-validated)
#' @return list(model, scaler = NULL, feature_names, needs_scaling = TRUE, predict).
train_elastic_net_regression <- function(X_train, y_train, hyperparams = list()) {
  
  # Check for glmnet
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Elastic Net requires the 'glmnet' package. Run install.packages('glmnet').")
  }
  
  hp <- hyperparams
  alpha <- as.numeric(hp$alpha %||% 1.0)  # Default to lasso
  lambda <- if (!is.null(hp$lambda)) as.numeric(hp$lambda) else NULL
  
  y_train <- as.numeric(y_train)
  ok <- !is.na(y_train)
  if (sum(ok) < 10) {
    stop("Elastic Net requires at least 10 non-NA training observations.")
  }
  
  X_train <- as.matrix(X_train[ok, , drop = FALSE])
  y_train <- y_train[ok]
  
  # Handle missing values in features
  if (any(is.na(X_train))) {
    medians <- apply(X_train, 2, median, na.rm = TRUE)
    for (j in seq_len(ncol(X_train))) {
      X_train[is.na(X_train[, j]), j] <- medians[j]
    }
  }
  
  feature_names <- colnames(X_train)
  
  # Fit model
  if (is.null(lambda)) {
    # Cross-validated lambda selection
    cv_fit <- glmnet::cv.glmnet(X_train, y_train, alpha = alpha, nfolds = 5)
    lambda <- cv_fit$lambda.min
    fit <- cv_fit$glmnet.fit
  } else {
    fit <- glmnet::glmnet(X_train, y_train, alpha = alpha, lambda = lambda)
  }
  
  # Feature importance (absolute coefficients at selected lambda)
  coefs <- as.numeric(coef(fit, s = lambda))[-1]  # Remove intercept
  importance <- abs(coefs)
  names(importance) <- feature_names
  
  list(
    model = fit,
    lambda = lambda,
    alpha = alpha,
    scaler = NULL,
    feature_names = feature_names,
    needs_scaling = TRUE,
    importance = sort(importance, decreasing = TRUE),
    predict = function(X_new) {
      X_new <- as.matrix(X_new)
      # Handle missing values
      if (any(is.na(X_new))) {
        for (j in seq_len(ncol(X_new))) {
          X_new[is.na(X_new[, j]), j] <- median(X_new[, j], na.rm = TRUE)
        }
      }
      as.numeric(stats::predict(fit, newx = X_new, s = lambda))
    }
  )
}

# =============================================================================
# ARIMA REGRESSION
# =============================================================================

#' Train ARIMA model for time-series forecasting.
#'
#' Uses auto.arima for automatic order selection. Forecasts are made
#' on the target series directly (forward returns), not prices.
#'
#' @param y_train Numeric vector of training targets.
#' @param hyperparams list(max_p = 5, max_q = 5, max_d = 2).
#' @return list(model, needs_scaling = FALSE, predict).
train_arima_regression <- function(y_train, hyperparams = list()) {
  
  # Check for forecast package
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("ARIMA requires the 'forecast' package. Run install.packages('forecast').")
  }
  
  hp <- hyperparams
  max_p <- as.integer(hp$max_p %||% 5L)
  max_q <- as.integer(hp$max_q %||% 5L)
  max_d <- as.integer(hp$max_d %||% 2L)
  
  y_train <- as.numeric(y_train)
  y_train <- y_train[!is.na(y_train)]
  
  if (length(y_train) < 20) {
    stop("ARIMA requires at least 20 training observations.")
  }
  
  # Fit auto.arima
  fit <- tryCatch({
    forecast::auto.arima(y_train, max.p = max_p, max.q = max_q, max.d = max_d,
                         seasonal = FALSE, stepwise = TRUE, approximation = TRUE,
                         trace = FALSE)
  }, error = function(e) {
    # Fallback to simple AR(1)
    forecast::Arima(y_train, order = c(1, 0, 0))
  })
  
  list(
    model = fit,
    order = fit$arma[c(1, 6, 2)],  # p, d, q
    needs_scaling = FALSE,
    predict = function(X_new, horizon = 1) {
      # ARIMA doesn't use features, only predicts forward
      n <- nrow(X_new)
      if (is.null(n)) n <- length(X_new)
      
      # Forecast n steps ahead
      fc <- forecast::forecast(fit, h = n)
      as.numeric(fc$mean)
    }
  )
}

# =============================================================================
# ETS (EXPONENTIAL SMOOTHING)
# =============================================================================

#' Train ETS model for time-series forecasting.
#'
#' Exponential smoothing state space model. Automatically selects
#' error, trend, and seasonality components.
#'
#' @param y_train Numeric vector of training targets.
#' @param hyperparams list(model = NULL) - NULL for automatic selection.
#' @return list(model, needs_scaling = FALSE, predict).
train_ets_regression <- function(y_train, hyperparams = list()) {
  
  # Check for forecast package
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("ETS requires the 'forecast' package. Run install.packages('forecast').")
  }
  
  hp <- hyperparams
  model_type <- hp$model %||% NULL  # NULL = automatic
  
  y_train <- as.numeric(y_train)
  y_train <- y_train[!is.na(y_train)]
  
  if (length(y_train) < 10) {
    stop("ETS requires at least 10 training observations.")
  }
  
  # Fit ETS
  fit <- tryCatch({
    forecast::ets(y_train, model = model_type)
  }, error = function(e) {
    # Fallback to simple exponential smoothing
    forecast::ets(y_train, model = "ANN")
  })
  
  list(
    model = fit,
    model_type = fit$method,
    needs_scaling = FALSE,
    predict = function(X_new, horizon = 1) {
      n <- nrow(X_new)
      if (is.null(n)) n <- length(X_new)
      
      fc <- forecast::forecast(fit, h = n)
      as.numeric(fc$mean)
    }
  )
}

# =============================================================================
# GRU (GATED RECURRENT UNIT)
# =============================================================================

#' Train GRU regression model for time-series forecasting.
#'
#' Simpler alternative to LSTM with fewer parameters.
#' Architecture: GRU -> Dropout -> Dense(1)
#'
#' @param X_train_scaled Preprocessed AND scaled training matrix.
#' @param y_train Numeric vector of training targets.
#' @param hyperparams list(sequence_length, units, learning_rate, epochs, batch_size, dropout).
#' @param X_val_scaled Optional validation matrix.
#' @param y_val Optional validation targets.
#' @return list(model, sequence_length, feature_names, needs_scaling = TRUE, predict).
train_gru_regression <- function(X_train_scaled, y_train, hyperparams = list(),
                                  X_val_scaled = NULL, y_val = NULL) {
  
  # Check Keras availability
  ok <- requireNamespace("keras", quietly = TRUE)
  if (!ok) {
    stop("The 'keras' R package is required for GRU regression but is not installed.")
  }
  
  keras_avail <- tryCatch(keras::is_keras_available(), error = function(e) FALSE)
  if (!isTRUE(keras_avail)) {
    stop("GRU requires the keras/TensorFlow runtime. Run keras::install_keras() first.")
  }
  
  hp <- hyperparams
  L <- as.integer(hp$sequence_length %||% 20L)
  units <- as.integer(hp$units %||% 16)
  lr <- as.numeric(hp$learning_rate %||% 0.001)
  epochs <- as.integer(hp$epochs %||% 30)
  batch <- as.integer(hp$batch_size %||% 32)
  dropout <- as.numeric(hp$dropout %||% 0.2)
  
  # Source sequence builder
  ml_dir <- tryCatch(get(".ML_DIR", envir = globalenv()), error = function(e) NULL)
  if (is.null(ml_dir)) {
    ml_dir <- file.path(getwd(), "backend", "analytics", "ml")
  }
  source(file.path(ml_dir, "lstm.R"), local = TRUE)
  
  # Build sequences
  seqs <- build_sequences(X_train_scaled, L)
  if (length(seqs$start) < 10) {
    stop(sprintf("GRU needs at least 10 full %d-day windows in training data.", L))
  }
  
  y_at <- as.numeric(y_train)[seqs$start]
  keep <- which(!is.na(y_at))
  
  if (length(keep) < 10) {
    stop("GRU needs at least 10 non-NA target values aligned to sequences.")
  }
  
  Xseq <- seq_subset(seqs$data, keep)
  yseq <- as.numeric(y_at[keep])
  
  # Set seed
  set.seed(42)
  tryCatch(tensorflow::tf$random$set_seed(42L), error = function(e) NULL)
  
  # Build model
  model <- keras::keras_model_sequential(name = "af_gru_reg", input_shape = c(L, ncol(X_train_scaled)))
  class(model) <- c(class(model),
                    "keras.engine.sequential.Sequential",
                    "keras.models.Sequential",
                    "keras.engine.training.Model")
  
  model <- model |>
    keras::layer_gru(units = units, return_sequences = FALSE) |>
    keras::layer_dropout(rate = dropout) |>
    keras::layer_dense(units = 1, activation = "linear")
  
  keras::compile(model,
                 optimizer = keras::optimizer_adam(learning_rate = lr),
                 loss = "mse",
                 metrics = "mae")
  
  fit_args <- list(model, Xseq, yseq, epochs = epochs, batch_size = batch,
                   verbose = 0, shuffle = FALSE)
  
  # Optional validation
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
  
  # Prediction function
  predict_fn <- function(X_new_scaled) {
    n <- nrow(X_new_scaled)
    preds <- rep(0, n)
    
    if (n >= L) {
      s <- build_sequences(X_new_scaled, L)
      if (length(s$start) > 0) {
        p <- as.numeric(stats::predict(model, s$data, verbose = 0))
        preds[s$start] <- p
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
    units = units,
    feature_names = colnames(X_train_scaled),
    needs_scaling = TRUE,
    predict = predict_fn
  )
}

# =============================================================================
# REGISTRY UPDATE
# =============================================================================

# Add new models to the registry (to be merged with REGRESSION_MODELS)
ADDITIONAL_REGRESSION_MODELS <- list(
  elastic_net = list(
    label = "Elastic Net Regression",
    type = "regularized_linear",
    needs_scaling = TRUE,
    train_fn = "train_elastic_net_regression",
    defaults = list(alpha = 1.0, lambda = NULL)
  ),
  arima = list(
    label = "ARIMA",
    type = "time_series",
    needs_scaling = FALSE,
    train_fn = "train_arima_regression",
    defaults = list(max_p = 5, max_q = 5, max_d = 2)
  ),
  ets = list(
    label = "ETS (Exponential Smoothing)",
    type = "time_series",
    needs_scaling = FALSE,
    train_fn = "train_ets_regression",
    defaults = list(model = NULL)
  ),
  gru = list(
    label = "GRU Regression",
    type = "sequence",
    needs_scaling = TRUE,
    train_fn = "train_gru_regression",
    defaults = list(sequence_length = 20, units = 16, learning_rate = 0.001,
                    epochs = 30, batch_size = 32, dropout = 0.2)
  )
)

ADDITIONAL_MODEL_KEYS <- names(ADDITIONAL_REGRESSION_MODELS)
