# AlphaForge Insight — LSTM sequence model (ML Phase D)
#
# ISOLATED from the tabular pipeline (ML plan section 8): the LSTM consumes
# rolling sequences of shape samples x timesteps x features (previous
# `sequence_length` trading days -> BUY/HOLD/SELL), NOT single rows. It still
# produces the SAME standardized prediction contract (prob matrix + records)
# as every other model, so the adapter and comparison table stay uniform.
#
# Implementation: keras (TensorFlow backend) via the reticulate environment
# installed by keras::install_keras(). If the environment is unavailable the
# train call fails with a clear, client-safe message — other models in the
# comparison are never blocked (engineering rule 18).
#
# Leakage notes:
#   - The feature matrix is scaled with the TRAIN-FIT scaler (never re-fit).
#   - Sequence windows never span the chronological split boundaries: each
#     split builds windows entirely inside its own rows.
#   - A sample at date t uses rows t-(L-1)..t (all <= t); its label at t uses
#     Close[t + horizon] (label side only, as everywhere else).

library(xts)
suppressPackageStartupMessages(library(TTR))

LSTM_DEFAULT_SEQUENCE_LENGTH <- 20L

#' Subset sequence windows from the 3-D samples x timesteps x features array.
#'
#' R 4.6 rejects `arr[idx, , drop = FALSE]` (empty middle index) on 3-D
#' arrays, so all three dimensions are indexed explicitly here.
#'
#' @param data 3-D sequence array. @param idx Integer row indices.
#' @return Sub-array with the same 3 dimensions reduced along dim 1.
seq_subset <- function(data, idx) {
  idx <- as.integer(idx)
  data[idx, seq_len(dim(data)[2]), seq_len(dim(data)[3]), drop = FALSE]
}

#' Build rolling sequence windows from a feature matrix.
#'
#' @param X Numeric matrix (rows = trading days, chronological).
#' @param sequence_length Window length L (>= 2).
#' @return list(start = integer row indices of each window's LAST day,
#'              data = 3-D array c(samples, L, n_features)).
build_sequences <- function(X, sequence_length = LSTM_DEFAULT_SEQUENCE_LENGTH) {
  L <- as.integer(sequence_length)
  if (is.na(L) || L < 2) {
    stop(validation_error("sequence_length must be an integer >= 2."))
  }
  n <- nrow(X)
  if (n < L) {
    return(list(start = integer(0), data = array(numeric(0), dim = c(0, L, ncol(X)))))
  }
  starts <- L:n
  data <- array(unlist(lapply(starts, function(s) X[(s - L + 1):s, , drop = FALSE])),
                dim = c(length(starts), L, ncol(X)))
  list(start = starts, data = data)
}

#' Ensure the keras/TF backend is usable; return invisibly or fail clearly.
.ensure_keras <- function() {
  ok <- requireNamespace("keras", quietly = TRUE)
  if (!ok) {
    stop("The 'keras' R package is required for the LSTM model but is not installed.")
  }
  usable <- tryCatch({
    keras::is_keras_available()
  }, error = function(e) FALSE)
  if (!isTRUE(usable)) {
    stop("LSTM requires the keras/TensorFlow runtime. Run keras::install_keras() once to install the Python environment, then retry.")
  }
  invisible(TRUE)
}

#' Train the LSTM sequence model.
#'
#' @param X_train_scaled Preprocessed + SCALED training matrix.
#' @param y_train Character class vector aligned with X_train_scaled rows.
#' @param hyperparams Merged LSTM hyperparameters (see ML_MODELS$lstm).
#' @param X_val_scaled Optional scaled validation matrix (early monitoring).
#' @param y_val Optional validation labels.
#' @return Model handle: list(model, sequence_length, feature_names,
#'   predict = function(X_new_scaled) -> n x 3 probability matrix aligned to
#'   the LAST sequence_length-1..n rows? No — aligned to ALL input rows: rows
#'   before the first full window receive uniform 1/3 probabilities).
predict_lstm_handle <- NULL # forward declaration guard for linters

train_lstm_model <- function(X_train_scaled, y_train,
                             hyperparams = list(),
                             X_val_scaled = NULL, y_val = NULL) {
  .ensure_keras()
  hp <- hyperparams
  L <- as.integer(hp$sequence_length %||% LSTM_DEFAULT_SEQUENCE_LENGTH)
  units <- as.integer(hp$units %||% 32)
  lr <- as.numeric(hp$learning_rate %||% 0.001)
  epochs <- as.integer(hp$epochs %||% 20)
  batch <- as.integer(hp$batch_size %||% 32)
  dropout <- as.numeric(hp$dropout %||% 0.2)

  seqs <- build_sequences(X_train_scaled, L)
  if (length(seqs$start) < 10) {
    stop(validation_error(sprintf(
      "LSTM needs at least 10 full %d-day windows in training data (got %d).",
      L, length(seqs$start)
    )))
  }
  y_at <- class_to_integer(y_train)[seqs$start]
  keep <- which(!is.na(y_at))
  Xseq <- seq_subset(seqs$data, keep)
  yseq <- y_at[keep] - 1L  # 0-based for sparse categorical loss

  set.seed(42)
  # This keras build exposes neither set_seed nor set_random_seed; seed the
  # TF graph directly when available (best-effort determinism).
  tryCatch(
    tensorflow::tf$random$set_seed(42L),
    error = function(e) NULL
  )
  model <- keras::keras_model_sequential(name = "af_lstm", input_shape = c(L, ncol(X_train_scaled)))
  # Keras 3 compatibility: the installed Python keras (3.x) exposes model
  # classes under `keras.src.models.sequential.Sequential`, while the keras R
  # package 2.16.1 registers its S3 methods (compose_layer used by the layer
  # pipes, compile, fit, predict) under the Keras 2-era class names
  # (`keras.engine.sequential.Sequential`, `keras.models.Sequential`,
  # `keras.engine.training.Model`). Without these, dispatch falls through to
  # compose_layer.default, which calls the Keras 3 layer as a callable with the
  # model passed POSITIONALLY as the input tensor, raising Keras 3's
  # "Only input tensors may be passed as positional arguments". Appending the
  # legacy class names lets the R wrappers route to the same object$add /
  # object$compile / object$fit / object$predict calls they make for Keras 2.
  class(model) <- c(class(model),
                    "keras.engine.sequential.Sequential",
                    "keras.models.Sequential",
                    "keras.engine.training.Model")
  model <- model |>
    keras::layer_lstm(units = units, return_sequences = FALSE) |>
    keras::layer_dropout(rate = dropout) |>
    keras::layer_dense(units = 16, activation = "relu") |>
    keras::layer_dense(3, activation = "softmax")
  keras::compile(model,
                 optimizer = keras::optimizer_adam(learning_rate = lr),
                 loss = "sparse_categorical_crossentropy",
                 metrics = "accuracy")

  fit_args <- list(model, Xseq, yseq, epochs = epochs, batch_size = batch,
                   verbose = 0, shuffle = FALSE)
  if (!is.null(X_val_scaled) && !is.null(y_val)) {
    vseq <- build_sequences(X_val_scaled, L)
    y_at_v <- class_to_integer(y_val)[vseq$start]
    keep_v <- !is.na(y_at_v)
    if (sum(keep_v) > 0) {
      fit_args$validation_data <- list(seq_subset(vseq$data, which(keep_v)),
                                       y_at_v[keep_v] - 1L)
    }
  }
  do.call(keras::fit, fit_args)

  predict_fn <- function(X_new_scaled) {
    n <- nrow(X_new_scaled)
    P <- matrix(1 / 3, nrow = n, ncol = 3)
    if (n >= L) {
      s <- build_sequences(X_new_scaled, L)
      if (length(s$start) > 0) {
        p <- stats::predict(model, s$data, verbose = 0)
        p <- as.matrix(p)
        if (is.vector(p) && length(p) == 3) p <- matrix(p, nrow = 1, ncol = 3)
        if (is.null(dim(p)) || ncol(p) != 3) {
          stop("LSTM produced an unexpected probability shape.")
        }
        P[s$start, ] <- p
      }
    }
    colnames(P) <- CLASS_LEVELS
    P
  }

  list(
    model = model,
    sequence_length = L,
    feature_names = colnames(X_train_scaled),
    needs_scaling = TRUE,
    predict = predict_fn
  )
}
