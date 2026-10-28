# AlphaForge Insight — common ML model interface + tabular models (ML B/C/D)
#
# ONE registry, ONE prediction contract. Every model — tabular or sequence —
# implements:
#
#   train_fn(X_train, y_train, X_val, y_val, hyperparams) -> model handle
#   predict_fn(model, X) -> 3-column probability matrix [BUY, HOLD, SELL]
#
# and produces the standardized prediction record (ML plan section 7):
#   model_name, ticker, date, predicted_class, prob_buy, prob_hold, prob_sell
#
# The backtester never sees any of this — the adapter converts predictions
# into the existing BUY/SELL/NA signal vector (ml/adapter.R).
#
# Scale policy (ML plan rule 4/§4): tree/boosting models and NB receive RAW
# (imputed but unscaled) features; distance/linear/neural models (logistic,
# LDA, QDA, KNN, SVM, MLP) receive z-scored features with TRAIN-FIT scaler.
# Both paths use the SAME fit_preprocessing()/apply_preprocessing() stage so
# training and inference stay consistent.

library(xts)
suppressPackageStartupMessages(library(TTR))

#' Null-coalescing helper (defined once here; guarded so re-sourcing is safe).
if (!exists("%||%", envir = globalenv(), inherits = FALSE)) {
  assign("%||%", function(a, b) if (is.null(a)) b else a, envir = globalenv())
}

# Model metadata registry: label, whether it needs scaled inputs, and default
# hyperparameters. 12 tabular models live here; LSTM is registered separately
# in ml/lstm.R (sequence model, same contract).
#
# Additionally, 12 Python time-series models are registered for the upgraded
# ML Compare feature (see ml/python_ts.R):
#   Baselines: zero_return, historical_mean
#   Statistical: linear_regression, elastic_net, arima, ets, garch
#   ML: random_forest_ts, xgboost_ts, svr
#   Deep Learning: lstm_ts, gru_ts
ML_MODELS <- list(
  logistic_regression = list(
    label = "Logistic Regression", family = "classical", needs_scaling = TRUE,
    defaults = list()
  ),
  lda = list(
    label = "Linear Discriminant Analysis", family = "classical",
    needs_scaling = TRUE, defaults = list()
  ),
  qda = list(
    label = "Quadratic Discriminant Analysis", family = "classical",
    needs_scaling = TRUE, defaults = list()
  ),
  knn = list(
    label = "K-Nearest Neighbors", family = "classical", needs_scaling = TRUE,
    defaults = list(k = 15)
  ),
  naive_bayes = list(
    label = "Naive Bayes", family = "classical", needs_scaling = FALSE,
    defaults = list(laplace = 1)
  ),
  decision_tree = list(
    label = "Decision Tree", family = "tree", needs_scaling = FALSE,
    defaults = list(maxdepth = 5, cp = 0.01, minsplit = 20)
  ),
  random_forest = list(
    label = "Random Forest", family = "tree", needs_scaling = FALSE,
    defaults = list(ntree = 300, mtry = NULL, nodesize = 5)
  ),
  svm = list(
    label = "Support Vector Machine", family = "kernel", needs_scaling = TRUE,
    defaults = list(kernel = "radial", cost = 1, gamma = 0.1)
  ),
  gradient_boosting = list(
    label = "Gradient Boosting", family = "boosting", needs_scaling = FALSE,
    defaults = list(n.trees = 200, interaction.depth = 3, shrinkage = 0.05,
                    n.minobsinnode = 10)
  ),
  xgboost = list(
    label = "XGBoost", family = "boosting", needs_scaling = FALSE,
    defaults = list(max_depth = 3, learning_rate = 0.05, n_estimators = 200,
                    subsample = 0.8, colsample_bytree = 0.8)
  ),
  mlp = list(
    label = "MLP (Neural Network)", family = "neural", needs_scaling = TRUE,
    defaults = list(size = c(32, 16), lambda = 0.01, maxit = 300, threshold = 0.01)
  ),
  lstm = list(
    label = "LSTM (Sequence Model)", family = "sequence", needs_scaling = TRUE,
    defaults = list(sequence_length = 20, units = 32, learning_rate = 0.001,
                    epochs = 20, batch_size = 32, dropout = 0.2)
  ),
  # Python time-series models (prefixed with ts_ to distinguish from R versions)
  ts_zero_return = list(
    label = "Zero-Return Baseline", family = "baseline", needs_scaling = FALSE,
    defaults = list(), python_only = TRUE, is_regression = TRUE
  ),
  ts_historical_mean = list(
    label = "Historical-Mean Baseline", family = "baseline", needs_scaling = FALSE,
    defaults = list(), python_only = TRUE, is_regression = TRUE
  ),
  ts_linear_regression = list(
    label = "Linear Regression (TS)", family = "statistical", needs_scaling = TRUE,
    defaults = list(), python_only = TRUE, is_regression = TRUE
  ),
  ts_elastic_net = list(
    label = "Elastic Net", family = "statistical", needs_scaling = TRUE,
    defaults = list(alpha = 1.0, l1_ratio = 0.5), python_only = TRUE, is_regression = TRUE
  ),
  ts_arima = list(
    label = "ARIMA", family = "statistical", needs_scaling = FALSE,
    defaults = list(order = c(1L, 0L, 0L)), python_only = TRUE, is_regression = TRUE
  ),
  ts_ets = list(
    label = "ETS (Exponential Smoothing)", family = "statistical", needs_scaling = FALSE,
    defaults = list(trend = NULL, seasonal = NULL), python_only = TRUE, is_regression = TRUE
  ),
  ts_garch = list(
    label = "GARCH", family = "statistical", needs_scaling = FALSE,
    defaults = list(p = 1L, q = 1L), python_only = TRUE, is_regression = FALSE, is_volatility = TRUE
  ),
  ts_random_forest = list(
    label = "Random Forest Regression", family = "ml", needs_scaling = FALSE,
    defaults = list(n_estimators = 200L, max_depth = 10L, min_samples_leaf = 5L),
    python_only = TRUE, is_regression = TRUE
  ),
  ts_xgboost = list(
    label = "XGBoost Regression", family = "ml", needs_scaling = FALSE,
    defaults = list(n_estimators = 200L, max_depth = 3L, learning_rate = 0.05),
    python_only = TRUE, is_regression = TRUE
  ),
  ts_svr = list(
    label = "Support Vector Regression", family = "ml", needs_scaling = TRUE,
    defaults = list(kernel = "rbf", C = 1.0, epsilon = 0.1),
    python_only = TRUE, is_regression = TRUE
  ),
  ts_lstm = list(
    label = "LSTM (TensorFlow)", family = "deep_learning", needs_scaling = TRUE,
    defaults = list(sequence_length = 20L, units = 32L, epochs = 20L, batch_size = 32L, dropout = 0.2),
    python_only = TRUE, is_regression = TRUE
  ),
  ts_gru = list(
    label = "GRU", family = "deep_learning", needs_scaling = TRUE,
    defaults = list(sequence_length = 20L, units = 32L, epochs = 20L, batch_size = 32L, dropout = 0.2),
    python_only = TRUE, is_regression = TRUE
  )
)

MODEL_KEYS <- c(names(ML_MODELS))

#' Resolve + validate a model key against the registry.
#'
#' @param model Raw model identifier from a client.
#' @return The canonical model key.
#' @throws validation_error for unknown models.
resolve_model_key <- function(model) {
  if (is.null(model) || length(model) == 0 || !nzchar(trimws(as.character(model)))) {
    stop(validation_error("model is required."))
  }
  key <- tolower(trimws(as.character(model)))
  key <- gsub("[^a-z0-9_]", "_", key)
  key <- gsub("_+", "_", key)
  key <- sub("^_", "", key); key <- sub("_$", "", key)
  if (!(key %in% MODEL_KEYS)) {
    stop(validation_error(sprintf(
      "Unknown model '%s'. Supported models: %s.",
      model, paste(MODEL_KEYS, collapse = ", ")
    )))
  }
  key
}

#' Merge client hyperparameters over the registry defaults (validated keys).
#'
#' @param key Canonical model key.
#' @param hyperparams Raw client list (NULL allowed).
#' @return list of merged hyperparameters.
#' @throws validation_error for unknown hyperparameter names.
resolve_hyperparams <- function(key, hyperparams) {
  defaults <- ML_MODELS[[key]]$defaults
  out <- defaults
  if (!is.null(hyperparams)) {
    if (!is.list(hyperparams)) {
      stop(validation_error("hyperparameters must be a JSON object."))
    }
    bad <- setdiff(names(hyperparams), names(defaults))
    if (length(bad) > 0) {
      stop(validation_error(sprintf(
        "Unknown hyperparameter(s) for %s: %s. Accepted: %s.",
        key, paste(bad, collapse = ", "), paste(names(defaults), collapse = ", ")
      )))
    }
    for (nm in names(hyperparams)) out[[nm]] <- hyperparams[[nm]]
  }
  out
}

#' Map numeric class labels (1/2/3) to BUY/HOLD/SELL (package conventions).
CLASS_LEVELS <- c("BUY", "HOLD", "SELL")
class_to_integer <- function(y) match(y, CLASS_LEVELS)
integer_to_class <- function(i) CLASS_LEVELS[pmin(pmax(as.integer(i), 1), 3)]

#' Ensure a probability matrix is 3-column [BUY, HOLD, SELL], rows sum to 1.
#'
#' Some engines return columns in odd orders or drop a degenerate class;
#' everything is normalized into the standard contract here.
#'
#' @param P Raw matrix/list of probabilities (n x 3 expected).
#' @return Numeric matrix (n x 3) with colnames CLASS_LEVELS, non-negative,
#'   row-normalized (uniform 1/3 fallback for degenerate rows).
normalize_probabilities <- function(P) {
  P <- as.matrix(P)
  if (nrow(P) == 0) {
    return(matrix(numeric(0), nrow = 0, ncol = 3, dimnames = list(NULL, CLASS_LEVELS)))
  }
  if (ncol(P) != 3) {
    stop("probability matrix must have exactly 3 columns (BUY, HOLD, SELL).")
  }
  P <- pmax(P, 0)
  P[!is.finite(P)] <- 1 / 3
  rs <- rowSums(P)
  rs[rs == 0] <- 1
  P <- P / rs
  colnames(P) <- CLASS_LEVELS
  P
}

# ---------------------------------------------------------------------------
# Per-model train/predict implementations
# ---------------------------------------------------------------------------
# Each trainer returns list(model = <handle>, predict = function(X) P).
# X is the preprocessed (and scaled when required) feature matrix.

.train_predict_factory <- function(train_fn, predict_fn, needs_factors = FALSE) {
  function(X_train, y_train, hyperparams) {
    y_int <- class_to_integer(y_train)
    ok <- !is.na(y_int)
    fit <- train_fn(X_train[ok, , drop = FALSE], y_int[ok], hyperparams)
    list(
      model = fit,
      predict = function(X) predict_fn(fit, X)
    )
  }
}

.train_tabular <- function(key, X_train, y_train, hyperparams) {
  hp <- hyperparams
  # Labels arrive as character BUY/HOLD/SELL; every trainer below expects the
  # integer coding (1=BUY, 2=HOLD, 3=SELL). Convert ONCE here and drop any
  # unlabelled rows defensively so model frames never contain NA responses.
  y_int <- class_to_integer(y_train)
  ok <- !is.na(y_int)
  if (any(!ok)) {
    X_train <- X_train[ok, , drop = FALSE]
    y_int <- y_int[ok]
  }
  if (length(y_int) < 3 || length(unique(y_int)) < 2) {
    stop(sprintf(
      "Model '%s' needs training data covering at least 2 of the BUY/HOLD/SELL classes.",
      key
    ))
  }
  needs_scaling <- isTRUE(ML_MODELS[[key]]$needs_scaling)

  if (needs_scaling) {
    scaler <- fit_scaler(X_train)
    Xs <- apply_scaler(X_train, scaler)
  } else {
    scaler <- NULL
    Xs <- X_train
  }

  handle <- switch(key,
    logistic_regression = .fit_multinom_logistic(Xs, y_int, hp),
    lda = .fit_lda(Xs, y_int),
    qda = .fit_qda(Xs, y_int),
    knn = .fit_knn(Xs, y_int, hp),
    naive_bayes = .fit_naive_bayes(Xs, y_int, hp),
    decision_tree = .fit_decision_tree(Xs, y_int, hp),
    random_forest = .fit_random_forest(Xs, y_int, hp),
    svm = .fit_svm(Xs, y_int, hp),
    gradient_boosting = .fit_gbm(Xs, y_int, hp),
    xgboost = .fit_xgboost(Xs, y_int, hp),
    mlp = .fit_mlp(Xs, y_int, hp),
    stop(sprintf("No tabular trainer registered for '%s'.", key))
  )

  predict_raw <- handle$predict
  list(
    model = handle$model,
    scaler = scaler,
    feature_names = colnames(X_train),
    needs_scaling = needs_scaling,
    predict = function(X_new) {
      Xn <- if (needs_scaling) apply_scaler(X_new, scaler) else X_new
      normalize_probabilities(predict_raw(Xn))
    }
  )
}

# --- Classical ---------------------------------------------------------------

.fit_multinom_logistic <- function(X, y, hp) {
  df <- as.data.frame(X); names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(y, levels = 1:3)
  form <- stats::as.formula(paste(".y ~", paste(names(df)[names(df) != ".y"],
                                                collapse = " + ")))
  fit <- nnet::multinom(form, data = df, trace = FALSE, maxit = 200,
                        decay = 0.01)
  list(
    model = fit,
    predict = function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      stats::predict(fit, newdata = dn, type = "probs")
    }
  )
}

.fit_lda <- function(X, y) {
  df <- as.data.frame(X); names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(y, levels = 1:3)
  form <- stats::as.formula(paste(".y ~", paste(names(df)[names(df) != ".y"],
                                                collapse = " + ")))
  fit <- MASS::lda(form, data = df)
  list(
    model = fit,
    predict = function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      stats::predict(fit, newdata = dn)$posterior
    }
  )
}

.fit_qda <- function(X, y) {
  df <- as.data.frame(X); names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(y, levels = 1:3)
  form <- stats::as.formula(paste(".y ~", paste(names(df)[names(df) != ".y"],
                                                collapse = " + ")))
  fit <- MASS::qda(form, data = df)
  list(
    model = fit,
    predict = function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      stats::predict(fit, newdata = dn)$posterior
    }
  )
}

.fit_knn <- function(X, y, hp) {
  k <- max(1L, min(as.integer(hp$k %||% 15), nrow(X)))
  list(
    model = list(X = X, y = y, k = k),
    predict = function(X_new) {
      P <- FNN::knn(X, X_new, factor(y, levels = 1:3), k = k, prob = TRUE)
      # knn returns class + winning-prob attribute; build 3-col probs.
      raw <- attr(P, "prob"); raw[is.na(raw)] <- 1 / 3
      P3 <- matrix(1 / 3, nrow = length(P), ncol = 3)
      lv <- as.integer(as.character(P))
      win <- pmax(raw, 1 / 3)
      P3[cbind(seq_along(lv), lv)] <- win
      P3
    }
  )
}

.fit_naive_bayes <- function(X, y, hp) {
  df <- as.data.frame(X); names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(y, levels = 1:3)
  laplace <- as.numeric(hp$laplace %||% 1)
  if (requireNamespace("naivebayes", quietly = TRUE)) {
    fit <- naivebayes::naive_bayes(.y ~ ., data = df, laplace = laplace)
    predictor <- function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      stats::predict(fit, dn, type = "prob")
    }
  } else {
    # Fallback: e1071::naiveBayes (dependency of e1071/SVM stack).
    fit <- e1071::naiveBayes(.y ~ ., data = df, laplace = laplace)
    predictor <- function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      stats::predict(fit, dn, type = "raw")
    }
  }
  list(model = fit, predict = predictor)
}

# --- Tree / ensemble ----------------------------------------------------------

.fit_decision_tree <- function(X, y, hp) {
  df <- as.data.frame(X); names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(y, levels = 1:3)
  form <- stats::as.formula(paste(".y ~", paste(names(df)[names(df) != ".y"],
                                                collapse = " + ")))
  fit <- rpart::rpart(form, data = df, method = "class",
                      control = rpart::rpart.control(
                        maxdepth = as.integer(hp$maxdepth %||% 5),
                        cp = as.numeric(hp$cp %||% 0.01),
                        minsplit = as.integer(hp$minsplit %||% 20)
                      ))
  list(
    model = fit,
    predict = function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      stats::predict(fit, dn, type = "prob")
    }
  )
}

.fit_random_forest <- function(X, y, hp) {
  df <- as.data.frame(X); names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(y, levels = 1:3)
  form <- stats::as.formula(paste(".y ~", paste(names(df)[names(df) != ".y"],
                                                collapse = " + ")))
  ntree <- as.integer(hp$ntree %||% 300)
  nodesize <- as.integer(hp$nodesize %||% 5)
  
  class_tab <- table(factor(y, levels = 1:3))
  classwt <- ifelse(class_tab > 0, sum(class_tab) / (3 * class_tab), 1)
  
  rf_args <- list(formula = form, data = df, ntree = ntree, nodesize = nodesize,
                  importance = TRUE, classwt = classwt)
  if (!is.null(hp$mtry)) rf_args$mtry <- as.integer(hp$mtry)
  fit <- do.call(randomForest::randomForest, rf_args)
  importance <- sort(randomForest::importance(fit)[, "MeanDecreaseGini"],
                     decreasing = TRUE)
  attr(fit, "af_importance") <- importance
  list(
    model = fit,
    predict = function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      stats::predict(fit, dn, type = "prob")
    }
  )
}

.fit_svm <- function(X, y, hp) {
  df <- as.data.frame(X); names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(y, levels = 1:3)
  form <- stats::as.formula(paste(".y ~", paste(names(df)[names(df) != ".y"],
                                                collapse = " + ")))
  fit <- e1071::svm(form, data = df, type = "C-classification",
                    kernel = match.arg(hp$kernel %||% "radial",
                                       c("linear", "polynomial", "radial", "sigmoid")),
                    cost = as.numeric(hp$cost %||% 1),
                    gamma = as.numeric(hp$gamma %||% 0.1),
                    probability = TRUE)
  list(
    model = fit,
    predict = function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      p <- stats::predict(fit, dn, probability = TRUE)
      attrs <- attr(p, "probabilities")
      # e1071 column order is factor-level order (1=BUY, 2=HOLD, 3=SELL).
      attrs
    }
  )
}

.fit_gbm <- function(X, y, hp) {
  df <- as.data.frame(X); names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(y, levels = 1:3)
  n.trees <- as.integer(hp$n.trees %||% 200)
  depth <- as.integer(hp$interaction.depth %||% 3)
  shrinkage <- as.numeric(hp$shrinkage %||% 0.05)
  minobs <- as.integer(hp$n.minobsinnode %||% 10)
  fit <- gbm::gbm(.y ~ ., data = df, distribution = "multinomial",
                  n.trees = n.trees, interaction.depth = depth,
                  shrinkage = shrinkage, n.minobsinnode = minobs,
                  verbose = FALSE)
  feature_importance <- gbm::relative.influence(fit, n.trees = n.trees)
  names(feature_importance) <- names(df)[names(df) != ".y"]
  feature_importance <- sort(feature_importance, decreasing = TRUE)
  attr(fit, "af_importance") <- feature_importance
  list(
    model = fit,
    predict = function(X_new) {
      dn <- as.data.frame(X_new); names(dn) <- names(df)[names(df) != ".y"]
      p <- stats::predict(fit, dn, n.trees = n.trees, type = "response")
      # gbm returns (n x classes x 1); squeeze the third dim.
      if (length(dim(p)) == 3) p <- p[, , 1]
      p
    }
  )
}

.fit_xgboost <- function(X, y, hp) {
  y_int <- as.integer(y) - 1L
  
  # Class weights for imbalanced data
  class_counts <- table(factor(y_int, levels = 0:2))
  class_weights <- ifelse(class_counts > 0, sum(class_counts) / (3 * class_counts), 1)
  sample_weights <- as.numeric(class_weights[y_int + 1])  # +1 because y_int is 0-based, R is 1-based
  
  params <- list(
    objective = "multi:softprob",
    num_class = 3,
    max_depth = as.integer(hp$max_depth %||% 3),
    eta = as.numeric(hp$learning_rate %||% 0.05),
    subsample = as.numeric(hp$subsample %||% 0.8),
    colsample_bytree = as.numeric(hp$colsample_bytree %||% 0.8),
    eval_metric = "mlogloss"
  )
  nrounds <- as.integer(hp$n_estimators %||% 200)
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X), label = y_int,
                                 weight = sample_weights,
                                 feature_names = colnames(X))
  fit <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds,
                            verbose = 0)
  feature_importance <- xgboost::xgb.importance(model = fit)
  imp_vec <- if (!is.null(feature_importance) && nrow(feature_importance) > 0) {
    v <- setNames(feature_importance$Gain, feature_importance$Feature)
    v[colnames(X)]
  } else setNames(rep(0, ncol(X)), colnames(X))
  imp_vec[is.na(imp_vec)] <- 0
  attr(fit, "af_importance") <- sort(imp_vec, decreasing = TRUE)
  attr(fit, "af_feature_names") <- colnames(X)
  list(
    model = fit,
    predict = function(X_new) {
      dtest <- xgboost::xgb.DMatrix(data = as.matrix(X_new))
      p <- stats::predict(fit, dtest)
      matrix(p, nrow = length(p) / 3, ncol = 3, byrow = TRUE)
    }
  )
}

# --- Neural (MLP) ---------------------------------------------------------------

.fit_mlp <- function(X, y, hp) {
  # nnet::nnet single-hidden-layer softmax with weight decay + early stop on
  # MaxNWts. size may be a scalar (single layer) or vector (>=2 layers ->
  # mirror into multiple nnet units via a small ensemble of single-layer
  # nets with different hidden sizes; keeps dependencies std-library only).
  sizes <- hp$size %||% c(32, 16)
  if (!is.numeric(sizes)) sizes <- as.numeric(sizes)
  sizes <- sizes[is.finite(sizes) & sizes >= 1]
  if (length(sizes) == 0) sizes <- 16
  decay <- as.numeric(hp$lambda %||% 0.01)
  maxit <- as.integer(hp$maxit %||% 300)
  threshold <- as.numeric(hp$threshold %||% 0.01)

  # One-hot targets for nnet.
  Y <- matrix(0, nrow = length(y), ncol = 3)
  Y[cbind(seq_along(y), as.integer(y))] <- 1

  n_feats <- ncol(X)
  n_wts <- function(s) s * (n_feats + 3) + 3 + s + 3

  train_one <- function(s) {
    nnet::nnet(x = X, y = Y, size = as.integer(s), decay = decay,
               maxit = maxit, trace = FALSE, MaxNWts = max(5000, n_wts(s) * 4),
               abstain = FALSE, softmax = TRUE)
  }

  if (length(sizes) == 1) {
    fit <- train_one(sizes[[1]])
    nets <- list(fit)
  } else {
    nets <- lapply(sizes, train_one)
  }

  feature_importance <- setNames(rep(NA_real_, n_feats), colnames(X))
  attr(nets[[1]], "af_importance_note") <-
    "MLP importance is not directly decomposable; see permutation importance"

  list(
    model = nets,
    predict = function(X_new) {
      Ps <- lapply(nets, function(net) stats::predict(net, X_new, type = "raw"))
      P <- Reduce(`+`, Ps) / length(Ps)
      P
    }
  )
}

# ---------------------------------------------------------------------------
# Registry-facing train/predict entry points
# ---------------------------------------------------------------------------

#' Train a tabular model (all models except LSTM).
#'
#' @param key Canonical model key (resolve_model_key()).
#' @param X_train Preprocessed training matrix.
#' @param y_train Character class vector (BUY/HOLD/SELL).
#' @param hyperparams Merged hyperparameter list (resolve_hyperparams()).
#' @return Model handle: list(model, scaler, feature_names, needs_scaling,
#'   predict = function(X_new) -> n x 3 probability matrix).
#' @throws error with a client-safe message when the underlying package fails.
train_tabular_model <- function(key, X_train, y_train, hyperparams) {
  tryCatch(
    .train_tabular(key, X_train, y_train, hyperparams),
    error = function(e) {
      stop(sprintf("Model '%s' failed to train: %s", key, conditionMessage(e)))
    }
  )
}

#' Predict class probabilities from a tabular model handle.
#'
#' @param handle A train_tabular_model() handle.
#' @param X_new Preprocessed (and, if the model needs it, scaled by the
#'   handle's own predict closure) feature matrix.
#' @return n x 3 probability matrix with colnames BUY/HOLD/SELL.
predict_tabular_model <- function(handle, X_new) {
  handle$predict(X_new)
}

#' Extract the model-based feature-importance vector from a fitted handle.
#'
#' Returns NULL when the model does not expose importance (e.g. MLP/LDA).
#'
#' @param handle A train_tabular_model() handle.
#' @return Named descending numeric vector or NULL.
tabular_feature_importance <- function(handle) {
  fit <- handle$model
  imp <- attr(fit, "af_importance", exact = TRUE)
  if (is.null(imp) && is.list(fit) && length(fit) > 0) {
    imp <- attr(fit[[1]], "af_importance", exact = TRUE)
  }
  if (is.null(imp)) return(NULL)
  sort(imp[!is.na(imp)], decreasing = TRUE)
}
