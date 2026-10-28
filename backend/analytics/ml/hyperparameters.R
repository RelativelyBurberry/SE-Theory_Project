# AlphaForge Insight — controlled hyperparameter tuning (ML Phase H)
#
# SMALL, documented grids only (ML plan section 11). Tuning scores each
# candidate on the VALIDATION period (never the test period) using macro F1
# with the winner retrained on the full train set by the caller.
#
# Grid sizes: <= 6 candidates per model. The LSTM grid is evaluated with a
# reduced epoch budget at tuning time to keep latency bounded.

library(xts)
suppressPackageStartupMessages(library(TTR))

ML_TUNING_GRIDS <- list(
  random_forest = list(
    ntree    = c(200, 500),
    mtry     = c("sqrt", "third"),   # resolved to sqrt(p) or p/3
    nodesize = c(5, 10)
  ),
  svm = list(
    kernel = c("radial", "linear"),
    cost   = c(0.5, 2),
    gamma  = c(0.05, 0.2)
  ),
  gradient_boosting = list(
    n.trees           = c(150, 300),
    shrinkage         = c(0.03, 0.1),
    interaction.depth = c(2, 3)
  ),
  xgboost = list(
    max_depth        = c(3, 5),
    learning_rate    = c(0.03, 0.1),
    n_estimators     = c(100, 200),
    subsample        = 0.8,
    colsample_bytree = 0.8
  ),
  mlp = list(
    size   = list(c(16), c(32, 16)),
    lambda = c(0.001, 0.01),
    maxit  = 200
  ),
  lstm = list(
    sequence_length = c(10, 20),
    units           = c(16, 32),
    epochs          = 8   # reduced budget at tuning time
  )
)

#' Expand a grid into candidate parameter lists (Cartesian product, capped).
#'
#' @param key Canonical model key with an entry in ML_TUNING_GRIDS.
#' @param limit Maximum number of candidates (default 6).
#' @return List of candidate parameter lists.
#' @return NULL when the model has no grid (tuning not supported).
expand_tuning_grid <- function(key, limit = 6L) {
  grid <- ML_TUNING_GRIDS[[key]]
  if (is.null(grid)) return(NULL)
  fixed <- grid[vapply(grid, function(x) length(x) == 1 && !is.list(x), logical(1))]
  varied <- grid[!names(grid) %in% names(fixed)]
  combos <- expand.grid(varied, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  if (nrow(combos) > limit) combos <- combos[seq_len(limit), , drop = FALSE]
  lapply(seq_len(nrow(combos)), function(i) {
    cand <- as.list(combos[i, , drop = TRUE])
    cand <- lapply(cand, function(x) if (length(x) == 1) x[[1]] else x)
    c(fixed, cand)
  })
}

#' Resolve the "sqrt"/"third" mtry tokens for random forest.
#' @param mtry Raw token or integer. @param p Number of features.
#' @return Integer mtry.
resolve_mtry <- function(mtry, p) {
  if (is.null(mtry)) return(max(1L, floor(sqrt(p))))
  if (is.character(mtry)) {
    if (mtry == "sqrt") return(max(1L, floor(sqrt(p))))
    if (mtry == "third") return(max(1L, floor(p / 3)))
    mtry <- suppressWarnings(as.integer(mtry))
  }
  if (is.na(mtry) || mtry < 1) max(1L, floor(sqrt(p))) else mtry
}

#' Tune a model's hyperparameters on the VALIDATION period.
#'
#' @param ds assemble_ml_dataset() output.
#' @param key Canonical model key.
#' @param limit Max candidates.
#' @return list(best = winning parameter list, summary = data.frame with one
#'   row per candidate: params (JSON-ish string), val_f1, val_accuracy).
tune_hyperparams <- function(ds, key, limit = 6L) {
  candidates <- expand_tuning_grid(key, limit)
  if (is.null(candidates)) {
    return(list(best = list(), summary = NULL))
  }
  inputs <- prepare_model_inputs(ds, key)
  p <- ncol(inputs$X_train)

  rows <- list()
  best_f1 <- -Inf
  best <- list()
  for (i in seq_along(candidates)) {
    hp <- candidates[[i]]
    if (key == "random_forest") hp$mtry <- resolve_mtry(hp$mtry, p)
    score <- tryCatch({
      if (key == "lstm") {
        hp$epochs <- hp$epochs %||% 8
        handle <- train_lstm_model(inputs$X_train, inputs$y_train, hp,
                                   NULL, NULL)
      } else {
        handle <- train_tabular_model(key, inputs$X_train, inputs$y_train, hp)
      }
      P <- handle$predict(inputs$X_val)
      recs <- prediction_records(key, NA_character_, inputs$dates_val, P)
      cls <- evaluate_classification(recs, inputs$y_val)
      c(f1 = cls$f1, acc = cls$accuracy)
    }, error = function(e) c(f1 = NA_real_, acc = NA_real_))

    rows[[i]] <- data.frame(
      candidate = i,
      params = paste(names(hp), unlist(lapply(hp, function(x) {
        if (length(x) > 1) paste0("[", paste(x, collapse = ","), "]") else as.character(x)
      })), sep = "=", collapse = " "),
      val_f1 = score[["f1"]], val_accuracy = score[["acc"]],
      stringsAsFactors = FALSE
    )
    f1 <- score[["f1"]]
    if (!is.na(f1) && f1 > best_f1) {
      best_f1 <- f1
      best <- hp
    }
  }
  summary <- do.call(rbind, rows)
  list(best = best, summary = summary)
}
