# AlphaForge Insight — ML preprocessing + chronological splitting (Phase A)
#
# Feature-selection / preprocessing stage shared by training and inference:
#   1. Drop features with excessive missing values (train-fit thresholds).
#   2. Drop constant / near-zero-variance features (nearZeroVar-style).
#   3. Drop highly correlated redundant features (cor + findCorrelation-
#      style greedy removal).
#   4. Optional scaling: z-score standardization with TRAIN-FIT means/sds
#      that are re-applied at inference (never re-fit on test data).
#   5. Optional model-based feature importance ranking (randomForest).
#
# Splitting: strictly chronological TRAIN -> VALIDATION -> TEST by fractions
# of the available rows. No shuffling, ever — random splits leak future
# information into training (ML plan sections 6, 18 engineering rule 11).

library(xts)
suppressPackageStartupMessages(library(TTR))

ML_SPLIT_DEFAULTS <- list(train = 0.6, validation = 0.2, test = 0.2)
ML_MAX_MISSING_FRAC <- 0.05   # drop features missing on >5% of training rows
ML_COR_THRESHOLD <- 0.95      # greedy correlation cutoff

#' Validate chronological split fractions.
#'
#' @param train,validation,test Fractions summing to 1 (tolerance 1e-6),
#'   each in (0, 1).
#' @return Named numeric vector c(train, validation, test).
#' @throws validation_error on violation.
validate_split_fractions <- function(train = ML_SPLIT_DEFAULTS[["train"]],
                                     validation = ML_SPLIT_DEFAULTS[["validation"]],
                                     test = ML_SPLIT_DEFAULTS[["test"]]) {
  v <- suppressWarnings(as.numeric(c(train, validation, test)))
  if (anyNA(v) || any(!is.finite(v)) || any(v <= 0) || any(v >= 1)) {
    stop(validation_error(
      "split fractions must be three numbers strictly between 0 and 1."
    ))
  }
  if (abs(sum(v) - 1) > 1e-6) {
    stop(validation_error(
      "split fractions train + validation + test must sum to 1."
    ))
  }
  list(train = v[[1]], validation = v[[2]], test = v[[3]])
}

#' Chronological train/validation/test index split with optional embargo.
#'
#' Rows are NEVER shuffled. TRAIN takes the OLDEST rows, TEST the newest
#' (ML plan section 6). At least 1 row is guaranteed per split when the input
#' has >= 3 rows; smaller inputs raise a validation error.
#'
#' When labels use a multi-day horizon (e.g., 5 days), the last `embargo` rows
#' of each preceding split are removed to prevent label-horizon overlap between
#' splits (leakage prevention). Embargo=0 produces the original behavior.
#'
#' @param n Total number of ordered rows.
#' @param fractions Named list from validate_split_fractions().
#' @param embargo Integer number of rows to exclude from the end of each split
#'   to prevent label-horizon overlap (default 0).
#' @return list(train, validation, test) of integer index vectors.
chronological_split <- function(n, fractions = ML_SPLIT_DEFAULTS, embargo = 0L) {
  embargo <- max(0L, as.integer(embargo))
  fr <- validate_split_fractions(fractions$train, fractions$validation, fractions$test)
  min_rows <- 10L + 2L * embargo
  if (n < min_rows) {
    stop(validation_error(
      sprintf("At least %d usable rows are required for a train/validation/test split (embargo=%d).",
              min_rows, embargo)
    ))
  }
  n_train <- max(1L, floor(n * fr$train))
  n_val <- max(1L, floor(n * fr$validation))
  n_test <- max(1L, n - n_train - n_val)
  if (n_train + n_val + n_test > n) n_train <- n - n_val - n_test
  
  idx <- seq_len(n)
  train_end <- n_train - embargo
  val_start <- n_train + 1
  val_end <- n_train + n_val - embargo
  test_start <- n_train + n_val + 1
  
  if (train_end < 1L) train_end <- 1L
  if (val_end < val_start) val_end <- val_start
  
  list(
    train = idx[seq_len(train_end)],
    validation = idx[val_start:val_end],
    test = idx[test_start:n]
  )
}

#' Fit the preprocessing specification on TRAINING data only.
#'
#' Steps (all fit on X_train; applied verbatim to validation/test later):
#'   - drop features whose training-row missing fraction exceeds max_missing
#'   - drop constant and near-zero-variance features (nearZeroVar-style:
#'     sd == 0, or sd/|mean| < 1e-4, or < 2 distinct values)
#'   - greedily drop one of any pair with |cor| > cor_threshold (findCorrelation
#'     style: the feature with the larger mean |correlation| to the others is
#'     removed first)
#'
#' @param X_train Numeric matrix of training features (rows = samples).
#' @param max_missing Max allowed fraction of NA per feature (default 0.05).
#' @param cor_threshold Correlation cutoff for redundancy removal (default
#'   0.95; pass NULL to disable).
#' @return A spec list:
#'   kept, dropped_missing, dropped_zero_var, dropped_correlated, medians (for
#'   residual NA imputation at inference).
fit_preprocessing <- function(X_train, max_missing = ML_MAX_MISSING_FRAC,
                              cor_threshold = ML_COR_THRESHOLD) {
  if (!is.matrix(X_train) || nrow(X_train) < 2L) {
    stop(validation_error(
      "preprocessing requires a numeric matrix with at least 2 training rows."
    ))
  }
  feat_names <- colnames(X_train)
  if (is.null(feat_names)) feat_names <- paste0("f", seq_len(ncol(X_train)))

  miss_frac <- colMeans(is.na(X_train))
  drop_missing <- feat_names[miss_frac > max_missing]

  keep1 <- setdiff(feat_names, drop_missing)
  M1 <- X_train[, keep1, drop = FALSE]

  sd_v <- apply(M1, 2, function(x) stats::sd(x, na.rm = TRUE))
  mean_v <- colMeans(M1, na.rm = TRUE)
  distinct <- vapply(seq_len(ncol(M1)), function(j) {
    length(unique(na.omit(M1[, j])))
  }, integer(1))
  zero_var <- is.na(sd_v) | sd_v == 0 |
    (abs(mean_v) > 0 & (sd_v / abs(mean_v)) < 1e-4) | distinct < 2
  drop_zero <- keep1[zero_var]

  keep2 <- setdiff(keep1, drop_zero)
  M2 <- X_train[, keep2, drop = FALSE]

  drop_cor <- character(0)
  if (!is.null(cor_threshold) && ncol(M2) >= 2) {
    cm <- stats::cor(M2, use = "pairwise.complete.obs")
    cm[is.na(cm)] <- 0
    # Hard iteration cap: at most one feature can be dropped per column.
    for (iter in seq_len(ncol(cm))) {
      abs_cm <- abs(cm)
      diag(abs_cm) <- 0
      if (ncol(abs_cm) == 0 || max(abs_cm) < cor_threshold) break
      # Highest-mean-correlation feature in the first offending pair is removed.
      pair <- which(abs_cm >= cor_threshold, arr.ind = TRUE)[1, , drop = FALSE]
      # IMPORTANT: index the CURRENT cm dimnames (they shrink every iteration;
      # indexing the original M2 names would misalign and never terminate).
      cand <- unique(colnames(cm)[pair])
      mean_cor <- rowMeans(abs_cm)
      loser <- cand[which.max(mean_cor[cand])]
      if (length(loser) != 1 || is.na(loser) || !nzchar(loser)) break
      drop_cor <- c(drop_cor, loser)
      cm <- cm[setdiff(rownames(cm), loser), setdiff(colnames(cm), loser), drop = FALSE]
    }
  }

  kept <- setdiff(keep2, drop_cor)
  if (length(kept) == 0) {
    stop(validation_error(
      "All features were removed by preprocessing (missing / zero-variance / correlated)."
    ))
  }

  # Median imputation for residual isolated NAs, fit on training medians only.
  medians <- vapply(X_train[, kept, drop = FALSE], function(x) {
    stats::median(x, na.rm = TRUE)
  }, numeric(1))
  medians[is.na(medians)] <- 0

  list(
    kept = kept,
    dropped_missing = drop_missing,
    dropped_zero_var = drop_zero,
    dropped_correlated = drop_cor,
    medians = medians,
    max_missing = max_missing,
    cor_threshold = cor_threshold
  )
}

#' Apply a fitted preprocessing spec to any feature matrix.
#'
#' Orders columns to the spec's `kept` list (missing columns become an error —
#' training/inference skew), imputes residual NAs with training medians.
#'
#' @param X Numeric matrix of raw features.
#' @param spec A fit_preprocessing() spec.
#' @return Imputed matrix with exactly the spec's kept columns.
apply_preprocessing <- function(X, spec) {
  if (!is.matrix(X)) stop("apply_preprocessing requires a matrix.")
  missing_cols <- setdiff(spec$kept, colnames(X))
  if (length(missing_cols) > 0) {
    stop(validation_error(sprintf(
      "Feature columns missing at inference: %s.",
      paste(missing_cols, collapse = ", ")
    )))
  }
  out <- X[, spec$kept, drop = FALSE]
  for (j in seq_along(spec$kept)) {
    nas <- is.na(out[, j])
    if (any(nas)) out[nas, j] <- spec$medians[[j]]
  }
  # Non-finite guard (division blowups etc.) -> median.
  for (j in seq_along(spec$kept)) {
    bad <- !is.finite(out[, j])
    if (any(bad)) out[bad, j] <- spec$medians[[j]]
  }
  out
}

#' Fit a z-score scaler on training data (for scale-sensitive models).
#'
#' @param X_train Numeric matrix (already preprocessed/imputed).
#' @return list(center = col means, scale = col sds; zero sds pinned to 1).
fit_scaler <- function(X_train) {
  center <- colMeans(X_train)
  scale_v <- apply(X_train, 2, stats::sd)
  scale_v[is.na(scale_v) | scale_v == 0] <- 1
  list(center = center, scale = scale_v)
}

#' Apply a fitted scaler.
#' @param X Numeric matrix. @param scaler fit_scaler() output.
#' @return Standardized matrix.
apply_scaler <- function(X, scaler) {
  sweep(sweep(X, 2, scaler$center, `-`), 2, scaler$scale, `/`)
}

#' Model-based feature importance (randomForest) on training data.
#'
#' Optional stage for the comparison report. Returns a ranked named vector of
#' permutation importance, or NULL when randomForest is unavailable.
#'
#' @param X_train Preprocessed training matrix.
#' @param y_train Character class vector (BUY/HOLD/SELL).
#' @param ntree Forest size for the importance pass (default 300).
#' @return Named numeric vector (descending) or NULL.
feature_importance_model <- function(X_train, y_train, ntree = 300L) {
  if (!requireNamespace("randomForest", quietly = TRUE)) return(NULL)
  df <- as.data.frame(X_train)
  df$.class <- factor(y_train, levels = VALID_CLASSES)
  rf <- tryCatch(
    randomForest::randomForest(.class ~ ., data = df, ntree = ntree, importance = TRUE),
    error = function(e) NULL
  )
  if (is.null(rf)) return(NULL)
  imp <- randomForest::importance(rf)[, "MeanDecreaseAccuracy", drop = TRUE]
  sort(imp[!is.na(imp)], decreasing = TRUE)
}

#' Generate expanding-window walk-forward splits with embargo.
#'
#' Divides the last portion of data into n_folds test windows. For each fold,
#' trains on everything before (minus embargo) and tests on the window.
#' This provides robust out-of-sample evaluation for time-series models.
#'
#' @param n Total rows.
#' @param n_folds Number of evaluation windows (default 5).
#' @param test_frac Fraction of data for each test window (default 0.1).
#' @param embargo Label horizon embargo (default 5).
#' @return List of lists, each with train and test integer index vectors.
walk_forward_splits <- function(n, n_folds = 5L, test_frac = 0.1, embargo = 5L) {
  n_folds <- max(1L, as.integer(n_folds))
  test_frac <- max(0.01, min(0.5, as.numeric(test_frac)))
  embargo <- max(0L, as.integer(embargo))
  
  if (n < 20L) {
    stop(validation_error(
      "Walk-forward validation requires at least 20 rows."
    ))
  }
  
  test_size <- max(1L, floor(n * test_frac))
  total_test <- test_size * n_folds
  
  if (total_test >= n) {
    stop(validation_error(
      sprintf("n_folds * test_frac too large for %d rows.", n)
    ))
  }
  
  splits <- list()
  for (i in seq_len(n_folds)) {
    test_start <- n - (n_folds - i) * test_size
    test_end <- test_start + test_size - 1L
    train_end <- test_start - 1L - embargo
    
    if (train_end < 1L) {
      stop(validation_error(
        "Insufficient training rows with current embargo setting."
      ))
    }
    
    splits[[i]] <- list(
      train = seq_len(train_end),
      test = test_start:min(test_end, n)
    )
  }
  
  splits
}
