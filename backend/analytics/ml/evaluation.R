# AlphaForge Insight — ML evaluation metrics (ML Phase E)
#
# Classification metrics computed from standardized prediction records:
#   accuracy, per-class precision/recall/F1 (macro + BUY-focused), confusion
#   matrix, and one-vs-rest ROC-AUC / PR-AUC per class.
#
# Metrics are computed against the TEST (or validation) labels only, never on
# training data. A model can score high accuracy and still be an unprofitable
# strategy — the backtest layer (existing engine) answers that separately.

library(xts)
suppressPackageStartupMessages(library(TTR))

#' Confusion matrix for BUY/HOLD/SELL predictions.
#'
#' @param predicted Character vector of predicted classes.
#' @param actual Character vector of true classes (same length).
#' @return 3x3 integer matrix, rows = predicted, cols = actual, ordered
#'   BUY/HOLD/SELL, with dimnames.
confusion_matrix <- function(predicted, actual) {
  predicted <- as.character(predicted)
  actual <- as.character(actual)
  if (length(predicted) != length(actual)) {
    stop("predicted and actual must have the same length.")
  }
  tab <- table(
    factor(predicted, levels = VALID_CLASSES),
    factor(actual, levels = VALID_CLASSES)
  )
  out <- matrix(as.integer(tab), nrow = 3, ncol = 3, dimnames = list(
    Predicted = VALID_CLASSES, Actual = VALID_CLASSES
  ))
  out
}

#' Per-class precision/recall/F1 (+ macro averages).
#'
#' @param cm A confusion_matrix() output.
#' @return list with per-class lists and macro_precision/recall/f1.
classification_prf <- function(cm) {
  per_class <- lapply(VALID_CLASSES, function(cls) {
    tp <- cm[cls, cls]
    fp <- sum(cm[cls, ]) - tp
    fn <- sum(cm[, cls]) - tp
    precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
    recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
    f1 <- if (is.na(precision) || is.na(recall) ||
              (precision + recall) == 0) NA_real_
           else 2 * precision * recall / (precision + recall)
    list(precision = precision, recall = recall, f1 = f1,
         support = sum(cm[, cls]))
  })
  names(per_class) <- VALID_CLASSES
  avg <- function(key) {
    vals <- vapply(per_class, function(x) x[[key]], numeric(1))
    mean(vals[!is.na(vals)])
  }
  list(per_class = per_class,
       macro_precision = avg("precision"),
       macro_recall = avg("recall"),
       macro_f1 = avg("f1"))
}

#' Classification accuracy.
#' @param predicted,actual Character class vectors.
#' @return Fraction correct; NA when no observations.
classification_accuracy <- function(predicted, actual) {
  if (length(predicted) == 0) return(NA_real_)
  mean(as.character(predicted) == as.character(actual))
}

#' Trinary ROC/PR curve points + AUC for one class (one-vs-rest).
#'
#' Curves are computed by sweeping the score threshold; AUC uses the
#' trapezoidal rule. Returns empty curves (NULL AUC) when the class has no
#' positives/negatives — undefined, never fabricated.
#'
#' @param scores Numeric vector of P(class).
#' @param actual Character class vector.
#' @param cls Class label ("BUY", "HOLD" or "SELL").
#' @return list(roc = list(fpr, tpr, auc), pr = list(recall, precision, auc)).
class_curve_auc <- function(scores, actual, cls) {
  pos <- as.character(actual) == cls
  if (!any(pos) || all(pos)) {
    return(list(roc = list(fpr = numeric(0), tpr = numeric(0), auc = NA_real_),
                pr = list(recall = numeric(0), precision = numeric(0), auc = NA_real_)))
  }
  y <- as.integer(pos)
  ord <- order(scores, decreasing = TRUE)
  s <- scores[ord]; yy <- y[ord]

  # Tie-aware sweep: one threshold per DISTINCT score, PLUS the explicit
  # (0,0) start and (1,1) end points so the curve is anchored at the corners
  # (a threshold below the minimum score classifies everything positive).
  thresholds <- unique(s)
  n_pos <- sum(yy); n_neg <- length(yy) - n_pos
  fpr <- numeric(length(thresholds) + 2); tpr <- numeric(length(thresholds) + 2)
  prec <- numeric(length(thresholds) + 2); rec <- tpr
  fpr[1] <- 0; tpr[1] <- 0          # (0,0): nothing predicted positive
  prec[1] <- 1; rec[1] <- 0
  fpr[2] <- 1; tpr[2] <- 0          # (1,0): everything predicted positive
  prec[2] <- n_pos / length(yy); rec[2] <- 0
  for (k in seq_along(thresholds)) {
    pred <- s >= thresholds[k]
    tp <- sum(yy[pred] == 1); fp <- sum(yy[pred] == 0)
    tpr[k + 2] <- tp / n_pos
    fpr[k + 2] <- fp / n_neg
    rec[k + 2] <- tpr[k + 2]
    prec[k + 2] <- if (tp + fp > 0) tp / (tp + fp) else n_pos / length(yy)
  }
  # Sort by fpr for the trapezoidal integration (the sweep walks fpr
  # downwards; points may repeat when scores tie).
  ord2 <- order(fpr, tpr)
  fpr_s <- fpr[ord2]; tpr_s <- tpr[ord2]
  # Collapse duplicate fpr keeping the max tpr (standard ROC convention).
  keep <- c(diff(fpr_s) > 0, TRUE)
  agg <- tapply(tpr_s, fpr_s, max)
  fpr_u <- as.numeric(names(agg)); tpr_u <- as.numeric(agg)

  trap <- function(x, yv) {
    if (length(x) < 2) return(NA_real_)
    sum(diff(x) * (head(yv, -1) + tail(yv, -1)) / 2)
  }
  # EXACT ROC-AUC via the Mann-Whitney U statistic (ties handled with
  # midranks). The trapezoid rule over a coarse threshold sweep cuts corners
  # on tied scores; the rank statistic is exact for the same curve.
  ranks <- rank(s, ties.method = "average")
  roc_auc <- (sum(ranks[yy == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  # PR-AUC via average precision on the recall grid (step interpolation).
  pr_auc <- NA_real_
  if (length(rec) >= 2) {
    # prepend (0, 1) anchor for average precision.
    rec2 <- c(0, rec); prec2 <- c(1, prec)
    pr_auc <- trap(rec2, prec2)
  }

  list(
    roc = list(fpr = fpr, tpr = tpr, auc = roc_auc),
    pr = list(recall = rec, precision = prec, auc = pr_auc)
  )
}

#' Full classification evaluation for one model's predictions.
#'
#' @param predictions data.frame of standardized prediction records
#'   (predicted_class, prob_buy, prob_hold, prob_sell) aligned 1:1 with
#'   `actual` (character BUY/HOLD/SELL).
#' @param actual Character vector of true labels.
#' @return list(accuracy, precision, recall, f1 (macro), per_class,
#'   confusion_matrix, roc_auc (per class + macro), pr_auc (per class +
#'   macro), curves (per class) for UI charts, n_eval).
evaluate_classification <- function(predictions, actual) {
  predicted <- as.character(predictions$predicted_class)
  actual <- as.character(actual)
  if (length(predicted) != length(actual)) {
    stop("predictions and actual labels must align 1:1.")
  }
  cm <- confusion_matrix(predicted, actual)
  prf <- classification_prf(cm)

  aucs <- list(); pr_aucs <- list(); curves <- list()
  for (cls in VALID_CLASSES) {
    scores <- switch(cls,
      BUY = as.numeric(predictions$prob_buy),
      HOLD = as.numeric(predictions$prob_hold),
      SELL = as.numeric(predictions$prob_sell)
    )
    res <- class_curve_auc(scores, actual, cls)
    aucs[[cls]] <- res$roc$auc
    pr_aucs[[cls]] <- res$pr$auc
    curves[[cls]] <- res
  }
  auc_vals <- unlist(aucs); pr_vals <- unlist(pr_aucs)

  list(
    accuracy = classification_accuracy(predicted, actual),
    precision = prf$macro_precision,
    recall = prf$macro_recall,
    f1 = prf$macro_f1,
    per_class = prf$per_class,
    confusion_matrix = cm,
    roc_auc = auc_vals,
    roc_auc_macro = mean(auc_vals[!is.na(auc_vals)]),
    pr_auc = pr_vals,
    pr_auc_macro = mean(pr_vals[!is.na(pr_vals)]),
    curves = curves,
    n_eval = length(predicted)
  )
}

#' Serialize the evaluation for JSON (matrix -> row lists, NA -> NULL handled
#' by the API layer's na_to_null).
#'
#' @param ev evaluate_classification() output.
#' @param include_curves Include ROC/PR curve point arrays (default TRUE).
#' @return JSON-friendly list.
evaluation_to_list <- function(ev, include_curves = TRUE) {
  cm <- ev$confusion_matrix
  cm_list <- lapply(rownames(cm), function(r) {
    list(predicted = r,
         actual_counts = as.list(setNames(as.integer(cm[r, ]), colnames(cm))))
  })
  out <- list(
    accuracy = ev$accuracy,
    precision = ev$precision,
    recall = ev$recall,
    f1 = ev$f1,
    per_class = ev$per_class,
    roc_auc = as.list(ev$roc_auc),
    roc_auc_macro = ev$roc_auc_macro,
    pr_auc = as.list(ev$pr_auc),
    pr_auc_macro = ev$pr_auc_macro,
    confusion_matrix = cm_list,
    n_eval = ev$n_eval
  )
  if (include_curves) {
    out$curves = lapply(names(ev$curves), function(cls) {
      c <- ev$curves[[cls]]
      list(
        class = cls,
        roc = list(
          fpr = as.list(c$roc$fpr), tpr = as.list(c$roc$tpr), auc = c$roc$auc
        ),
        pr = list(
          recall = as.list(c$pr$recall), precision = as.list(c$pr$precision),
          auc = c$pr$auc
        )
      )
    })
  }
  out
}
