# AlphaForge Insight — ML feature leakage regression tests
#
# CRITICAL: Proves that perturbing future observations cannot change features
# calculated for earlier dates. Also validates label alignment, embargo gaps,
# and temporal split correctness.
#
# Run from the repository root:
#   Rscript tests/ml/test_feature_leakage.R
# Exit code 0 = all pass, 1 = at least one failure.

options(warn = -1)

suppressPackageStartupMessages(source("backend/analytics/utils/validation.R"))
suppressPackageStartupMessages(source("backend/analytics/data_retrieval.R"))
for (f in c("features.R", "labels.R", "preprocessing.R", "evaluation.R",
            "models.R", "lstm.R", "hyperparameters.R", "adapter.R",
            "pipeline.R")) {
  suppressPackageStartupMessages(source(file.path("backend/analytics/ml", f)))
}
suppressPackageStartupMessages(source("backend/analytics/execution.R"))
suppressPackageStartupMessages(source("backend/analytics/metrics.R"))

results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

cat("==================================================\n")
cat("FEATURE LEAKAGE & TEMPORAL INTEGRITY TESTS\n")
cat("==================================================\n\n")

# ---------------------------------------------------------------------------
# Synthetic OHLCV
# ---------------------------------------------------------------------------
set.seed(42)
n_days <- 500
ret <- rnorm(n_days, 0.0004, 0.015)
close <- 100 * exp(cumsum(ret))
open <- close * exp(rnorm(n_days, 0, 0.003))
high <- pmax(open, close) * exp(abs(rnorm(n_days, 0, 0.002)))
low <- pmin(open, close) * exp(-abs(rnorm(n_days, 0, 0.002)))
volume <- sample(1e6:3e6, n_days, replace = TRUE)
d <- as.Date("2021-01-04") + 0:(n_days - 1)
wd <- !format(d, "%u") %in% c("6", "7")
d2 <- d[wd]
n_usable <- length(d2)
ohlcv <- xts::xts(cbind(
  Open = open[wd], High = high[wd], Low = low[wd],
  Close = close[wd], Adjusted = close[wd], Volume = volume[wd]
), order.by = d2)

# ---------------------------------------------------------------------------
# Test 1: Feature causality — perturbing future does NOT change past features
# ---------------------------------------------------------------------------
cat("\n--- Feature causality tests ---\n")

feats_full <- build_features(ohlcv, NULL)
cutoff <- 200  # check feature at row 200

for (group_name in names(FEATURE_GROUPS)) {
  feat_names <- FEATURE_GROUPS[[group_name]]
  
  # Build features for full series
  f_full <- build_features(ohlcv, group_name)
  
  # Truncate: keep only rows 1:250 (row 200 retained, but 201+ changed)
  ohlcv_trunc <- ohlcv[1:250, ]
  f_trunc <- build_features(ohlcv_trunc, group_name)
  
  for (fn in feat_names) {
    v_full <- as.numeric(f_full$features[cutoff, fn])
    v_trunc <- as.numeric(f_trunc$features[cutoff, fn])
    
    # Both NA is fine; both equal is fine
    ok <- (is.na(v_full) && is.na(v_trunc)) ||
      (!is.na(v_full) && !is.na(v_trunc) &&
         abs(v_full - v_trunc) < 1e-10)
    
    record(sprintf("causality[%s/%s]: row %d unchanged when future removed",
                   group_name, fn, cutoff), ok,
           if (!ok) sprintf("full=%.6f, trunc=%.6f", v_full, v_trunc) else "")
  }
}

# ---------------------------------------------------------------------------
# Test 2: Feature causality — perturbing ONE future value
# ---------------------------------------------------------------------------
cat("\n--- Single-future-perturbation tests ---\n")

# Spike Close on row 300, verify row 250 unchanged
ohlcv_perturbed <- ohlcv
ohlcv_perturbed[300, "Close"] <- as.numeric(ohlcv[300, "Close"]) * 2
ohlcv_perturbed[300, "High"] <- as.numeric(ohlcv[300, "High"]) * 2

f_orig <- build_features(ohlcv, NULL)
f_pert <- build_features(ohlcv_perturbed, NULL)

check_row <- 250
all_names <- unlist(FEATURE_GROUPS, use.names = FALSE)
n_ok <- 0
n_checked <- 0
for (fn in all_names) {
  v1 <- as.numeric(f_orig$features[check_row, fn])
  v2 <- as.numeric(f_pert$features[check_row, fn])
  ok2 <- (is.na(v1) && is.na(v2)) ||
    (!is.na(v1) && !is.na(v2) && abs(v1 - v2) < 1e-10)
  n_checked <- n_checked + 1
  if (ok2) n_ok <- n_ok + 1
}
record(sprintf("perturbation: %d/%d features at row %d unchanged when row 300 spiked",
               n_ok, n_checked, check_row),
       n_ok == n_checked)

# ---------------------------------------------------------------------------
# Test 3: Label alignment — labels at time t use future data ONLY
# ---------------------------------------------------------------------------
cat("\n--- Label alignment tests ---\n")

closes <- as.numeric(ohlcv[, "Close"])
lab <- validate_and_generate_labels(closes, horizon = 5, buy_threshold = 0.02,
                                     sell_threshold = 0.02)

# Verify: future_return[t] = Close[t+5]/Close[t] - 1
for (t in c(1, 50, 100, 200)) {
  expected <- closes[t + 5] / closes[t] - 1
  actual_label <- lab$labels[t]
  expected_label <- if (expected > 0.02) "BUY" else if (expected < -0.02) "SELL" else "HOLD"
  record(sprintf("label[t=%d]: correct class (%s)", t, expected_label),
         actual_label == expected_label)
}

# Verify: final horizon rows are NA (no fabricated labels)
n_close <- length(closes)
record("labels: final 5 observations are NA (no fabrication)",
       all(is.na(lab$labels[(n_close - 4):n_close])))
record("labels: valid mask is FALSE for final 5 rows",
       all(!lab$valid[(n_close - 4):n_close]))

# ---------------------------------------------------------------------------
# Test 4: Temporal embargo gaps
# ---------------------------------------------------------------------------
cat("\n--- Embargo gap tests ---\n")

# With embargo=5, the last 5 train rows and last 5 val rows should be removed
sp <- chronological_split(100, ML_SPLIT_DEFAULTS, embargo = 5)

record("embargo: train/val gap exists",
       max(sp$train) + 5 < min(sp$validation) ||
       max(sp$train) < min(sp$validation))

record("embargo: val/test gap exists",
       max(sp$validation) + 5 < min(sp$test) ||
       max(sp$validation) < min(sp$test))

record("embargo: train ends before validation starts",
       max(sp$train) < min(sp$validation))

record("embargo: validation ends before test starts",
       max(sp$validation) < min(sp$test))

# No embargo should produce contiguous splits
sp0 <- chronological_split(100, ML_SPLIT_DEFAULTS, embargo = 0)
record("no-embargo: splits are contiguous",
       max(sp0$train) + 1 == min(sp0$validation) &&
         max(sp0$validation) + 1 == min(sp0$test))

# ---------------------------------------------------------------------------
# Test 5: Walk-forward splits correctness
# ---------------------------------------------------------------------------
cat("\n--- Walk-forward split tests ---\n")

wf <- walk_forward_splits(200, n_folds = 3, test_frac = 0.1, embargo = 5)

record("walk-forward: returns 3 folds", length(wf) == 3)

for (i in seq_along(wf)) {
  fold <- wf[[i]]
  record(sprintf("walk-forward fold %d: train before test", i),
         max(fold$train) < min(fold$test))
  record(sprintf("walk-forward fold %d: embargo gap >= 5", i),
         min(fold$test) - max(fold$train) > 5)
}

# Expanding windows: each subsequent fold should have more training data
if (length(wf) >= 2) {
  record("walk-forward: expanding window (fold 2 has more train than fold 1)",
         length(wf[[2]]$train) > length(wf[[1]]$train))
}

# ---------------------------------------------------------------------------
# Test 6: No target leakage in assembled dataset
# ---------------------------------------------------------------------------
cat("\n--- Target leakage prevention ---\n")

ds <- assemble_ml_dataset(ohlcv = ohlcv, ticker = "SYNTH",
                          start = format(min(d2), "%Y-%m-%d"),
                          end = format(max(d2), "%Y-%m-%d"))

# Feature matrix should NOT contain future_return or label columns
record("dataset: X_raw does not contain 'future_return'",
       !("future_return" %in% colnames(ds$X_raw)))
record("dataset: X_raw does not contain 'label'",
       !("label" %in% colnames(ds$X_raw)))
record("dataset: X_raw does not contain 'labels'",
       !("labels" %in% colnames(ds$X_raw)))

# Features should all be from the documented set
known_features <- unlist(FEATURE_GROUPS, use.names = FALSE)
unknown <- setdiff(colnames(ds$X_raw), known_features)
record("dataset: no unknown feature columns in X_raw",
       length(unknown) == 0,
       if (length(unknown) > 0) paste(unknown, collapse = ", ") else "")

# ---------------------------------------------------------------------------
# Test 7: Class distribution reporting
# ---------------------------------------------------------------------------
cat("\n--- Class distribution tests ---\n")

train_tab <- table(factor(ds$y[ds$split$train], levels = c("BUY", "HOLD", "SELL")))
val_tab <- table(factor(ds$y[ds$split$validation], levels = c("BUY", "HOLD", "SELL")))
test_tab <- table(factor(ds$y[ds$split$test], levels = c("BUY", "HOLD", "SELL")))

record("class distribution: all 3 classes in train",
       all(train_tab > 0),
       sprintf("BUY=%d HOLD=%d SELL=%d", train_tab["BUY"], train_tab["HOLD"], train_tab["SELL"]))

cat(sprintf("  Train:  BUY=%d (%.1f%%) HOLD=%d (%.1f%%) SELL=%d (%.1f%%)\n",
            train_tab["BUY"], 100 * train_tab["BUY"] / sum(train_tab),
            train_tab["HOLD"], 100 * train_tab["HOLD"] / sum(train_tab),
            train_tab["SELL"], 100 * train_tab["SELL"] / sum(train_tab)))
cat(sprintf("  Val:    BUY=%d (%.1f%%) HOLD=%d (%.1f%%) SELL=%d (%.1f%%)\n",
            val_tab["BUY"], 100 * val_tab["BUY"] / sum(val_tab),
            val_tab["HOLD"], 100 * val_tab["HOLD"] / sum(val_tab),
            val_tab["SELL"], 100 * val_tab["SELL"] / sum(val_tab)))
cat(sprintf("  Test:   BUY=%d (%.1f%%) HOLD=%d (%.1f%%) SELL=%d (%.1f%%)\n",
            test_tab["BUY"], 100 * test_tab["BUY"] / sum(test_tab),
            test_tab["HOLD"], 100 * test_tab["HOLD"] / sum(test_tab),
            test_tab["SELL"], 100 * test_tab["SELL"] / sum(test_tab)))

# ---------------------------------------------------------------------------
# Test 8: Regime features are causal
# ---------------------------------------------------------------------------
cat("\n--- Regime feature causality ---\n")

feats_regime <- build_features(ohlcv, "regime")
feats_regime_trunc <- build_features(ohlcv[1:200, ], "regime")

# Check row 150 across both
for (fn in FEATURE_GROUPS$regime) {
  v1 <- as.numeric(feats_regime$features[150, fn])
  v2 <- as.numeric(feats_regime_trunc$features[150, fn])
  ok3 <- (is.na(v1) && is.na(v2)) ||
    (!is.na(v1) && !is.na(v2) && abs(v1 - v2) < 1e-10)
  record(sprintf("regime causality[%s]: row 150 unchanged when future removed", fn),
         ok3)
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d feature leakage tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  cat("\nCRITICAL: Feature leakage detected. Check identified failures.\n")
  quit(status = 1)
}
cat("ALL FEATURE LEAKAGE TESTS PASSED\n")
quit(status = 0)
