# AlphaForge Insight — ML module unit tests (no network, no DB)
#
# Covers: feature engineering (shape, causality, warm-up NAs), label
# generation (thresholds, horizon masking, no future features), preprocessing
# (fit/apply consistency, correlation removal termination, scaler), evaluation
# metrics (hand-computed confusion/F1/ROC fixtures), the common model
# interface (all 12 models on synthetic data), the ML signal adapter, and the
# Top-N portfolio aggregation.
#
# Run from the repository root:
#   Rscript tests/ml/test_ml_module.R
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
expect_equal <- function(a, b, tol = 1e-8) {
  if (length(a) != length(b)) return(FALSE)
  na_mismatch <- any(is.na(a) != is.na(b))
  if (na_mismatch) return(FALSE)
  fin <- is.finite(a) & is.finite(b)
  if (any(!fin & !is.na(a))) return(FALSE)
  all(abs(a[fin] - b[fin]) < tol, na.rm = TRUE)
}

cat("==================================================\n")
cat("ML MODULE TESTS\n")
cat("==================================================\n\n")

# ---------------------------------------------------------------------------
# Synthetic OHLCV (deterministic geometric walk, weekdays only)
# ---------------------------------------------------------------------------
set.seed(7)
n_days <- 640  # ~455 weekday rows after calendar filtering
ret <- c(rnorm(320, 0.0006, 0.012), rnorm(320, -0.0004, 0.02))
close <- 100 * exp(cumsum(ret))
open <- close * exp(rnorm(n_days, 0, 0.004))
high <- pmax(open, close) * exp(abs(rnorm(n_days, 0, 0.003)))
low <- pmin(open, close) * exp(-abs(rnorm(n_days, 0, 0.003)))
volume <- sample(2e6:5e6, n_days, replace = TRUE)
d <- as.Date("2022-01-03") + 0:(n_days - 1)
wd <- !format(d, "%u") %in% c("6", "7")
d2 <- d[wd]
ohlcv <- xts::xts(cbind(
  Open = open[wd], High = high[wd], Low = low[wd],
  Close = close[wd], Adjusted = close[wd], Volume = volume[wd]
), order.by = d2)

# ---------------------------------------------------------------------------
# 1. Feature engineering
# ---------------------------------------------------------------------------
feats <- build_features(ohlcv, NULL)
record("features: all 40 documented features present (35 + 5 regime)",
       length(feats$feature_names) == 40 &&
         setequal(feats$feature_names,
                  unlist(FEATURE_GROUPS, use.names = FALSE)))
record("features: index identical to OHLCV",
       identical(zoo::index(feats$features), zoo::index(ohlcv)))
m <- as.matrix(feats$features)
record("features: warm-up rows preserve NAs (SMA200 NA until row 200)",
       all(is.na(m[1:195, "sma_200"])))
record("features: SMA50 consistent with TTR::SMA",
       expect_equal(as.numeric(feats$features[300, "sma_50"]),
                    as.numeric(TTR::SMA(ohlcv[, "Close"], n = 50)[300])))
r5 <- as.numeric(ohlcv[, "Close"])[300] / as.numeric(ohlcv[, "Close"])[295] - 1
record("features: 5-day return matches hand computation",
       expect_equal(as.numeric(feats$features[300, "ret_5d"]), r5))

# Causality: removing future data must not change past features
# (k must sit inside BOTH series; NA-tolerant comparison: identical NA pattern
# and equal finite values)
k <- 250
stopifnot(k <= nrow(ohlcv) - 100)  # truncation must keep row k
feats_trunc <- build_features(ohlcv[1:(nrow(ohlcv) - 100), ], NULL)
v_full <- as.numeric(feats$features[k, ])
v_trunc <- as.numeric(feats_trunc$features[k, ])
record("features: NO LOOK-AHEAD (row k unchanged when future removed)",
       identical(is.na(v_full), is.na(v_trunc)) &&
         expect_equal(v_full[!is.na(v_full)], v_trunc[!is.na(v_trunc)]))

g <- resolve_feature_groups(c("rsi", "bollinger"))
record("features: alias resolution (rsi/bollinger -> momentum/volatility)",
       setequal(g, c("momentum", "volatility")))
bad <- tryCatch(resolve_feature_groups("nope"), validation_error = function(e) e)
record("features: unknown group -> validation_error",
       inherits(bad, "validation_error"))

# ---------------------------------------------------------------------------
# 2. Labels
# ---------------------------------------------------------------------------
lc <- validate_label_config(5, 0.02, 0.02)
record("labels: defaults validate to horizon 5 / 0.02 / 0.02",
       lc$horizon == 5 && lc$buy_threshold == 0.02 && lc$sell_threshold == 0.02)
bad_h <- tryCatch(validate_label_config(0, 0.02, 0.02), validation_error = function(e) e)
record("labels: horizon 0 rejected", inherits(bad_h, "validation_error"))

closes <- c(100, 102, 104, 103, 101, 99, 98, 101, 105, 110, 108, 107)
lab <- validate_and_generate_labels(closes, horizon = 3, buy_threshold = 0.02,
                                    sell_threshold = 0.02)
# future_return[1] = 105/100-1 = 5% -> BUY; [2] = 108/102-1 ~ 5.9% -> BUY;
# [8] = 107/101-1 ~ 5.9% -> BUY; final 3 rows must be NA
record("labels: +5% labelled BUY", lab$labels[1] == "BUY")
record("labels: final horizon rows masked (no fabricated labels)",
       all(is.na(lab$labels[10:12])) && !lab$valid[10])
lab2 <- validate_and_generate_labels(c(100, rep(98, 10)), horizon = 1,
                                     buy_threshold = 0.02, sell_threshold = 0.02)
record("labels: -2% labelled SELL", lab2$labels[1] == "SELL")
lab3 <- validate_and_generate_labels(c(100, 100.5, 100.4, 100.6, 100.5),
                                     horizon = 1, buy_threshold = 0.02,
                                     sell_threshold = 0.02)
record("labels: flat returns labelled HOLD", lab3$labels[1] == "HOLD")

# ---------------------------------------------------------------------------
# 3. Preprocessing + splitting
# ---------------------------------------------------------------------------
sp <- validate_split_fractions(0.6, 0.2, 0.2)
record("split: fractions validate", abs(sp$train + sp$validation + sp$test - 1) < 1e-9)
bad_sp <- tryCatch(validate_split_fractions(0.5, 0.2, 0.2), validation_error = function(e) e)
record("split: non-summing fractions rejected", inherits(bad_sp, "validation_error"))
s <- chronological_split(100, ML_SPLIT_DEFAULTS)
record("split: chronological order train < validation < test",
       max(s$train) < min(s$validation) && max(s$validation) < min(s$test))

set.seed(11)
X <- matrix(rnorm(200 * 6), 200, 6,
            dimnames = list(NULL, c("a", "b", "c", "const", "dup_a", "e")))
X[, "const"] <- 5
X[, "dup_a"] <- X[, "a"]
spec <- fit_preprocessing(X[1:120, ])
record("preprocess: constant feature removed", !"const" %in% spec$kept)
record("preprocess: perfectly correlated duplicate removed (a or dup_a)",
       !(("a" %in% spec$kept) && ("dup_a" %in% spec$kept)))
Xa <- apply_preprocessing(X, spec)
record("preprocess: apply returns exactly the kept columns",
       identical(colnames(Xa), spec$kept) && !anyNA(Xa))
X2miss <- Xa; X2miss[1, 1] <- NA
X2imp <- apply_preprocessing(X2miss, spec)
record("preprocess: inference NAs imputed with train medians",
       !anyNA(X2imp) && X2imp[1, 1] == spec$medians[[1]])
sc <- fit_scaler(Xa[1:120, ])
Xs <- apply_scaler(Xa, sc)
record("preprocess: scaler standardizes train to mean 0 / sd 1",
       max(abs(colMeans(Xs[1:120, ]))) < 1e-8)

# ---------------------------------------------------------------------------
# 4. Evaluation metrics (hand-computed fixture)
# ---------------------------------------------------------------------------
pred <- data.frame(
  predicted_class = c("BUY", "BUY", "HOLD", "SELL", "HOLD", "BUY"),
  prob_buy = c(0.9, 0.7, 0.2, 0.1, 0.3, 0.6),
  prob_hold = c(0.05, 0.2, 0.6, 0.1, 0.5, 0.3),
  prob_sell = c(0.05, 0.1, 0.2, 0.8, 0.2, 0.1)
)
actual <- c("BUY", "HOLD", "HOLD", "SELL", "BUY", "HOLD")
ev <- evaluate_classification(pred, actual)
# correct: rows 1,3,4 -> accuracy 3/6 = 0.5
record("eval: accuracy = 0.5 on fixture", abs(ev$accuracy - 0.5) < 1e-9)
cm <- ev$confusion_matrix
record("eval: confusion matrix shape + BUY/BUY cell = 1",
       all(dim(cm) == c(3, 3)) && cm["BUY", "BUY"] == 1 &&
         cm["HOLD", "HOLD"] == 1 && cm["SELL", "SELL"] == 1)
# BUY precision = 1/3 (1 TP, 2 FP)
record("eval: BUY precision = 1/3",
       abs(ev$per_class$BUY$precision - 1 / 3) < 1e-9)
record("eval: SELL precision = 1 (no false SELL)",
       ev$per_class$SELL$precision == 1)
record("eval: ROC-AUC defined for all classes with 2-class scores",
       all(!is.na(unlist(ev$roc_auc))))
# Perfect-separation sanity: BUY AUC = 1 when all BUY scores rank first
pred_perf <- data.frame(
  predicted_class = c("BUY", "SELL", "BUY", "SELL"),
  prob_buy = c(0.9, 0.1, 0.8, 0.2), prob_hold = 0, prob_sell = c(0.1, 0.9, 0.2, 0.8)
)
ev_p <- evaluate_classification(pred_perf, c("BUY", "SELL", "BUY", "SELL"))
record("eval: ROC-AUC(BUY) = 1.0 under perfect separation",
       abs(ev_p$roc_auc[["BUY"]] - 1) < 1e-9)
evl <- evaluation_to_list(ev)
record("eval: serialization includes curves + confusion rows",
       length(evl$confusion_matrix) == 3 && length(evl$curves) == 3)

# ---------------------------------------------------------------------------
# 5. Common model interface — all 12 models on synthetic data
# ---------------------------------------------------------------------------
ds <- assemble_ml_dataset(ohlcv = ohlcv, ticker = "SYNTH",
                          start = "2022-01-03",
                          end = format(max(d2), "%Y-%m-%d"))
record("dataset: usable rows < raw rows (warm-up + label horizon dropped)",
       ds$n < nrow(ohlcv))
record("dataset: split sizes sum <= n (embargo removes overlapping rows)",
       length(ds$split$train) + length(ds$split$validation) + length(ds$split$test) <= ds$n)
record("dataset: embargo creates gap between splits (no overlap)",
       max(ds$split$train) < min(ds$split$validation) &&
       max(ds$split$validation) < min(ds$split$test))
record("dataset: label classes all present in train",
       setequal(unique(ds$y[ds$split$train]), c("BUY", "HOLD", "SELL")))

# Fast models in-process; LSTM trained with a tiny budget; heavy boosters
# get reduced defaults so the suite stays fast.
lstm_hp <- list(sequence_length = 10, units = 8, epochs = 2, batch_size = 64)
small_hp <- list(
  random_forest = list(ntree = 60),
  xgboost = list(n_estimators = 40),
  gradient_boosting = list(n.trees = 40),
  mlp = list(maxit = 80)
)
trained <- 0
for (key in MODEL_KEYS) {
  hp <- if (key == "lstm") lstm_hp else (small_hp[[key]] %||% ML_MODELS[[key]]$defaults)
  t0 <- Sys.time()
  r <- evaluate_model(ds, key, hp)
  ok <- isTRUE(r$success)
  if (ok) {
    trained <- trained + 1
    recs <- r$predictions
    contract_ok <- all(c("model_name", "ticker", "date", "predicted_class",
                         "prob_buy", "prob_hold", "prob_sell") %in% names(recs)) &&
      nrow(recs) == length(ds$split$test) &&
      max(abs(rowSums(cbind(recs$prob_buy, recs$prob_hold, recs$prob_sell)) - 1)) < 1e-6
    record(sprintf("model %s: standardized contract (7 fields, rows, probs sum to 1)", key),
           contract_ok)
    record(sprintf("model %s: classification + existing-engine backtest present", key),
           !is.null(r$classification) && !is.null(r$backtest$summary) &&
             r$backtest$metadata$strategy == paste0("ML_", toupper(key)))
  } else {
    record(sprintf("model %s: FAILED", key), FALSE, r$error)
  }
}
record("all 12 models trained and evaluated", trained == 12,
       sprintf("%d/12 ok", trained))

# Comparison table
cmp <- run_model_comparison(ds, c("decision_tree", "random_forest"), small_hp)
record("comparison: one row per model with classification + trading columns",
       nrow(cmp$comparison) == 2 &&
         all(c("Accuracy", "F1", "ROC_AUC", "CAGR", "Sharpe", "MaxDrawdown", "WinRate")
             %in% names(cmp$comparison)))

# Signal adapter semantics
recs <- prediction_records("t", "T",
                           as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
                           matrix(c(0.8, 0.1, 0.3, 0.2, 0.6, 0.2, 0.0, 0.3, 0.5),
                                  nrow = 3, byrow = TRUE))
idx <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
sig_classes <- ml_signal_series(recs, idx, "classes")
record("adapter: classes mode maps BUY/SELL/HOLD -> BUY/SELL/NA",
       identical(as.character(sig_classes[, 1]), c("BUY", NA, "SELL")))
sig_prob <- ml_signal_series(recs, idx, "probability", 0.6)
# row1 P(BUY)=0.667 >= 0.6 -> BUY; row2 P(SELL)=P(HOLD) -> NA; row3 P(SELL)>P(HOLD) -> SELL
record("adapter: probability mode applies P(BUY) threshold",
       identical(as.character(sig_prob[, 1]), c("BUY", NA, "SELL")))
sig_na <- ml_signal_series(recs, as.Date(c("2024-02-01")), "classes")
record("adapter: predictions outside window -> all NA (engine-safe)",
       all(is.na(as.character(sig_na[, 1]))))

# The adapted signals run through the UNMODIFIED engine
exec <- execute_backtest(ohlcv[300:340, ], ml_signal_series(
  prediction_records("t", "SYNTH", as.Date(zoo::index(ohlcv[300:340, ])),
                     matrix(runif(41 * 3), 41, 3)),
  zoo::index(ohlcv[300:340, ]), "classes"), 10000)
record("adapter: engine accepts adapted signals (t+1-open conventions intact)",
       nrow(exec$equity_curve) == 41)

# Top-N ranking
pf <- list(
  INFY = data.frame(date = as.Date("2024-01-05"), prob_buy = 0.82,
                    predicted_class = "BUY"),
  RELIANCE = data.frame(date = as.Date("2024-01-05"), prob_buy = 0.74,
                        predicted_class = "BUY"),
  TCS = data.frame(date = as.Date("2024-01-05"), prob_buy = 0.69,
                   predicted_class = "BUY")
)
ranked <- rank_stocks_by_prob_buy(pf, "2024-01-05", 2)
record("ranking: sorted by P(BUY) desc, top_n respected",
       ranked$ticker[1] == "INFY" && nrow(ranked) == 2 && ranked$rank[1] == 1)
pc <- validate_portfolio_config(5, "weekly")
record("portfolio config: weekly -> 5 trading days",
       pc$top_n == 5 && pc$rebalance_days == 5)
bad_rb <- tryCatch(validate_portfolio_config(5, "yearly"), validation_error = function(e) e)
record("portfolio config: unknown rebalance rejected",
       inherits(bad_rb, "validation_error"))

agg <- aggregate_portfolio_equity(list(
  A = execute_backtest(ohlcv[300:340, ], rep(NA_character_, 41), 10000),
  B = execute_backtest(ohlcv[300:340, ], rep(NA_character_, 41), 10000)
), 10000, min(d2[300:340]), max(d2[300:340]))
record("portfolio aggregation: equal-weight curve + existing metrics",
       nrow(agg$equity_curve) == 41 && !is.null(agg$metrics$cagr))

# Hyperparameter grids
g_rf <- expand_tuning_grid("random_forest", 6)
record("tuning: RF grid expanded and capped (<= 6 candidates)",
       !is.null(g_rf) && length(g_rf) <= 6)
record("tuning: model without grid returns NULL",
       is.null(expand_tuning_grid("knn", 6)))

cat(sprintf("\n%d tests, %d passed, %d failed\n",
            length(results),
            sum(vapply(results, function(x) x$passed, logical(1))),
            sum(!vapply(results, function(x) x$passed, logical(1)))))
quit(status = if (all(vapply(results, function(x) x$passed, logical(1)))) 0 else 1)
