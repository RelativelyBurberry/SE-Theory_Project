# AlphaForge Insight — ML pipeline orchestration (ML Phases A–F)
#
# One entry point that assembles the full supervised pipeline and one that
# runs the 12-model comparison, reusing the EXISTING market-data layer
# (fetch_ohlcv) and the EXISTING backtesting engine (run_backtest) for all
# trading performance. The backtester itself is never modified or duplicated:
# ml_signal_series() adapts predictions into its BUY/SELL/NA signal contract.
#
#   fetch_ohlcv -> build_features -> labels -> chronological split ->
#   preprocessing (fit on train) -> model train -> predictions ->
#   classification metrics + adapted backtest (existing engine)
#
# Prediction contract (every model, standardized):
#   model_name, ticker, date, predicted_class, prob_buy, prob_hold, prob_sell

library(xts)
suppressPackageStartupMessages(library(TTR))

ML_MODULE_DIR <- function() {
  file.path(dirname(sys.frame(1)$ofile %||% "backend"), "analytics", "ml")
}

#' Assemble the model-ready dataset for one ticker.
#'
#' Fetches OHLCV (existing layer), builds features, generates labels, drops
#' warm-up/unlabelled rows and performs the chronological split. The future
#' return is used ONLY inside the label generator and is never attached to
#' the feature matrices (leakage rule).
#'
#' @param ticker Normalized ticker.
#' @param start,end Date strings (YYYY-MM-DD).
#' @param horizon Label horizon (trading days).
#' @param buy_threshold,sell_threshold Label thresholds (decimals).
#' @param groups Feature groups (NULL = all).
#' @param split_fractions Named list(train, validation, test).
#' @return list(ohlcv, features_xts, dates, X_raw (n x p raw features),
#'   y (character labels), split (index list), n, p, feature_names).
assemble_ml_dataset <- function(ohlcv = NULL, ticker, start, end,
                                horizon = 5, buy_threshold = 0.02,
                                sell_threshold = 0.02, groups = NULL,
                                split_fractions = ML_SPLIT_DEFAULTS) {
  ohlcv <- if (is.null(ohlcv)) fetch_ohlcv(ticker, start, end) else ohlcv
  feats <- build_features(ohlcv, groups)
  labels <- validate_and_generate_labels(as.numeric(ohlcv[, "Close"]),
                                         horizon, buy_threshold, sell_threshold)

  X_raw <- feats$features
  y <- labels$labels
  valid <- labels$valid & !is.na(y)

  # Usable modelling rows: label defined AND enough observed features to be
  # informative (warm-up rows carry mostly-NA features and are dropped as a
  # block). Requiring >= 70% non-NA features keeps the long SMA-200 warm-up
  # rows out of the TRAIN split — which matters because all preprocessing
  # statistics and the chronological split are fitted on the earliest rows.
  non_na_frac <- 1 - rowSums(is.na(as.matrix(X_raw))) / ncol(X_raw)
  keep <- valid & (non_na_frac >= 0.7)
  if (sum(keep) < 10) {
    stop(validation_error(sprintf(
      "Only %d usable feature/label rows for %s — widen the date range (features need warm-up and labels need +%d days of future data).",
      sum(keep), ticker, labels$horizon
    )))
  }

  X_raw <- X_raw[keep, ]
  y <- y[keep]
  dates <- as.Date(zoo::index(X_raw))

  split <- chronological_split(nrow(X_raw), split_fractions, embargo = labels$horizon)

  list(
    ohlcv = ohlcv,
    ticker = ticker,
    features_xts = feats$features,
    dates = dates,
    X_raw = as.matrix(X_raw),
    y = y,
    split = split,
    n = nrow(X_raw),
    p = ncol(X_raw),
    feature_names = feats$feature_names,
    horizon = labels$horizon,
    label_config = list(horizon = labels$horizon,
                        buy_threshold = labels$buy_threshold,
                        sell_threshold = labels$sell_threshold)
  )
}

#' Prepare scaled model inputs from a dataset (fit preprocessing on TRAIN).
#'
#' @param ds assemble_ml_dataset() output.
#' @param key Canonical model key.
#' @param hyperparams Merged hyperparameters.
#' @return list(spec, scaler, X_train, X_val, X_test, y_train, y_val, y_test,
#'   dates_*).
prepare_model_inputs <- function(ds, key, hyperparams = list()) {
  spec <- fit_preprocessing(ds$X_raw[ds$split$train, , drop = FALSE])
  X_imp <- apply_preprocessing(ds$X_raw, spec)

  scale_needed <- if (key == "lstm") TRUE else isTRUE(ML_MODELS[[key]]$needs_scaling)
  scaler <- if (scale_needed) fit_scaler(X_imp[ds$split$train, , drop = FALSE]) else NULL

  mk <- function(idx) {
    M <- X_imp[idx, , drop = FALSE]
    if (!is.null(scaler)) M <- apply_scaler(M, scaler) else M
    M
  }
  list(
    spec = spec,
    scaler = scaler,
    X_train = mk(ds$split$train),
    X_val = mk(ds$split$validation),
    X_test = mk(ds$split$test),
    y_train = ds$y[ds$split$train],
    y_val = ds$y[ds$split$validation],
    y_test = ds$y[ds$split$test],
    dates_train = ds$dates[ds$split$train],
    dates_val = ds$dates[ds$split$validation],
    dates_test = ds$dates[ds$split$test]
  )
}

#' Build the standardized prediction-record data.frame.
#'
#' @param model_name Canonical model key.
#' @param ticker Ticker label.
#' @param dates Date vector for the prediction rows.
#' @param P n x 3 probability matrix [BUY, HOLD, SELL].
#' @return data.frame(model_name, ticker, date, predicted_class, prob_buy,
#'   prob_hold, prob_sell).
prediction_records <- function(model_name, ticker, dates, P) {
  P <- normalize_probabilities(P)
  cls <- apply(P, 1, function(r) CLASS_LEVELS[which.max(r)])
  data.frame(
    model_name = model_name,
    ticker = ticker,
    date = as.Date(dates),
    predicted_class = cls,
    prob_buy = P[, "BUY"],
    prob_hold = P[, "HOLD"],
    prob_sell = P[, "SELL"],
    stringsAsFactors = FALSE
  )
}

#' Convert ML predictions into the EXISTING backtester's signal vector.
#'
#' ADAPTER (ML plan section 12 / section 20): the engine consumes an xts of
#' "BUY"/"SELL"/NA aligned with OHLCV and executes at the t+1 OPEN — its
#' semantics are untouched. Mapping (signal_mode):
#'   - "classes": predicted BUY -> BUY, SELL -> SELL, HOLD -> NA
#'   - "probability": BUY when P(BUY) >= prob_threshold (else NA); SELL
#'     emitted only from a LONG state by the engine, so P(SELL) maps to SELL
#'     when P(BUY) < threshold and P(SELL) > P(HOLD).
#'
#' @param predictions prediction_records() data.frame (test-period rows).
#' @param ohlcv_index Date index of the OHLCV series passed to run_backtest.
#' @param signal_mode "classes" (default) or "probability".
#' @param prob_threshold Minimum P(BUY) for a BUY signal in probability mode
#'   (default 0.5).
#' @return xts of BUY/SELL/NA with index == ohlcv_index.
ml_signal_series <- function(predictions, ohlcv_index,
                             signal_mode = "classes", prob_threshold = 0.5) {
  idx <- as.Date(ohlcv_index)
  sig <- rep(NA_character_, length(idx))
  pred_date <- as.Date(predictions$date)
  pos <- match(pred_date, idx)
  pos <- pos[!is.na(pos)]
  if (length(pos) == 0) {
    return(xts::xts(sig, order.by = idx))
  }
  keep <- !is.na(match(as.Date(predictions$date), idx))
  pd <- predictions[keep, ]
  pos <- match(as.Date(pd$date), idx)

  if (signal_mode == "probability") {
    # Normalize the raw (input) probabilities first so the threshold applies
    # on a proper simplex (rows sum to 1); prediction_records() already did
    # this for its own columns, but the adapter must not assume it.
    pb <- as.numeric(pd$prob_buy); ph <- as.numeric(pd$prob_hold)
    ps <- as.numeric(pd$prob_sell)
    tot <- pb + ph + ps
    tot[tot == 0] <- 1
    pb <- pb / tot; ph <- ph / tot; ps <- ps / tot
    buy <- pb >= prob_threshold
    sell <- !buy & (ps > ph)
  } else {
    buy <- pd$predicted_class == "BUY"
    sell <- pd$predicted_class == "SELL"
  }
  sig[pos[buy]] <- "BUY"
  sig[pos[sell]] <- "SELL"
  xts::xts(sig, order.by = idx)
}

#' Evaluate ONE model end-to-end on a dataset (train + classify + backtest).
#'
#' @param ds assemble_ml_dataset() output.
#' @param key Canonical model key.
#' @param hyperparams Merged hyperparameters.
#' @param evaluate_on Which labels to score: "test" (default) or "validation".
#' @param initial_capital Backtest capital (existing engine).
#' @param signal_mode,prob_threshold Adapter settings.
#' @param tune Boolean — run validation-tuned hyperparameters (simple grid).
#' @return list(success, key, label, predictions (records), classification,
#'   backtest (existing-engine result object), hyperparams, train_info,
#'   error (when success = FALSE)).
evaluate_model <- function(ds, key, hyperparams = list(), evaluate_on = "test",
                           initial_capital = 10000, signal_mode = "classes",
                           prob_threshold = 0.5, tune = FALSE) {
  out <- list(success = TRUE, key = key, label = ML_MODELS[[key]]$label)
  tryCatch(
    {
      inputs <- prepare_model_inputs(ds, key, hyperparams)

      # Optional controlled hyperparameter tuning ON VALIDATION ONLY.
      if (isTRUE(tune)) {
        tuned <- tune_hyperparams(ds, key, limit = 4)
        hyperparams <- tuned$best
        out$tuning <- tuned$summary
        inputs <- prepare_model_inputs(ds, key, hyperparams)
      }

      if (key == "lstm") {
        handle <- train_lstm_model(inputs$X_train, inputs$y_train,
                                   hyperparams, inputs$X_val, inputs$y_val)
      } else {
        handle <- train_tabular_model(key, inputs$X_train, inputs$y_train,
                                      hyperparams)
      }

      eval_idx <- if (evaluate_on == "validation") ds$split$validation else ds$split$test
      eval_dates <- if (evaluate_on == "validation") inputs$dates_val else inputs$dates_test
      eval_y <- if (evaluate_on == "validation") inputs$y_val else inputs$y_test
      eval_X <- if (evaluate_on == "validation") inputs$X_val else inputs$X_test

      P <- handle$predict(eval_X)
      recs <- prediction_records(key, ds$ticker, eval_dates, P)

      cls <- evaluate_classification(recs, eval_y)

      # --- Existing backtester (UNCHANGED) on the evaluation period --------
      # Recover the exact OHLCV rows whose dates survived dataset assembly.
      kept_index <- zoo::index(ds$features_xts)
      ohlcv_eval <- ds$ohlcv[kept_index[eval_idx], ]
      sig <- ml_signal_series(recs, zoo::index(ohlcv_eval), signal_mode,
                              prob_threshold)
      bt <- run_backtest(
        ohlcv = ohlcv_eval,
        signals = sig,
        initial_capital = initial_capital,
        start_date = utils::head(zoo::index(ohlcv_eval), 1),
        end_date = utils::tail(zoo::index(ohlcv_eval), 1),
        ticker = ds$ticker,
        strategy = sprintf("ML_%s", toupper(key)),
        parameters = list(hyperparams = hyperparams,
                          signal_mode = signal_mode,
                          prob_threshold = prob_threshold,
                          horizon = ds$horizon)
      )

      imp <- if (key == "lstm") NULL else tabular_feature_importance(handle)

      out$predictions <- recs
      out$classification <- cls
      out$backtest <- bt
      out$hyperparams <- hyperparams
      out$feature_importance <- imp
      out$train_info <- list(
        n_train = length(ds$split$train),
        n_validation = length(ds$split$validation),
        n_test = length(ds$split$test),
        features_used = inputs$spec$kept,
        dropped_missing = inputs$spec$dropped_missing,
        dropped_zero_var = inputs$spec$dropped_zero_var,
        dropped_correlated = inputs$spec$dropped_correlated,
        scaled = !is.null(inputs$scaler)
      )
      out
    },
    validation_error = function(e) {
      list(success = FALSE, key = key, label = ML_MODELS[[key]]$label,
           error = conditionMessage(e))
    },
    error = function(e) {
      # One model failing must never block the comparison (rule 18): the
      # failure is recorded per-model and surfaced in the response.
      list(success = FALSE, key = key, label = ML_MODELS[[key]]$label,
           error = conditionMessage(e))
    }
  )
}

#' Evaluate a Python time-series model end-to-end.
#'
#' @param ds assemble_ml_dataset() output.
#' @param key Canonical model key (must be a ts_* model).
#' @param hyperparams Merged hyperparameters.
#' @param evaluate_on Which labels to score: "test" (default) or "validation".
#' @param initial_capital Backtest capital.
#' @param buy_threshold Threshold for BUY signals (default 0.02).
#' @param sell_threshold Threshold for SELL signals (default -0.02).
#' @return list(success, key, label, predictions, regression, backtest, ...).
evaluate_ts_model <- function(ds, key, hyperparams = list(), evaluate_on = "test",
                              initial_capital = 10000,
                              buy_threshold = 0.02,
                              sell_threshold = 0.02) {
  ts_key <- sub("^ts_", "", key)
  out <- list(success = TRUE, key = key, label = ML_MODELS[[key]]$label)
  
  tryCatch({
    eval_idx <- if (evaluate_on == "validation") ds$split$validation else ds$split$test
    eval_dates <- ds$dates[eval_idx]
    
    hp <- hyperparams %||% ML_MODELS[[key]]$defaults
    
    results <- execute_ts_models(
      X_raw = ds$X_raw,
      y = NULL,
      dates = ds$dates,
      split = ds$split,
      models = ts_key,
      hyperparams = setNames(list(hp), ts_key),
      horizon = ds$horizon
    )
    
    result <- results[[ts_key]]
    
    if (!isTRUE(result$success)) {
      return(list(
        success = FALSE,
        key = key,
        label = ML_MODELS[[key]]$label,
        error = result$error %||% "Python time-series model failed"
      ))
    }
    
    predictions <- result$predictions
    pred_dates <- result$prediction_dates
    
    if (is.null(predictions) || length(predictions) == 0) {
      return(list(
        success = FALSE,
        key = key,
        label = ML_MODELS[[key]]$label,
        error = "No predictions returned from Python service"
      ))
    }
    
    y_test <- as.numeric(ds$X_raw[eval_idx, 1])
    
    kept_index <- zoo::index(ds$features_xts)
    ohlcv_eval <- ds$ohlcv[kept_index[eval_idx], ]
    
    eval_result <- evaluate_ts_predictions(
      predictions = predictions,
      y_test = y_test,
      ohlcv_test = ohlcv_eval,
      dates_test = eval_dates,
      ticker = ds$ticker,
      initial_capital = initial_capital,
      signal_mode = "regression",
      buy_threshold = buy_threshold,
      sell_threshold = sell_threshold
    )
    
    recs <- ts_predictions_to_records(
      predictions = predictions,
      dates = eval_dates,
      ticker = ds$ticker,
      buy_threshold = buy_threshold,
      sell_threshold = sell_threshold
    )
    
    out$predictions <- recs
    out$regression <- eval_result$regression
    out$backtest <- eval_result$backtest
    out$hyperparams <- hp
    out$train_info <- result$train_info
    out$feature_importance <- result$feature_importance
    out
  }, error = function(e) {
    list(success = FALSE, key = key, label = ML_MODELS[[key]]$label,
         error = conditionMessage(e))
  })
}

#' Run all (or a subset of) the 12 models and build the comparison table.
#'
#' @param ds assemble_ml_dataset() output.
#' @param models Character vector of model keys (NULL = all 12).
#' @param hyperparams Named list of per-model hyperparameter lists.
#' @param ... Passed through to evaluate_model().
#' @return list(results = named list of per-model outcomes (failed models
#'   carry success = FALSE + error), comparison = data.frame with one row per
#'   successful model: Model, Accuracy, Precision, Recall, F1, ROC_AUC,
#'   PR_AUC, CAGR, Sharpe, Sortino, MaxDD, WinRate, Volatility, Trades).
run_model_comparison <- function(ds, models = NULL, hyperparams = list(), ...) {
  keys <- if (is.null(models)) MODEL_KEYS else {
    vapply(models, resolve_model_key, character(1))
  }
  results <- list()
  for (key in keys) {
    message(sprintf("[ml/compare] Running model: %s", key))
    hp <- hyperparams[[key]] %||% list()
    
    is_ts <- is_ts_model(key)
    
    if (is_ts) {
      results[[key]] <- evaluate_ts_model(ds, key, hp, ...)
    } else {
      results[[key]] <- evaluate_model(ds, key, hp, ...)
    }
    
    if (isTRUE(results[[key]]$success)) {
      message(sprintf("[ml/compare] Model completed: %s", key))
    } else {
      message(sprintf("[ml/compare] Model failed: %s - %s", key, results[[key]]$error))
    }
  }

  rows <- lapply(names(results), function(key) {
    r <- results[[key]]
    if (!isTRUE(r$success)) {
      return(data.frame(
        Model = r$label,
        Status = "failed",
        Error = r$error,
        RMSE = NA_real_,
        MAE = NA_real_,
        R2 = NA_real_,
        IC = NA_real_,
        DirAcc = NA_real_,
        CAGR = NA_real_,
        Sharpe = NA_real_,
        Sortino = NA_real_,
        MaxDrawdown = NA_real_,
        WinRate = NA_real_,
        Volatility = NA_real_,
        TotalTrades = NA_integer_,
        Accuracy = NA_real_,
        Precision = NA_real_,
        Recall = NA_real_,
        F1 = NA_real_,
        ROC_AUC = NA_real_,
        PR_AUC = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    
    tryCatch({
      is_ts <- is_ts_model(key)
      
      safe_num <- function(x, default = NA_real_) {
        if (is.null(x)) return(default)
        x <- as.numeric(x)
        if (length(x) == 0) return(default)
        if (length(x) > 1) return(x[1])
        if (is.na(x) || !is.finite(x)) return(default)
        x
      }
      
      safe_int <- function(x, default = NA_integer_) {
        if (is.null(x)) return(default)
        x <- as.integer(x)
        if (length(x) == 0) return(default)
        if (length(x) > 1) return(x[1])
        if (is.na(x)) return(default)
        x
      }
      
      m <- r$backtest$summary
      
      if (is_ts) {
        reg <- r$regression
        df <- data.frame(
          Model = as.character(r$label),
          Status = "ok",
          Error = NA_character_,
          RMSE = safe_num(reg$rmse),
          MAE = safe_num(reg$mae),
          R2 = safe_num(reg$r2),
          IC = safe_num(reg$ic),
          DirAcc = safe_num(reg$directional_accuracy),
          CAGR = safe_num(m$cagr),
          Sharpe = safe_num(m$sharpe),
          Sortino = safe_num(m$sortino),
          MaxDrawdown = safe_num(m$maximum_drawdown),
          WinRate = safe_num(m$win_rate),
          Volatility = safe_num(m$volatility),
          TotalTrades = safe_int(m$total_trades),
          Accuracy = NA_real_,
          Precision = NA_real_,
          Recall = NA_real_,
          F1 = NA_real_,
          ROC_AUC = NA_real_,
          PR_AUC = NA_real_,
          stringsAsFactors = FALSE
        )
      } else {
        cls <- r$classification
        df <- data.frame(
          Model = as.character(r$label),
          Status = "ok",
          Error = NA_character_,
          RMSE = NA_real_,
          MAE = NA_real_,
          R2 = NA_real_,
          IC = NA_real_,
          DirAcc = NA_real_,
          CAGR = safe_num(m$cagr),
          Sharpe = safe_num(m$sharpe),
          Sortino = safe_num(m$sortino),
          MaxDrawdown = safe_num(m$maximum_drawdown),
          WinRate = safe_num(m$win_rate),
          Volatility = safe_num(m$volatility),
          TotalTrades = safe_int(m$total_trades),
          Accuracy = safe_num(cls$accuracy),
          Precision = safe_num(cls$precision),
          Recall = safe_num(cls$recall),
          F1 = safe_num(cls$f1),
          ROC_AUC = safe_num(cls$roc_auc_macro),
          PR_AUC = safe_num(cls$pr_auc_macro),
          stringsAsFactors = FALSE
        )
      }
      
      for (col in names(df)) {
        if (length(df[[col]]) != 1) {
          stop(sprintf("Column %s has length %d instead of 1", col, length(df[[col]])))
        }
      }
      
      df
    }, error = function(e) {
      warning(sprintf("[ml/compare] %s: failed to build comparison row: %s",
                      r$label, conditionMessage(e)))
      data.frame(
        Model = r$label,
        Status = "failed",
        Error = sprintf("Comparison aggregation error: %s", conditionMessage(e)),
        RMSE = NA_real_,
        MAE = NA_real_,
        R2 = NA_real_,
        IC = NA_real_,
        DirAcc = NA_real_,
        CAGR = NA_real_,
        Sharpe = NA_real_,
        Sortino = NA_real_,
        MaxDrawdown = NA_real_,
        WinRate = NA_real_,
        Volatility = NA_real_,
        TotalTrades = NA_integer_,
        Accuracy = NA_real_,
        Precision = NA_real_,
        Recall = NA_real_,
        F1 = NA_real_,
        ROC_AUC = NA_real_,
        PR_AUC = NA_real_,
        stringsAsFactors = FALSE
      )
    })
  })
  
  if (length(rows) > 0) {
    expected_cols <- colnames(rows[[1]])
    for (i in seq_along(rows)) {
      if (!identical(expected_cols, colnames(rows[[i]]))) {
        stop(sprintf("Column mismatch in model %s", names(results)[i]))
      }
    }
    row_lengths <- vapply(rows, nrow, integer(1))
    if (any(row_lengths != 1L)) {
      stop(sprintf("Invalid comparison row dimensions: %s",
                   paste(names(results)[row_lengths != 1L], collapse = ", ")))
    }
  }
  
  comparison <- do.call(rbind, rows)
  list(results = results, comparison = comparison)
}
