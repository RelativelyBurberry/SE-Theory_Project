# AlphaForge Insight — Python time-series model integration
#
# Integration layer that calls the Python FastAPI service for time-series
# forecasting models (baselines, statistical, ML, deep learning).
#
# This module:
#   - Provides a unified interface to the Python time-series models
#   - Handles service health checks and availability
#   - Converts R data structures to Python service format
#   - Normalizes responses back to the standard ML contract
#
# Models available:
#   Baselines: zero_return, historical_mean
#   Statistical: linear_regression, elastic_net, arima, ets, garch
#   ML: random_forest, xgboost, svr
#   Deep Learning: lstm, gru

library(httr)
library(jsonlite)

# Python service configuration
PYTHON_TS_HOST <- Sys.getenv("ALPHAFORGE_PYTHON_TS_HOST", "localhost")
PYTHON_TS_PORT <- as.integer(Sys.getenv("ALPHAFORGE_PYTHON_TS_PORT", 8001))
PYTHON_TS_BASE_URL <- sprintf("http://%s:%d", PYTHON_TS_HOST, PYTHON_TS_PORT)

# Time-series model registry (Python models)
TS_MODELS <- list(
  zero_return = list(
    label = "Zero-Return Baseline",
    family = "baseline",
    needs_scaling = FALSE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(),
    python_only = TRUE
  ),
  historical_mean = list(
    label = "Historical-Mean Baseline",
    family = "baseline",
    needs_scaling = FALSE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(),
    python_only = TRUE
  ),
  linear_regression = list(
    label = "Linear Regression",
    family = "statistical",
    needs_scaling = TRUE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(),
    python_only = TRUE
  ),
  elastic_net = list(
    label = "Elastic Net",
    family = "statistical",
    needs_scaling = TRUE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(alpha = 1.0, l1_ratio = 0.5),
    python_only = TRUE
  ),
  arima = list(
    label = "ARIMA",
    family = "statistical",
    needs_scaling = FALSE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(order = c(1L, 0L, 0L)),
    python_only = TRUE
  ),
  ets = list(
    label = "ETS (Exponential Smoothing)",
    family = "statistical",
    needs_scaling = FALSE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(trend = NULL, seasonal = NULL),
    python_only = TRUE
  ),
  garch = list(
    label = "GARCH",
    family = "statistical",
    needs_scaling = FALSE,
    is_regression = FALSE,
    is_volatility = TRUE,
    defaults = list(p = 1L, q = 1L),
    python_only = TRUE
  ),
  random_forest = list(
    label = "Random Forest Regression",
    family = "ml",
    needs_scaling = FALSE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(n_estimators = 200L, max_depth = 10L, min_samples_leaf = 5L),
    python_only = TRUE
  ),
  xgboost = list(
    label = "XGBoost Regression",
    family = "ml",
    needs_scaling = FALSE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(n_estimators = 200L, max_depth = 3L, learning_rate = 0.05),
    python_only = TRUE
  ),
  svr = list(
    label = "Support Vector Regression",
    family = "ml",
    needs_scaling = TRUE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(kernel = "rbf", C = 1.0, epsilon = 0.1),
    python_only = TRUE
  ),
  lstm = list(
    label = "LSTM",
    family = "deep_learning",
    needs_scaling = TRUE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(sequence_length = 20L, units = 32L, epochs = 20L, batch_size = 32L, dropout = 0.2),
    python_only = TRUE
  ),
  gru = list(
    label = "GRU",
    family = "deep_learning",
    needs_scaling = TRUE,
    is_regression = TRUE,
    is_volatility = FALSE,
    defaults = list(sequence_length = 20L, units = 32L, epochs = 20L, batch_size = 32L, dropout = 0.2),
    python_only = TRUE
  )
)

TS_MODEL_KEYS <- names(TS_MODELS)

#' Check if the Python time-series service is healthy.
#'
#' @return List with status, version, and available models, or NULL if unavailable.
check_python_ts_health <- function() {
  url <- paste0(PYTHON_TS_BASE_URL, "/health")
  tryCatch(
    {
      resp <- httr::GET(url, httr::timeout(5))
      if (httr::status_code(resp) == 200) {
        content <- httr::content(resp, as = "text", encoding = "UTF-8")
        jsonlite::fromJSON(content)
      } else {
        NULL
      }
    },
    error = function(e) {
      NULL
    }
  )
}

#' Get available models from the Python service.
#'
#' @return List of model metadata, or NULL if service unavailable.
get_python_ts_models <- function() {
  url <- paste0(PYTHON_TS_BASE_URL, "/models")
  tryCatch(
    {
      resp <- httr::GET(url, httr::timeout(5))
      if (httr::status_code(resp) == 200) {
        content <- httr::content(resp, as = "text", encoding = "UTF-8")
        jsonlite::fromJSON(content)$models
      } else {
        NULL
      }
    },
    error = function(e) {
      NULL
    }
  )
}

#' Execute time-series models via the Python service.
#'
#' @param X_raw Raw feature matrix (n x p).
#' @param y Target vector (future returns), or NULL for unsupervised models.
#' @param dates Date vector aligned with X_raw rows.
#' @param split Named list with train, validation, test index vectors.
#' @param models Character vector of model keys to run.
#' @param hyperparams Named list of per-model hyperparameter lists.
#' @param horizon Forecast horizon (for metadata).
#' @return Named list of results, one per model. Each result contains:
#'   success, key, label, predictions, metrics, error (if failed).
execute_ts_models <- function(X_raw, y = NULL, dates, split,
                              models = TS_MODEL_KEYS,
                              hyperparams = list(),
                              horizon = 5) {
  # Prepare request payload
  n <- nrow(X_raw)
  p <- ncol(X_raw)
  
  # Convert features to list of row records
  feature_names <- colnames(X_raw)
  if (is.null(feature_names)) {
    feature_names <- paste0("f", seq_len(p))
  }
  
  features_list <- lapply(seq_len(n), function(i) {
    row <- as.list(X_raw[i, ])
    names(row) <- feature_names
    row
  })
  
  # Prepare dates
  dates_str <- format(as.Date(dates), "%Y-%m-%d")
  
  # Determine split indices
  train_end_idx <- max(split$train)
  val_end_idx <- max(split$validation)
  
  # Build hyperparameters dict (convert R vectors to lists for JSON)
  hp_dict <- list()
  for (key in models) {
    if (!is.null(hyperparams[[key]])) {
      hp_dict[[key]] <- hyperparams[[key]]
    } else if (!is.null(TS_MODELS[[key]]$defaults)) {
      # Use defaults
      hp_dict[[key]] <- TS_MODELS[[key]]$defaults
    }
  }
  
  payload <- list(
    features = features_list,
    target = if (!is.null(y)) as.list(y) else NULL,
    dates = as.list(dates_str),
    horizon = horizon,
    train_end_idx = train_end_idx,
    val_end_idx = val_end_idx,
    models = models,
    hyperparameters = hp_dict
  )
  
  # Remove NULL fields
  payload <- payload[!sapply(payload, is.null)]
  
  # Call Python service
  url <- paste0(PYTHON_TS_BASE_URL, "/execute")
  
  tryCatch(
    {
      resp <- httr::POST(
        url,
        body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
        httr::content_type_json(),
        httr::timeout(300)  # 5 minutes for deep learning
      )
      
      if (httr::status_code(resp) != 200) {
        # Return all models as failed
        return(lapply(models, function(key) {
          list(
            success = FALSE,
            key = key,
            label = TS_MODELS[[key]]$label,
            error = sprintf("Python service returned status %d", httr::status_code(resp))
          )
        }))
      }
      
      content <- httr::content(resp, as = "text", encoding = "UTF-8")
      result <- jsonlite::fromJSON(content)
      
      # Convert results to R format
      results <- list()
      for (i in seq_along(result$results)) {
        r <- result$results[[i]]
        
        if (r$status == "ok") {
          results[[r$model_key]] <- list(
            success = TRUE,
            key = r$model_key,
            label = r$model_name,
            family = r$model_family,
            predictions = if (!is.null(r$predictions)) unlist(r$predictions) else NULL,
            prediction_dates = if (!is.null(r$prediction_dates)) unlist(r$prediction_dates) else NULL,
            metrics = r$metrics,
            train_info = r$train_info,
            hyperparameters_used = r$hyperparameters_used,
            feature_importance = r$feature_importance
          )
        } else {
          results[[r$model_key]] <- list(
            success = FALSE,
            key = r$model_key,
            label = r$model_name %||% TS_MODELS[[r$model_key]]$label,
            error = r$error %||% "Unknown error from Python service"
          )
        }
      }
      
      results
    },
    error = function(e) {
      # Return all models as failed
      lapply(models, function(key) {
        list(
          success = FALSE,
          key = key,
          label = TS_MODELS[[key]]$label,
          error = sprintf("Python service error: %s", conditionMessage(e))
        )
      })
    }
  )
}

#' Convert time-series regression predictions to classification records.
#'
#' Time-series models predict returns (regression). For integration with the
#' existing ML Compare UI (which expects BUY/HOLD/SELL classes), we convert
#' predicted returns to classes using thresholds.
#'
#' @param predictions Numeric vector of predicted returns.
#' @param dates Date vector aligned with predictions.
#' @param ticker Ticker symbol.
#' @param buy_threshold Threshold for BUY (default 0.02).
#' @param sell_threshold Threshold for SELL (default -0.02).
#' @return data.frame with model_name, ticker, date, predicted_class,
#'   prob_buy, prob_hold, prob_sell.
ts_predictions_to_records <- function(predictions, dates, ticker,
                                      buy_threshold = 0.02,
                                      sell_threshold = 0.02) {
  if (is.null(predictions) || length(predictions) == 0) {
    return(data.frame(
      model_name = character(0),
      ticker = character(0),
      date = as.Date(character(0)),
      predicted_class = character(0),
      prob_buy = numeric(0),
      prob_hold = numeric(0),
      prob_sell = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  n <- length(predictions)
  
  # Convert predictions to class probabilities
  # Use a simple softmax-like transformation based on distance from thresholds
  pred <- as.numeric(predictions)
  
  # Compute pseudo-probabilities
  prob_buy <- pmax(0, (pred - 0) / buy_threshold)
  prob_sell <- pmax(0, (0 - pred) / abs(sell_threshold))
  prob_hold <- 1 - pmin(1, prob_buy + prob_sell)
  
  # Normalize
  total <- prob_buy + prob_hold + prob_sell
  total[total == 0] <- 1
  prob_buy <- prob_buy / total
  prob_hold <- prob_hold / total
  prob_sell <- prob_sell / total
  
  # Determine predicted class
  predicted_class <- ifelse(pred > buy_threshold, "BUY",
                            ifelse(pred < sell_threshold, "SELL", "HOLD"))
  
  data.frame(
    model_name = "ts_model",
    ticker = ticker,
    date = as.Date(dates),
    predicted_class = predicted_class,
    prob_buy = prob_buy,
    prob_hold = prob_hold,
    prob_sell = prob_sell,
    stringsAsFactors = FALSE
  )
}

#' Evaluate time-series model predictions through the existing backtest engine.
#'
#' @param predictions Numeric vector of predicted returns.
#' @param y_test Actual returns for the test period.
#' @param ohlcv_test OHLCV xts for the test period.
#' @param dates_test Dates for the test period.
#' @param ticker Ticker symbol.
#' @param initial_capital Initial capital.
#' @param signal_mode "regression" (trade on predicted direction) or "classes".
#' @param buy_threshold Threshold for BUY signal.
#' @param sell_threshold Threshold for SELL signal.
#' @return List with classification (regression metrics) and backtest results.
evaluate_ts_predictions <- function(predictions, y_test, ohlcv_test, dates_test, ticker,
                                    initial_capital = 10000,
                                    signal_mode = "regression",
                                    buy_threshold = 0.02,
                                    sell_threshold = 0.02) {
  
  # Regression metrics
  y_true <- as.numeric(y_test)
  y_pred <- as.numeric(predictions)
  
  valid <- is.finite(y_true) & is.finite(y_pred)
  
  if (sum(valid) < 2) {
    regression_metrics <- list(
      rmse = NA_real_,
      mae = NA_real_,
      r2 = NA_real_,
      ic = NA_real_,
      directional_accuracy = NA_real_,
      n_eval = sum(valid)
    )
  } else {
    y_true_v <- y_true[valid]
    y_pred_v <- y_pred[valid]
    
    rmse <- sqrt(mean((y_true_v - y_pred_v)^2))
    mae <- mean(abs(y_true_v - y_pred_v))
    
    ss_res <- sum((y_true_v - y_pred_v)^2)
    ss_tot <- sum((y_true_v - mean(y_true_v))^2)
    r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
    
    # Information Coefficient (rank correlation)
    ic <- NA_real_
    if (length(y_true_v) >= 5) {
      tryCatch({
        ic <- cor(rank(y_true_v), rank(y_pred_v), method = "spearman")
      }, error = function(e) NA_real_)
    }
    
    # Directional accuracy
    true_dir <- sign(y_true_v)
    pred_dir <- sign(y_pred_v)
    non_zero <- pred_dir != 0
    dir_acc <- if (sum(non_zero) > 0) {
      mean(true_dir[non_zero] == pred_dir[non_zero])
    } else {
      NA_real_
    }
    
    regression_metrics <- list(
      rmse = rmse,
      mae = mae,
      r2 = r2,
      ic = ic,
      directional_accuracy = dir_acc,
      n_eval = sum(valid)
    )
  }
  
  # Convert predictions to signals for backtest
  # Use predicted direction for trading
  signals <- rep(NA_character_, length(predictions))
  signals[predictions > buy_threshold] <- "BUY"
  signals[predictions < sell_threshold] <- "SELL"
  
  sig_xts <- xts::xts(signals, order.by = zoo::index(ohlcv_test))
  
  # Run backtest through existing engine
  bt <- tryCatch(
    run_backtest(
      ohlcv = ohlcv_test,
      signals = sig_xts,
      initial_capital = initial_capital,
      start_date = utils::head(zoo::index(ohlcv_test), 1),
      end_date = utils::tail(zoo::index(ohlcv_test), 1),
      ticker = ticker,
      strategy = "TS_Regression",
      parameters = list(buy_threshold = buy_threshold, sell_threshold = sell_threshold)
    ),
    error = function(e) {
      list(summary = list(
        cagr = NA_real_,
        sharpe = NA_real_,
        sortino = NA_real_,
        maximum_drawdown = NA_real_,
        win_rate = NA_real_,
        total_trades = 0L
      ))
    }
  )
  
  list(
    regression = regression_metrics,
    backtest = bt
  )
}

#' Check if a model key refers to a Python time-series model.
#'
#' Python TS models are prefixed with "ts_" in the unified MODEL_KEYS registry
#' (e.g., ts_zero_return, ts_lstm, ts_random_forest) to distinguish them from
#' R classification models with similar names (e.g., lstm, random_forest).
#'
#' @param key Model key.
#' @return TRUE if it's a Python time-series model, FALSE otherwise.
is_ts_model <- function(key) {
  # Python TS models are identified by the "ts_" prefix in the unified registry
  grepl("^ts_", key)
}

#' Resolve a model key, handling both R and Python models.
#'
#' @param model Raw model identifier.
#' @return The canonical model key.
resolve_any_model_key <- function(model) {
  key <- tolower(trimws(as.character(model)))
  key <- gsub("[^a-z0-9_]", "_", key)
  key <- gsub("_+", "_", key)
  key <- sub("^_", "", key)
  key <- sub("_$", "", key)
  
  # Check R models first
  if (key %in% MODEL_KEYS) {
    return(list(key = key, backend = "R"))
  }
  
  # Check Python TS models
  if (key %in% TS_MODEL_KEYS) {
    return(list(key = key, backend = "python"))
  }
  
  stop(validation_error(sprintf(
    "Unknown model '%s'. Supported R models: %s. Python TS models: %s.",
    model, paste(MODEL_KEYS, collapse = ", "), paste(TS_MODEL_KEYS, collapse = ", ")
  )))
}
