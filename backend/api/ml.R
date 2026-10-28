# AlphaForge Insight — ML API handlers (ML Phases G/H)
#
# Handlers for the five new endpoints. They follow the EXISTING backend
# conventions exactly (see backend/api/backtest.R):
#   - JWT via require_auth(req) as the FIRST step of every protected handler
#   - validation_error -> 400, data_error -> e$status, error -> 500 (safe msg)
#   - plain-R list bodies; NA removed via na_to_null (backend/api/backtest.R)
#
#   POST /ml/features   Feature matrix for a ticker/date range
#   POST /ml/train      Train ONE model -> metrics + metadata (+ persist)
#   POST /ml/compare    Train ALL requested models -> comparison table
#   POST /ml/predict    Latest standardized predictions + P(BUY) ranking
#   POST /ml/backtest   Top-N portfolio backtest via the EXISTING engine

# ---------------------------------------------------------------------------
# Request-parsing helpers (shared by all /ml handlers)
# ---------------------------------------------------------------------------

.parse_json_body <- function(req) {
  body <- req$body
  if (is.null(body) || !is.list(body)) {
    stop(validation_error("Request body is required and must be JSON."))
  }
  body
}

.parse_ml_common <- function(body) {
  ticker <- body$ticker
  if (is.null(ticker) || !nzchar(trimws(as.character(ticker)))) {
    stop(validation_error("ticker is required."))
  }
  start <- body$start; end <- body$end
  if (is.null(start) || is.null(end)) {
    stop(validation_error("start and end dates are required (YYYY-MM-DD)."))
  }
  dates <- validate_date_range(start, end)
  list(ticker = toupper(trimws(as.character(ticker))),
       start = format(dates$start, "%Y-%m-%d"),
       end = format(dates$end, "%Y-%m-%d"))
}

.parse_label_config_body <- function(body) {
  validate_label_config(
    if (is.null(body$horizon)) LABEL_DEFAULTS$horizon else body$horizon,
    if (is.null(body$buy_threshold)) LABEL_DEFAULTS$buy_threshold else body$buy_threshold,
    if (is.null(body$sell_threshold)) LABEL_DEFAULTS$sell_threshold else body$sell_threshold
  )
}

#' Dataset assembly + caching keyed by (ticker, range, labels, groups, split).
.ml_dataset_cache <- new.env(parent = emptyenv())

.get_or_build_dataset <- function(ticker, start, end, horizon,
                                  buy_threshold, sell_threshold, groups,
                                  split_fractions) {
  key <- paste(ticker, start, end, horizon, buy_threshold, sell_threshold,
               paste(groups, collapse = ","), sep = "|")
  cached <- .ml_dataset_cache[[key]]
  if (!is.null(cached)) return(cached)
  ds <- assemble_ml_dataset(ohlcv = NULL, ticker = ticker, start = start, end = end,
                            horizon = horizon, buy_threshold = buy_threshold,
                            sell_threshold = sell_threshold, groups = groups,
                            split_fractions = split_fractions)
  ds$ticker <- ticker
  .ml_dataset_cache[[key]] <- ds
  ds
}

# Serialize prediction records for JSON.
.prediction_records_to_list <- function(recs, max_rows = NULL) {
  if (is.null(recs) || nrow(recs) == 0) return(list())
  n <- nrow(recs)
  if (!is.null(max_rows) && n > max_rows) {
    keep_from <- n - max_rows + 1L
  } else {
    keep_from <- 1L
  }
  lapply(keep_from:n, function(i) {
    list(
      model_name = recs$model_name[i],
      ticker = recs$ticker[i],
      date = format(as.Date(recs$date[i]), "%Y-%m-%d"),
      predicted_class = recs$predicted_class[i],
      prob_buy = recs$prob_buy[i],
      prob_hold = recs$prob_hold[i],
      prob_sell = recs$prob_sell[i]
    )
  })
}

# Split the comparison table into JSON rows.
.comparison_to_list <- function(comparison) {
  if (is.null(comparison) || nrow(comparison) == 0) return(list())
  lapply(seq_len(nrow(comparison)), function(i) {
    row <- as.list(comparison[i, , drop = FALSE])
    lapply(row, function(v) if (length(v) == 1) v[[1]] else v)
  })
}

#' Safely format a scalar/vector as "YYYY-MM-DD".
#'
#' format(x, "%Y-%m-%d") throws R's opaque "invalid 'trim' argument" when x is
#' numeric/integer (e.g. a Date scalar that lost its class inside a list or a
#' 0-row data.frame slice). Converting with as.Date() first keeps the API
#' contract (ISO date strings) and turns that crash into a clean value.
.format_iso_date <- function(x) {
  format(as.Date(x), "%Y-%m-%d")
}

# ---------------------------------------------------------------------------
# POST /ml/features
# ---------------------------------------------------------------------------

handle_ml_features <- function(req) {
  auth <- require_auth(req)
  if (!auth$ok) return(list(status = auth$status, body = auth$body))

  tryCatch(
    {
      body <- .parse_json_body(req)
      common <- .parse_ml_common(body)
      groups <- if (is.null(body$feature_groups)) NULL else as.character(body$feature_groups)

      ohlcv <- fetch_ohlcv(common$ticker, common$start, common$end)
      feats <- build_features(ohlcv, groups)
      m <- as.matrix(feats$features)
      idx <- as.Date(zoo::index(feats$features))

      rows <- lapply(seq_len(nrow(m)), function(i) {
        list(date = format(idx[i], "%Y-%m-%d"),
             values = as.list(stats::setNames(as.numeric(m[i, ]), colnames(m))))
      })

      list(status = 200L, body = list(
        success = TRUE,
        ticker = common$ticker,
        start = common$start,
        end = common$end,
        groups = feats$groups,
        feature_names = feats$feature_names,
        n_rows = length(rows),
        n_features = length(feats$feature_names),
        features = rows
      ))
    },
    validation_error = function(e) list(status = 400L, body = list(success = FALSE, error = conditionMessage(e))),
    data_error = function(e) list(status = e$status, body = list(success = FALSE, error = conditionMessage(e))),
    error = function(e) {
      cat("[ml/features] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(success = FALSE,
           error = "An unexpected server error occurred while building features."))
    }
  )
}

# ---------------------------------------------------------------------------
# POST /ml/train
# ---------------------------------------------------------------------------

handle_ml_train <- function(req) {
  auth <- require_auth(req)
  if (!auth$ok) return(list(status = auth$status, body = auth$body))
  user_id <- auth$claims$sub

  tryCatch(
    {
      body <- .parse_json_body(req)
      common <- .parse_ml_common(body)
      label_cfg <- .parse_label_config_body(body)
      key <- resolve_model_key(body$model)
      hp <- resolve_hyperparams(key, body$hyperparameters)
      groups <- if (is.null(body$feature_groups)) NULL else as.character(body$feature_groups)
      evaluate_on <- if (identical(body$evaluate_on, "validation")) "validation" else "test"
      tune <- isTRUE(body$tune)
      initial_capital <- if (is.null(body$initialCapital)) 10000 else {
        cap <- suppressWarnings(as.numeric(body$initialCapital))
        if (is.na(cap) || cap <= 0) stop(validation_error("initialCapital must be > 0."))
        cap
      }

      ds <- .get_or_build_dataset(common$ticker, common$start, common$end,
                                  label_cfg$horizon, label_cfg$buy_threshold,
                                  label_cfg$sell_threshold, groups,
                                  ML_SPLIT_DEFAULTS)

      is_ts <- is_ts_model(key)
      
      if (is_ts) {
        result <- evaluate_ts_model(ds, key, hp, evaluate_on = evaluate_on,
                                    initial_capital = initial_capital,
                                    buy_threshold = label_cfg$buy_threshold,
                                    sell_threshold = abs(label_cfg$sell_threshold))
      } else {
        result <- evaluate_model(ds, key, hp, evaluate_on = evaluate_on,
                                 initial_capital = initial_capital,
                                 signal_mode = body$signal_mode %||% "classes",
                                 prob_threshold = as.numeric(body$prob_threshold %||% 0.5),
                                 tune = tune)
      }
      
      if (!isTRUE(result$success)) {
        return(list(status = 400L, body = list(
          success = FALSE, model = key, error = result$error
        )))
      }

      model_id <- NULL
      con <- tryCatch(get_db_connection(), error = function(e) {
        cat("[ml/train] DB connection failed:", conditionMessage(e), "\n")
        NULL
      })
      if (!is.null(con)) {
        model_id <- tryCatch(
          ml_persist_model(con, user_id, result, ds, body),
          error = function(e) {
            cat("[ml/train] persistence error:", conditionMessage(e), "\n")
            NULL
          }
        )
        close_db_connection(con)
      }

      bt_summary <- result$backtest$summary
      
      if (is_ts) {
        list(status = 200L, body = list(
          success = TRUE,
          model_id = model_id,
          model = key,
          model_name = result$label,
          ticker = common$ticker,
          start = common$start,
          end = common$end,
          label_config = label_cfg,
          evaluate_on = evaluate_on,
          hyperparameters = result$hyperparams,
          train_info = result$train_info,
          feature_importance = result$feature_importance,
          regression_metrics = result$regression,
          backtest = list(
            engine = "existing t+1-open engine (run_backtest)",
            metrics = metrics_to_list(bt_summary)
          ),
          predictions = .prediction_records_to_list(result$predictions, 60)
        ))
      } else {
        list(status = 200L, body = list(
          success = TRUE,
          model_id = model_id,
          model = key,
          model_name = result$label,
          ticker = common$ticker,
          start = common$start,
          end = common$end,
          label_config = label_cfg,
          evaluate_on = evaluate_on,
          hyperparameters = result$hyperparams,
          train_info = result$train_info,
          feature_importance = result$feature_importance,
          validation_metrics = evaluation_to_list(result$classification),
          backtest = list(
            engine = "existing t+1-open engine (run_backtest)",
            metrics = metrics_to_list(bt_summary)
          ),
          predictions = .prediction_records_to_list(result$predictions, 60)
        ))
      }
    },
    validation_error = function(e) list(status = 400L, body = list(success = FALSE, error = conditionMessage(e))),
    data_error = function(e) list(status = e$status, body = list(success = FALSE, error = conditionMessage(e))),
    error = function(e) {
      cat("[ml/train] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(success = FALSE,
           error = "An unexpected server error occurred during model training."))
    }
  )
}

# ---------------------------------------------------------------------------
# POST /ml/compare
# ---------------------------------------------------------------------------

handle_ml_compare <- function(req) {
  auth <- require_auth(req)
  if (!auth$ok) return(list(status = auth$status, body = auth$body))

  tryCatch(
    {
      body <- .parse_json_body(req)
      common <- .parse_ml_common(body)
      label_cfg <- .parse_label_config_body(body)
      models <- if (is.null(body$models)) NULL else as.character(body$models)
      # Validate model keys up front so typos are 400s, not per-model errors.
      if (!is.null(models)) {
        lapply(models, resolve_model_key)
      }
      groups <- if (is.null(body$feature_groups)) NULL else as.character(body$feature_groups)
      hyperparams <- if (is.null(body$hyperparameters)) list() else body$hyperparameters
      evaluate_on <- if (identical(body$evaluate_on, "validation")) "validation" else "test"

      ds <- .get_or_build_dataset(common$ticker, common$start, common$end,
                                  label_cfg$horizon, label_cfg$buy_threshold,
                                  label_cfg$sell_threshold, groups,
                                  ML_SPLIT_DEFAULTS)

      comparison_run <- run_model_comparison(
        ds, models, hyperparams,
        evaluate_on = evaluate_on,
        initial_capital = if (is.null(body$initialCapital)) 10000 else as.numeric(body$initialCapital),
        signal_mode = body$signal_mode %||% "classes",
        prob_threshold = as.numeric(body$prob_threshold %||% 0.5)
      )

      ok_rows <- Filter(function(r) identical(r$Status, "ok"),
                        split(comparison_run$comparison, seq_len(nrow(comparison_run$comparison))))
      best_by_metric <- NULL
      if (length(ok_rows) > 0) {
        has_f1 <- any(!is.na(sapply(ok_rows, function(r) r$F1[[1]])))
        has_sharpe <- any(!is.na(sapply(ok_rows, function(r) r$Sharpe[[1]])))
        
        if (has_f1) {
          f1s <- vapply(ok_rows, function(r) r$F1[[1]], numeric(1))
          f1s[is.na(f1s)] <- -Inf
          best_by_metric <- ok_rows[[which.max(f1s)]]$Model[[1]]
        } else if (has_sharpe) {
          sharpes <- vapply(ok_rows, function(r) r$Sharpe[[1]], numeric(1))
          sharpes[is.na(sharpes)] <- -Inf
          best_by_metric <- ok_rows[[which.max(sharpes)]]$Model[[1]]
        }
      }

      list(status = 200L, body = list(
        success = TRUE,
        ticker = common$ticker,
        start = common$start,
        end = common$end,
        label_config = label_cfg,
        evaluate_on = evaluate_on,
        models_run = length(comparison_run$comparison$Model),
        models_failed = sum(comparison_run$comparison$Status == "failed"),
        best_by_metric = best_by_metric,
        comparison = .comparison_to_list(comparison_run$comparison),
        per_model = lapply(comparison_run$results, function(r) {
          if (!isTRUE(r$success)) return(NULL)
          is_ts <- is_ts_model(r$key)
          if (is_ts) {
            list(
              model = r$key,
              model_name = r$label,
              regression_metrics = r$regression,
              backtest = list(
                engine = "existing t+1-open engine (run_backtest)",
                metrics = metrics_to_list(r$backtest$summary)
              )
            )
          } else {
            list(
              model = r$key,
              model_name = r$label,
              validation_metrics = evaluation_to_list(r$classification),
              backtest = list(
                engine = "existing t+1-open engine (run_backtest)",
                metrics = metrics_to_list(r$backtest$summary)
              )
            )
          }
        }),
        failures = Filter(function(r) identical(r$Status, "failed"),
                          .comparison_to_list(comparison_run$comparison))
      ))
    },
    validation_error = function(e) list(status = 400L, body = list(success = FALSE, error = conditionMessage(e))),
    data_error = function(e) list(status = e$status, body = list(success = FALSE, error = conditionMessage(e))),
    error = function(e) {
      cat("[ml/compare] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(success = FALSE,
           error = "An unexpected server error occurred during model comparison."))
    }
  )
}

# ---------------------------------------------------------------------------
# POST /ml/predict
# ---------------------------------------------------------------------------

handle_ml_predict <- function(req) {
  auth <- require_auth(req)
  if (!auth$ok) return(list(status = auth$status, body = auth$body))

  tryCatch(
    {
      body <- .parse_json_body(req)
      common <- .parse_ml_common(body)
      label_cfg <- .parse_label_config_body(body)
      key <- resolve_model_key(body$model)
      hp <- resolve_hyperparams(key, body$hyperparameters)
      groups <- if (is.null(body$feature_groups)) NULL else as.character(body$feature_groups)

      ds <- .get_or_build_dataset(common$ticker, common$start, common$end,
                                  label_cfg$horizon, label_cfg$buy_threshold,
                                  label_cfg$sell_threshold, groups,
                                  ML_SPLIT_DEFAULTS)
      inputs <- prepare_model_inputs(ds, key, hp)
      handle <- if (key == "lstm") {
        train_lstm_model(inputs$X_train, inputs$y_train, hp,
                         inputs$X_val, inputs$y_val)
      } else {
        train_tabular_model(key, inputs$X_train, inputs$y_train, hp)
      }

      P_test <- handle$predict(inputs$X_test)
      recs <- prediction_records(key, common$ticker, inputs$dates_test, P_test)
      latest <- recs[nrow(recs), , drop = FALSE]

      ranked <- rank_stocks_by_prob_buy(
        stats::setNames(list(recs), common$ticker),
        latest$date, top_n = 1
      )

      list(status = 200L, body = list(
        success = TRUE,
        model = key,
        model_name = ML_MODELS[[key]]$label,
        ticker = common$ticker,
        as_of = format(latest$date[[1]], "%Y-%m-%d"),
        prediction = list(
          ticker = common$ticker,
          date = format(latest$date[[1]], "%Y-%m-%d"),
          prediction = latest$predicted_class[[1]],
          prob_buy = latest$prob_buy[[1]],
          prob_hold = latest$prob_hold[[1]],
          prob_sell = latest$prob_sell[[1]]
        ),
        ranking = lapply(seq_len(nrow(ranked)), function(i) {
          list(ticker = ranked$ticker[i], prob_buy = ranked$prob_buy[i],
               rank = ranked$rank[i])
        })
      ))
    },
    validation_error = function(e) list(status = 400L, body = list(success = FALSE, error = conditionMessage(e))),
    data_error = function(e) list(status = e$status, body = list(success = FALSE, error = conditionMessage(e))),
    error = function(e) {
      cat("[ml/predict] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(success = FALSE,
           error = "An unexpected server error occurred while generating predictions."))
    }
  )
}

# ---------------------------------------------------------------------------
# POST /ml/backtest
# ---------------------------------------------------------------------------

handle_ml_backtest <- function(req) {
  auth <- require_auth(req)
  if (!auth$ok) return(list(status = auth$status, body = auth$body))
  user_id <- auth$claims$sub

  tryCatch(
    {
      body <- .parse_json_body(req)
      tickers <- body$tickers
      if (is.null(tickers) || length(tickers) == 0) {
        stop(validation_error("tickers must be a non-empty array."))
      }
      tickers <- toupper(trimws(as.character(tickers)))
      if (length(tickers) > 10) {
        stop(validation_error("A maximum of 10 tickers is supported per ML backtest."))
      }
      if (anyDuplicated(tickers)) {
        stop(validation_error("Duplicate tickers are not allowed."))
      }
      common <- .parse_ml_common(body)
      label_cfg <- .parse_label_config_body(body)
      key <- resolve_model_key(body$model)
      hp <- resolve_hyperparams(key, body$hyperparameters)
      groups <- if (is.null(body$feature_groups)) NULL else as.character(body$feature_groups)
      top_n <- if (is.null(body$top_n)) 5 else body$top_n
      rebalance <- if (is.null(body$rebalance)) "weekly" else body$rebalance
      initial_capital <- if (is.null(body$initialCapital)) 10000 else {
        cap <- suppressWarnings(as.numeric(body$initialCapital))
        if (is.na(cap) || cap <= 0) stop(validation_error("initialCapital must be > 0."))
        cap
      }
      signal_mode <- body$signal_mode %||% "classes"
      prob_threshold <- as.numeric(body$prob_threshold %||% 0.5)

      market_data <- list()
      prediction_frames <- list()
      eval_metrics <- list()
      for (t in tickers) {
        ds <- .get_or_build_dataset(t, common$start, common$end,
                                    label_cfg$horizon, label_cfg$buy_threshold,
                                    label_cfg$sell_threshold, groups,
                                    ML_SPLIT_DEFAULTS)
        inputs <- prepare_model_inputs(ds, key, hp)
        handle <- if (key == "lstm") {
          train_lstm_model(inputs$X_train, inputs$y_train, hp,
                           inputs$X_val, inputs$y_val)
        } else {
          train_tabular_model(key, inputs$X_train, inputs$y_train, hp)
        }
        P <- handle$predict(inputs$X_test)
        recs <- prediction_records(key, t, inputs$dates_test, P)
        prediction_frames[[t]] <- recs
        market_data[[t]] <- ds$ohlcv
        cls <- evaluate_classification(recs, inputs$y_test)
        eval_metrics[[t]] <- evaluation_to_list(cls, include_curves = FALSE)
      }

      # The traded window is the intersection of every ticker's test period.
      test_dates <- lapply(prediction_frames, function(r) as.Date(r$date))
      shared <- Reduce(intersect, test_dates)
      if (length(shared) < 5) {
        stop(validation_error(
          "The tickers' test periods do not overlap enough (need >= 5 shared trading days); use identical date ranges."
        ))
      }

      portfolio_run <- run_topn_portfolio_backtest(
        market_data, prediction_frames, shared, top_n, rebalance,
        initial_capital, signal_mode, prob_threshold
      )

      # Persist (best-effort) through the existing backtests/results tables
      # so the run shows up in history; per-user isolation applies.
      backtest_id <- NULL
      con <- tryCatch(get_db_connection(), error = function(e) {
        cat("[ml/backtest] DB connection failed:", conditionMessage(e), "\n")
        NULL
      })
      if (!is.null(con)) {
        backtest_id <- tryCatch({
          params <- list(
            model = key, hyperparameters = hp, top_n = portfolio_run$config$top_n,
            rebalance_days = portfolio_run$config$rebalance_days,
            signal_mode = signal_mode, prob_threshold = prob_threshold,
            tickers = tickers, label_config = label_cfg
          )
          bt <- insert_backtest(con, user_id, tickers[[1]], "ML_Strategy",
                                common$start, common$end, initial_capital, params)
          ec <- portfolio_run$portfolio$equity_curve
          ec_list <- lapply(seq_len(nrow(ec)), function(i) list(
            date = .format_iso_date(ec$date[i]),
            portfolioValue = ec$portfolio_value[i]
          ))
          # per_ticker entries are build_backtest_result() objects: the trade
          # history lives at $trades$history. Serialize with the SAME helper
          # the regular /backtest/run persistence uses so the Results
          # dashboard contract stays identical.
          trades_list <- lapply(names(portfolio_run$per_ticker), function(t) {
            lapply(trade_history_to_list(portfolio_run$per_ticker[[t]]$trades$history),
                   function(row) c(list(ticker = t), row))
          })
          trades_flat <- if (length(trades_list) > 0) unlist(trades_list, recursive = FALSE) else list()
          insert_result(con, bt$id, portfolio_run$portfolio$metrics,
                        jsonlite::toJSON(ec_list, auto_unbox = TRUE),
                        jsonlite::toJSON(trades_flat, auto_unbox = TRUE))
          bt$id
        }, error = function(e) {
          cat("[ml/backtest] persistence error:", conditionMessage(e), "\n")
          NULL
        })
        close_db_connection(con)
      }

      list(status = 200L, body = list(
        success = TRUE,
        backtestId = backtest_id,
        model = key,
        model_name = ML_MODELS[[key]]$label,
        tickers = tickers,
        start = common$start,
        end = common$end,
        config = portfolio_run$config,
        classification_metrics = eval_metrics,
        rebalances = lapply(portfolio_run$rebalances, function(rb) {
          list(date = .format_iso_date(rb$date),
               holdings = lapply(seq_len(nrow(rb$holdings)), function(i) {
                 list(ticker = rb$holdings$ticker[i],
                      prob_buy = rb$holdings$prob_buy[i],
                      rank = rb$holdings$rank[i])
               }))
        }),
        portfolio = list(
          metrics = metrics_to_list(portfolio_run$portfolio$metrics),
          equityCurve = equity_curve_to_list(portfolio_run$portfolio$equity_curve)
        ),
        perTicker = lapply(names(portfolio_run$per_ticker), function(t) {
          list(
            ticker = t,
            metrics = metrics_to_list(portfolio_run$per_ticker[[t]]$summary),
            tradeHistory = trade_history_to_list(portfolio_run$per_ticker[[t]]$trades$history)
          )
        })
      ))
    },
    validation_error = function(e) list(status = 400L, body = list(success = FALSE, error = conditionMessage(e))),
    data_error = function(e) list(status = e$status, body = list(success = FALSE, error = conditionMessage(e))),
    error = function(e) {
      cat("[ml/backtest] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(success = FALSE,
           error = "An unexpected server error occurred during the ML backtest."))
    }
  )
}
