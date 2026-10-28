# AlphaForge Insight — ML persistence (ML Phase H)
#
# Additive storage for trained models and training runs:
#   ml_models — one row per trained model (metadata + hyperparameters +
#               train/val/test metrics) so predictions can be re-associated
#               with the model that produced them.
#   ml_runs   — one row per /ml/compare execution (the unified comparison
#               table) for the DA2 comparative-analysis requirement.
#
# All SQL is parameterised (project convention). Persistence is best-effort:
# the API handlers never fail a request because persistence failed.

library(DBI)

#' Persist one trained model from an evaluate_model() result.
#'
#' @param con DBI connection.
#' @param user_id Owning user.
#' @param result Successful evaluate_model() output.
#' @param ds The assemble_ml_dataset() output used for training.
#' @param request_body Raw request (for provenance fields).
#' @return The new ml_models.id (integer).
ml_persist_model <- function(con, user_id, result, ds, request_body = list()) {
  cls <- result$classification
  m <- result$backtest$summary
  hp_json <- jsonlite::toJSON(result$hyperparams %||% list(), auto_unbox = TRUE)
  imp_json <- if (is.null(result$feature_importance)) NA_character_ else
    jsonlite::toJSON(as.list(result$feature_importance), auto_unbox = TRUE)
  ti <- result$train_info %||% list()
  feat_json <- jsonlite::toJSON(ti$features_used %||% list(), auto_unbox = TRUE)

  query <- "
    INSERT INTO ml_models
      (user_id, model_key, model_name, ticker, start_date, end_date,
       horizon, buy_threshold, sell_threshold, hyperparameters,
       n_train, n_validation, n_test, features_used, feature_importance,
       accuracy, precision_macro, recall_macro, f1_macro, roc_auc_macro,
       total_return, cagr, sharpe, sortino, max_drawdown, win_rate,
       volatility, total_trades)
    VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,
            $16,$17,$18,$19,$20,$21,$22,$23,$24,$25,$26,$27,$28)
    RETURNING id
  "
  res <- dbGetQuery(con, query, params = list(
    as.integer(user_id),
    result$key, result$label, ds$ticker,
    format(min(ds$dates), "%Y-%m-%d"), format(max(ds$dates), "%Y-%m-%d"),
    as.integer(ds$horizon),
    ds$label_config$buy_threshold, ds$label_config$sell_threshold,
    hp_json,
    as.integer(ti$n_train %||% NA_integer_),
    as.integer(ti$n_validation %||% NA_integer_),
    as.integer(ti$n_test %||% NA_integer_),
    feat_json, imp_json,
    cls$accuracy, cls$precision, cls$recall, cls$f1, cls$roc_auc_macro,
    m$total_return, m$cagr, m$sharpe, m$sortino, m$maximum_drawdown,
    m$win_rate, m$volatility, as.integer(m$total_trades)
  ))
  as.integer(res$id[[1]])
}

#' Persist a model-comparison run.
#'
#' @param con DBI connection.
#' @param user_id Owning user.
#' @param comparison data.frame from run_model_comparison().
#' @param request_body Raw request for provenance.
#' @return The new ml_runs.id (integer), or NULL when the comparison is empty.
ml_persist_run <- function(con, user_id, comparison, request_body = list()) {
  if (is.null(comparison) || nrow(comparison) == 0) return(NULL)
  query <- "
    INSERT INTO ml_runs (user_id, tickers, model_keys, comparison)
    VALUES ($1, $2, $3, $4)
    RETURNING id
  "
  tickers <- request_body$ticker %||% NA_character_
  keys <- if (is.null(comparison$Model)) NA_character_ else
    jsonlite::toJSON(as.character(comparison$Model), auto_unbox = FALSE)
  res <- dbGetQuery(con, query, params = list(
    as.integer(user_id),
    as.character(tickers),
    keys,
    jsonlite::toJSON(comparison, auto_unbox = TRUE)
  ))
  as.integer(res$id[[1]])
}

#' Fetch recent ML runs for a user (dashboard listing hook).
#'
#' @param con DBI connection. @param user_id Owning user.
#' @param limit Max rows (default 20).
#' @return data.frame of ml_runs rows (id, created_at, tickers).
ml_get_runs <- function(con, user_id, limit = 20L) {
  dbGetQuery(con, "
    SELECT id, tickers, model_keys, created_at
    FROM ml_runs WHERE user_id = $1
    ORDER BY created_at DESC LIMIT $2
  ", params = list(as.integer(user_id), as.integer(limit)))
}
