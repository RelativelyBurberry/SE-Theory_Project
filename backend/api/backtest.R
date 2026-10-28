# AlphaForge Insight — backtest API handlers (Phase 6)
#
# Endpoints:
#   POST /backtest/run         Run a full backtest (auth -> validate -> fetch ->
#                              indicators -> signals -> execute -> metrics ->
#                              persist -> JSON response)
#   GET  /backtest/history     Paginated list of the user's past backtests
#   GET  /backtest/:id         Full backtest record with metrics + data
#
# All endpoints are JWT-protected with user isolation (WHERE user_id = ?).
#
# Serialization strategy (Phase 6 decision):
#   - Convert all xts/data.frame objects to plain R lists BEFORE returning.
#   - Replace R NA values with JSON-null-compatible values at the application
#     level using na_to_null() — no custom Plumber serializer needed.
#   - Use the existing @serializer unboxedJSON annotation (project convention).

# ---------------------------------------------------------------------------
# Application-level NA-to-null conversion
# ---------------------------------------------------------------------------
#' Recursively replace NA values with NULL in nested R lists.
#'
#' This allows Plumber's unboxedJSON serializer to produce JSON null for
#' missing values instead of the string "NA". Works on nested lists and
#' vectors. Data frames (e.g. from jsonlite::fromJSON on a JSONB array of
#' objects) are first converted to a LIST OF ROWS so the JSON output stays an
#' array of objects, never a column-oriented object.
#'
#' @param x An R object (list, vector, data.frame, scalar).
#' @return The same structure with NA replaced by NULL (data.frames become
#'   row-lists).
na_to_null <- function(x) {
  if (is.data.frame(x)) {
    if (nrow(x) == 0) return(list())
    return(lapply(seq_len(nrow(x)), function(i) {
      row <- as.list(x[i, , drop = FALSE])  # one list per row (length-1 cols)
      na_to_null(row)
    }))
  }
  if (is.list(x)) {
    return(lapply(x, na_to_null))
  }
  if (length(x) == 1L && is.na(x)) {
    return(NULL)
  }
  x
}

# ---------------------------------------------------------------------------
# Equity curve / trade history serialization helpers
# ---------------------------------------------------------------------------
#' Convert an equity-curve data.frame to a plain list of lists.
#'
#' @param ec data.frame with date, cash, shares, close, portfolio_value,
#'   daily_return columns.
#' @return A list of named lists (one per row), dates as "YYYY-MM-DD" strings.
equity_curve_to_list <- function(ec) {
  if (is.null(ec) || nrow(ec) == 0) return(list())
  lapply(seq_len(nrow(ec)), function(i) {
    row <- list(
      date = format(ec$date[i], "%Y-%m-%d"),
      portfolioValue = ec$portfolio_value[i],
      cash = ec$cash[i],
      shares = ec$shares[i],
      close = ec$close[i]
    )
    # daily_return: first row is NA by design — omit the key entirely
    if (!is.na(ec$daily_return[i])) {
      row$dailyReturn <- ec$daily_return[i]
    }
    row
  })
}

#' Convert a trade-history data.frame to a plain list of lists.
#'
#' @param th data.frame from execute_backtest()$trade_history.
#' @return A list of named lists (one per trade), dates as strings.
trade_history_to_list <- function(th) {
  if (is.null(th) || nrow(th) == 0) return(list())
  lapply(seq_len(nrow(th)), function(i) {
    list(
      tradeId = th$trade_id[i],
      signalDate = format(th$signal_date[i], "%Y-%m-%d"),
      executionDate = format(th$execution_date[i], "%Y-%m-%d"),
      side = th$side[i],
      executionPrice = th$execution_price[i],
      shares = th$shares[i],
      cost = th$cost[i],
      cashAfter = th$cash_after[i],
      positionAfter = th$position_after[i]
    )
  })
}

#' Convert metrics to a camelCase named list with NA -> NULL.
#'
#' @param summary Named list from compute_performance_metrics().
#' @return Named list in camelCase, ready for JSON serialization.
metrics_to_list <- function(summary) {
  na_to_null(list(
    initialCapital = summary$initial_capital,
    finalValue = summary$final_value,
    totalReturn = summary$total_return,
    cagr = summary$cagr,
    sharpe = summary$sharpe,
    sortino = summary$sortino,
    maximumDrawdown = summary$maximum_drawdown,
    winRate = summary$win_rate,
    volatility = summary$volatility,
    totalTrades = summary$total_trades
  ))
}

# ---------------------------------------------------------------------------
# POST /backtest/run handler
# ---------------------------------------------------------------------------
#' Run a full backtest.
#'
#' Flow: auth -> validate -> fetch OHLCV -> indicators -> signals ->
#'       execute_backtest -> compute_performance_metrics -> persist -> respond.
#'
#' @param req Plumber request object.
#' @return list(status, body).
handle_backtest_run <- function(req) {
  # 1) Authentication
  auth <- require_auth(req)
  if (!auth$ok) {
    return(list(status = auth$status, body = auth$body))
  }
  user_id <- auth$claims$sub

  tryCatch(
    {
      # 2) Parse request body
      body <- req$body
      if (is.null(body) || !is.list(body)) {
        stop(validation_error("Request body is required and must be JSON."))
      }

      # 3) Validate strategy configuration
      config <- validate_strategy_config(
        body$ticker, body$start, body$end,
        body$initialCapital, body$strategy, body$parameters
      )

      # 4) Fetch OHLCV data
      ohlcv <- fetch_ohlcv(config$ticker, config$start, config$end)
      if (NROW(ohlcv) == 0) {
        stop(validation_error(sprintf(
          "No market data available for '%s' in the requested date range.",
          config$ticker
        )))
      }

      close_prices <- ohlcv[, "Close"]

      # 5) Calculate indicators and generate signals
      if (config$strategy == "MA_Crossover") {
        fast_sma <- calculate_sma(close_prices, config$parameters$fast_period)
        slow_sma <- calculate_sma(close_prices, config$parameters$slow_period)
        signals <- generate_ma_crossover_signals(fast_sma, slow_sma)
      } else if (config$strategy == "RSI") {
        rsi <- calculate_rsi(close_prices, config$parameters$period)
        signals <- generate_rsi_signals(
          rsi, config$parameters$oversold, config$parameters$overbought
        )
      } else {
        stop(validation_error(sprintf("Unknown strategy: %s", config$strategy)))
      }

      # 6) Run the Phase 5 backtest engine
      result <- run_backtest(
        ohlcv = ohlcv,
        signals = signals,
        initial_capital = config$initial_capital,
        start_date = config$start,
        end_date = config$end,
        ticker = config$ticker,
        strategy = config$strategy,
        parameters = config$parameters
      )

      # 7) Serialize to plain R lists (no xts, no data.frames, no NA)
      metrics_list <- metrics_to_list(result$summary)
      ec_list <- equity_curve_to_list(result$equity_curve)
      th_list <- trade_history_to_list(result$trades$history)

      # 8) Persist to database
      backtest_id <- NULL
      con <- tryCatch(get_db_connection(), error = function(e) {
        cat("[backtest/run] DB connection failed:", conditionMessage(e), "\n")
        NULL
      })
      if (!is.null(con)) {
        tryCatch(
          {
            bt <- insert_backtest(
              con, user_id, config$ticker, config$strategy,
              config$start, config$end, config$initial_capital,
              config$parameters
            )
            backtest_id <- bt$id

            ec_json <- jsonlite::toJSON(ec_list, auto_unbox = TRUE)
            th_json <- jsonlite::toJSON(th_list, auto_unbox = TRUE)
            insert_result(con, backtest_id, result$summary, ec_json, th_json)
          },
          error = function(e) {
            cat("[backtest/run] Persistence error:", conditionMessage(e), "\n")
          }
        )
        close_db_connection(con)
      }

      # 9) Return the response
      response_body <- list(
        success = TRUE,
        backtestId = backtest_id,
        ticker = config$ticker,
        strategy = config$strategy,
        start = format(config$start, "%Y-%m-%d"),
        end = format(config$end, "%Y-%m-%d"),
        parameters = config$parameters,
        metrics = metrics_list,
        equityCurve = ec_list,
        tradeHistory = th_list
      )

      list(status = 200L, body = response_body)
    },
    validation_error = function(e) {
      list(status = 400L, body = list(success = FALSE, error = conditionMessage(e)))
    },
    data_error = function(e) {
      list(status = e$status, body = list(success = FALSE, error = conditionMessage(e)))
    },
    error = function(e) {
      cat("[backtest/run] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE,
        error = "An unexpected server error occurred. Please try again later."
      ))
    }
  )
}

# ---------------------------------------------------------------------------
# GET /backtest/history handler
# ---------------------------------------------------------------------------
#' Get paginated backtest history for the authenticated user.
#'
#' @param req Plumber request object.
#' @param limit Max results (default 20).
#' @param offset Pagination offset (default 0).
#' @return list(status, body).
handle_backtest_history <- function(req, limit = 20, offset = 0) {
  auth <- require_auth(req)
  if (!auth$ok) {
    return(list(status = auth$status, body = auth$body))
  }
  user_id <- auth$claims$sub

  con <- tryCatch(get_db_connection(), error = function(e) NULL)
  if (is.null(con)) {
    return(list(status = 500L, body = list(
      success = FALSE, error = "Database connection failed."
    )))
  }

  tryCatch(
    {
      lim <- min(as.integer(limit), 100L)
      off <- max(as.integer(offset), 0L)
      history <- get_backtest_history(con, user_id, lim, off)

      # Convert each item to camelCase with NA -> NULL
      items <- lapply(history$items, function(item) {
        na_to_null(list(
          id = item$id,
          ticker = item$ticker,
          strategy = item$strategy,
          startDate = format(as.Date(item$start_date), "%Y-%m-%d"),
          endDate = format(as.Date(item$end_date), "%Y-%m-%d"),
          initialCapital = item$initial_capital,
          createdAt = as.character(item$created_at),
          totalReturn = item$total_return,
          sharpeRatio = item$sharpe_ratio,
          totalTrades = item$total_trades
        ))
      })

      list(status = 200L, body = list(
        success = TRUE,
        items = items,
        total = history$total,
        limit = lim,
        offset = off
      ))
    },
    error = function(e) {
      cat("[backtest/history] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE, error = "An unexpected error occurred."
      ))
    },
    finally = close_db_connection(con)
  )
}

# ---------------------------------------------------------------------------
# GET /backtest/<id>/trades handler
# ---------------------------------------------------------------------------
#' Get the trade history for a backtest.
#'
#' @param req Plumber request object.
#' @param id Backtest ID (from URL path).
#' @return list(status, body).
handle_backtest_trades <- function(req, id) {
  auth <- require_auth(req)
  if (!auth$ok) {
    return(list(status = auth$status, body = auth$body))
  }
  user_id <- auth$claims$sub

  bt_id <- suppressWarnings(as.integer(id))
  if (is.na(bt_id)) {
    return(list(status = 400L, body = list(
      success = FALSE, error = "Invalid backtest ID."
    )))
  }

  con <- tryCatch(get_db_connection(), error = function(e) NULL)
  if (is.null(con)) {
    return(list(status = 500L, body = list(
      success = FALSE, error = "Database connection failed."
    )))
  }

  tryCatch(
    {
      bt <- get_backtest_by_id(con, bt_id, user_id)
      if (is.null(bt)) {
        return(list(status = 404L, body = list(
          success = FALSE, error = "Backtest not found."
        )))
      }

      trades <- tryCatch(
        jsonlite::fromJSON(as.character(bt$trade_history)),
        error = function(e) list()
      )

      list(status = 200L, body = list(
        success = TRUE,
        backtestId = bt$id,
        tradeHistory = na_to_null(trades)
      ))
    },
    error = function(e) {
      cat("[backtest/trades] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE, error = "An unexpected error occurred."
      ))
    },
    finally = close_db_connection(con)
  )
}

# ---------------------------------------------------------------------------
# GET /backtest/<id>/metrics handler
# ---------------------------------------------------------------------------
#' Get the performance metrics for a backtest.
#'
#' @param req Plumber request object.
#' @param id Backtest ID (from URL path).
#' @return list(status, body).
handle_backtest_metrics <- function(req, id) {
  auth <- require_auth(req)
  if (!auth$ok) {
    return(list(status = auth$status, body = auth$body))
  }
  user_id <- auth$claims$sub

  bt_id <- suppressWarnings(as.integer(id))
  if (is.na(bt_id)) {
    return(list(status = 400L, body = list(
      success = FALSE, error = "Invalid backtest ID."
    )))
  }

  con <- tryCatch(get_db_connection(), error = function(e) NULL)
  if (is.null(con)) {
    return(list(status = 500L, body = list(
      success = FALSE, error = "Database connection failed."
    )))
  }

  tryCatch(
    {
      bt <- get_backtest_by_id(con, bt_id, user_id)
      if (is.null(bt)) {
        return(list(status = 404L, body = list(
          success = FALSE, error = "Backtest not found."
        )))
      }

      metrics_list <- na_to_null(list(
        initialCapital = if (!is.na(bt$result_initial_capital)) bt$result_initial_capital else bt$initial_capital,
        finalValue = bt$result_final_value,
        totalReturn = bt$total_return,
        cagr = bt$cagr,
        sharpe = bt$sharpe_ratio,
        sortino = bt$sortino_ratio,
        maximumDrawdown = bt$max_drawdown,
        winRate = bt$win_rate,
        volatility = bt$volatility,
        totalTrades = bt$total_trades
      ))

      list(status = 200L, body = list(
        success = TRUE,
        backtestId = bt$id,
        metrics = metrics_list
      ))
    },
    error = function(e) {
      cat("[backtest/metrics] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE, error = "An unexpected error occurred."
      ))
    },
    finally = close_db_connection(con)
  )
}

# ---------------------------------------------------------------------------
# GET /backtest/<id> handler
# ---------------------------------------------------------------------------
#' Get a full backtest record by ID.
#'
#' @param req Plumber request object.
#' @param id Backtest ID (from URL path).
#' @return list(status, body).
handle_backtest_get <- function(req, id) {
  auth <- require_auth(req)
  if (!auth$ok) {
    return(list(status = auth$status, body = auth$body))
  }
  user_id <- auth$claims$sub

  bt_id <- suppressWarnings(as.integer(id))
  if (is.na(bt_id)) {
    return(list(status = 400L, body = list(
      success = FALSE, error = "Invalid backtest ID."
    )))
  }

  con <- tryCatch(get_db_connection(), error = function(e) NULL)
  if (is.null(con)) {
    return(list(status = 500L, body = list(
      success = FALSE, error = "Database connection failed."
    )))
  }

  tryCatch(
    {
      bt <- get_backtest_by_id(con, bt_id, user_id)
      if (is.null(bt)) {
        return(list(status = 404L, body = list(
          success = FALSE, error = "Backtest not found."
        )))
      }

      # Parse JSONB fields back to R objects
      params <- tryCatch(
        jsonlite::fromJSON(as.character(bt$parameters)),
        error = function(e) list()
      )
      eq_curve <- tryCatch(
        jsonlite::fromJSON(as.character(bt$equity_curve)),
        error = function(e) list()
      )
      trades <- tryCatch(
        jsonlite::fromJSON(as.character(bt$trade_history)),
        error = function(e) list()
      )

      response <- na_to_null(list(
        success = TRUE,
        id = bt$id,
        ticker = bt$ticker,
        strategy = bt$strategy,
        startDate = format(as.Date(bt$start_date), "%Y-%m-%d"),
        endDate = format(as.Date(bt$end_date), "%Y-%m-%d"),
        initialCapital = bt$initial_capital,
        parameters = params,
        createdAt = as.character(bt$created_at),
        metrics = list(
          initialCapital = if (!is.na(bt$result_initial_capital)) bt$result_initial_capital else bt$initial_capital,
          finalValue = bt$result_final_value,
          totalReturn = bt$total_return,
          cagr = bt$cagr,
          sharpe = bt$sharpe_ratio,
          sortino = bt$sortino_ratio,
          maximumDrawdown = bt$max_drawdown,
          winRate = bt$win_rate,
          volatility = bt$volatility,
          totalTrades = bt$total_trades
        ),
        equityCurve = eq_curve,
        tradeHistory = trades
      ))

      list(status = 200L, body = response)
    },
    error = function(e) {
      cat("[backtest/get] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE, error = "An unexpected error occurred."
      ))
    },
    finally = close_db_connection(con)
  )
}