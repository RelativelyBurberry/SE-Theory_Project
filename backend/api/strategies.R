# AlphaForge Insight — strategy preview endpoint (Phase 4)
#
# Phase 4 implements signal-generation without trade execution. The backtesting
# engine (portfolio/cash/equity curve/metrics) belongs to Phase 5.
#
# Endpoints (Phase 4 scope):
#   POST /strategies/preview   Generate signals for a strategy configuration
#     Request:  { ticker, start, end, strategy, parameters }
#     Response: { success, ticker, start, end, strategy, parameters,
#                 signal_count, signals: [{date, close, indicators, signal}] }
#
# The endpoint validates the configuration, retrieves market data, calculates
# indicators, and generates signals. It does NOT execute trades or compute
# performance metrics (that's Phase 5).
#
# Later phases (post-Review 1) may add:
#   POST /strategies   Save a named strategy template
#   GET  /strategies   List saved strategies
#
# Both are JWT-protected (require_auth guard).

#' Handler for POST /strategies/preview (JWT-protected).
#'
#' Validates strategy configuration, retrieves market data, calculates
#' indicators, and generates signals. Returns the signal series for frontend
#' preview/debugging. Does NOT execute trades (Phase 5).
#'
#' @param req Plumber request object (for auth + body).
#' @return list {status, body} — caller sets the HTTP status.
handle_strategy_preview <- function(req) {
  # 1) Authentication guard FIRST
  auth <- require_auth(req)
  if (!auth$ok) {
    return(list(status = auth$status, body = auth$body))
  }
  
  tryCatch(
    {
      # 2) Parse request body
      body <- req$body
      if (is.null(body) || !is.list(body)) {
        stop(validation_error("Request body is required and must be JSON."))
      }
      
      ticker <- body$ticker
      start <- body$start
      end <- body$end
      strategy <- body$strategy
      parameters <- body$parameters
      # initialCapital is validated when supplied (mandatory for the Phase 5
      # /backtest/run request shape) but does NOT affect signal generation —
      # the preview produces no portfolio/cash state. Documented Review 1
      # experiment default (10000) is used when the client omits it.
      initial_capital <- body$initialCapital
      if (is.null(initial_capital)) initial_capital <- 10000
      
      # 3) Validate complete strategy configuration
      config <- validate_strategy_config(
        ticker, start, end,
        initial_capital,
        strategy, parameters
      )
      
      # 4) Retrieve market data
      data_xts <- fetch_ohlcv(config$ticker, config$start, config$end)
      
      if (NROW(data_xts) == 0) {
        stop(validation_error(sprintf(
          "No market data available for '%s' in the requested date range.",
          config$ticker
        )))
      }
      
      close_prices <- data_xts[, "Close"]
      
      # 5) Calculate indicators and generate signals based on strategy type
      # NOTE on JSON serialization: jsonlite (plumber's unboxedJSON serializer)
      # renders R NULL elements as "{}" and NA_real_ as the STRING "NA" — neither
      # is a clean JSON representation of "no value". Strategy preview rows
      # therefore OMIT absent fields entirely (missing key == no value yet).
      # Frontend consumers must treat a missing indicator/signal key as absent.
      idx <- index(data_xts)
      n_rows <- NROW(data_xts)

      make_rows <- function(indicator_names, indicator_vectors, sig_vec) {
        lapply(seq_len(n_rows), function(i) {
          row <- list(
            date = format(idx[i], "%Y-%m-%d"),
            close = as.numeric(close_prices[i])
          )
          for (k in seq_along(indicator_names)) {
            v <- indicator_vectors[[k]][i]
            if (!is.na(v)) row[[indicator_names[k]]] <- v
          }
          # Signal: BUY/SELL on the crossing day; HOLD = key absent.
          if (!is.na(sig_vec[i])) row$signal <- sig_vec[i]
          row
        })
      }

      if (config$strategy == "MA_Crossover") {
        fast_sma <- calculate_sma(close_prices, config$parameters$fast_period)
        slow_sma <- calculate_sma(close_prices, config$parameters$slow_period)
        signals_xts <- generate_ma_crossover_signals(fast_sma, slow_sma)
        
        fast_v <- as.numeric(fast_sma[, 1])
        slow_v <- as.numeric(slow_sma[, 1])
        sig_v <- as.character(signals_xts[, 1])
        rows <- make_rows(c("fast_sma", "slow_sma"),
                          list(fast_v, slow_v), sig_v)
        
      } else if (config$strategy == "RSI") {
        rsi <- calculate_rsi(close_prices, config$parameters$period)
        signals_xts <- generate_rsi_signals(
          rsi,
          config$parameters$oversold,
          config$parameters$overbought
        )
        
        rsi_v <- as.numeric(rsi[, 1])
        sig_v <- as.character(signals_xts[, 1])
        rows <- make_rows("rsi", list(rsi_v), sig_v)
        
      } else {
        stop(validation_error(sprintf("Unknown strategy type: %s", config$strategy)))
      }
      
      # 6) Count actual signals (non-NA)
      signal_count <- sum(!is.na(sig_v))
      
      # 7) Return structured response
      list(status = 200L, body = list(
        success = TRUE,
        ticker = config$ticker,
        start = format(config$start, "%Y-%m-%d"),
        end = format(config$end, "%Y-%m-%d"),
        strategy = config$strategy,
        parameters = config$parameters,
        row_count = NROW(data_xts),
        signal_count = signal_count,
        signals = rows
      ))
      
    },
    validation_error = function(e) {
      list(status = 400L, body = list(success = FALSE, error = conditionMessage(e)))
    },
    data_error = function(e) {
      list(status = e$status, body = list(success = FALSE, error = conditionMessage(e)))
    },
    error = function(e) {
      # Internal failure: log details server-side only.
      cat("[strategy-preview] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE, error = "An unexpected server error occurred. Please try again later."
      ))
    }
  )
}

#' Handler for GET /strategies/defaults (public).
#'
#' Returns the documented default parameters for each strategy type.
#' This ensures the frontend and backend share a single source of truth.
#'
#' @return list {status, body}.
handle_strategy_defaults <- function() {
  list(status = 200L, body = list(
    success = TRUE,
    defaults = STRATEGY_DEFAULTS
  ))
}
