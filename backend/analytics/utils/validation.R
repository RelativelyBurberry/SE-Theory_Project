# AlphaForge Insight — strategy parameter validation (Phase 4)
#
# Centralized validation for all strategy-related parameters. Validation is
# reusable across API endpoints and ensures that invalid configurations never
# reach the indicator/signal/execution layers.
#
# Functions:
#   STRATEGY_DEFAULTS              Named list of default parameters
#   validate_strategy_config(...)  Complete strategy configuration validation
#   validate_ticker_param(...)     Ticker validation (delegates to data layer)
#   validate_date_range(...)       Start/end date validation
#   validate_initial_capital(...)  Initial capital validation
#   validate_ma_params(...)        Moving Average Crossover parameter validation
#   validate_rsi_params(...)       RSI parameter validation
#
# Error convention: all functions raise "validation_error" carrying a
# client-safe message; the API layer converts them into 400 JSON responses.

# ---------------------------------------------------------------------------
# Structured validation errors
# ---------------------------------------------------------------------------
validation_error <- function(message) {
  structure(
    list(message = message, status = 400L),
    class = c("validation_error", "error", "condition")
  )
}

# ---------------------------------------------------------------------------
# Strategy defaults (single source of truth)
# ---------------------------------------------------------------------------
# These are the documented Review 1 defaults (IMPLEMENTATION_PLAN.md § I).
# The frontend receives these same values; the backend validates independently.
STRATEGY_DEFAULTS <- list(
  ma_crossover = list(
    fast_period = 50L,
    slow_period = 200L
  ),
  rsi = list(
    period = 14L,
    oversold = 30,
    overbought = 70
  )
)

# ---------------------------------------------------------------------------
# Ticker validation
# ---------------------------------------------------------------------------
#' Validate ticker parameter.
#' @param ticker Raw ticker string.
#' @return Normalized ticker string (uppercase, trimmed).
#' @throws validation_error if ticker is missing or empty.
validate_ticker_param <- function(ticker) {
  if (is.null(ticker) || length(ticker) == 0 || 
      !nzchar(trimws(as.character(ticker)))) {
    stop(validation_error("ticker is required."))
  }
  toupper(trimws(as.character(ticker)))
}

# ---------------------------------------------------------------------------
# Date range validation
# ---------------------------------------------------------------------------
#' Validate start and end dates.
#' @param start Raw start date string (YYYY-MM-DD).
#' @param end Raw end date string (YYYY-MM-DD).
#' @param today Optional "today" Date (injected for deterministic tests).
#' @return list(start = Date, end = Date).
#' @throws validation_error on any violation.
validate_date_range <- function(start, end, today = Sys.Date()) {
  # Check presence
  if (is.null(start) || length(start) == 0 || 
      !nzchar(trimws(as.character(start)))) {
    stop(validation_error("start date is required (format YYYY-MM-DD)."))
  }
  if (is.null(end) || length(end) == 0 || 
      !nzchar(trimws(as.character(end)))) {
    stop(validation_error("end date is required (format YYYY-MM-DD)."))
  }
  
  # Parse
  s_str <- trimws(as.character(start))
  e_str <- trimws(as.character(end))
  
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s_str)) {
    stop(validation_error(sprintf(
      "start date must use YYYY-MM-DD format (got '%s').", s_str
    )))
  }
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", e_str)) {
    stop(validation_error(sprintf(
      "end date must use YYYY-MM-DD format (got '%s').", e_str
    )))
  }
  
  s <- tryCatch(as.Date(s_str, format = "%Y-%m-%d"), error = function(e) NA)
  e <- tryCatch(as.Date(e_str, format = "%Y-%m-%d"), error = function(e) NA)
  
  if (is.na(s)) {
    stop(validation_error(sprintf("Invalid start date: '%s'.", s_str)))
  }
  if (is.na(e)) {
    stop(validation_error(sprintf("Invalid end date: '%s'.", e_str)))
  }
  
  today <- as.Date(today)
  
  # Future dates
  if (s > today) {
    stop(validation_error("start date cannot be in the future."))
  }
  if (e > today) {
    stop(validation_error("end date cannot be in the future."))
  }
  
  # Ordering
  if (s > e) {
    stop(validation_error("start date must be on or before the end date."))
  }
  
  list(start = s, end = e)
}

# ---------------------------------------------------------------------------
# Initial capital validation
# ---------------------------------------------------------------------------
#' Validate initial capital.
#' @param capital Raw initial capital value.
#' @return Numeric initial capital.
#' @throws validation_error if capital is invalid.
validate_initial_capital <- function(capital) {
  if (is.null(capital) || length(capital) == 0) {
    stop(validation_error("initial capital is required."))
  }
  
  cap <- tryCatch(as.numeric(capital), warning = function(w) NA, 
                  error = function(e) NA)
  
  if (length(cap) != 1 || is.na(cap)) {
    stop(validation_error("initial capital must be a number."))
  }
  
  if (!is.finite(cap)) {
    stop(validation_error("initial capital must be a finite number."))
  }
  
  if (cap <= 0) {
    stop(validation_error("initial capital must be greater than zero."))
  }
  
  cap
}

# ---------------------------------------------------------------------------
# Strategy type validation
# ---------------------------------------------------------------------------
SUPPORTED_STRATEGIES <- c("MA_Crossover", "RSI")

#' Validate strategy type.
#' @param strategy Raw strategy type string.
#' @return Normalized strategy type.
#' @throws validation_error if strategy is invalid.
validate_strategy_type <- function(strategy) {
  if (is.null(strategy) || length(strategy) == 0 || 
      !nzchar(trimws(as.character(strategy)))) {
    stop(validation_error("strategy type is required."))
  }
  
  s <- trimws(as.character(strategy))
  
  if (!(s %in% SUPPORTED_STRATEGIES)) {
    stop(validation_error(sprintf(
      "Unsupported strategy type '%s'. Supported strategies: %s.",
      s, paste(SUPPORTED_STRATEGIES, collapse = ", ")
    )))
  }
  
  s
}

# ---------------------------------------------------------------------------
# MA Crossover parameter validation
# ---------------------------------------------------------------------------
#' Validate Moving Average Crossover parameters.
#' @param fast_period Raw fast period.
#' @param slow_period Raw slow period.
#' @return list(fast_period = integer, slow_period = integer).
#' @throws validation_error on any violation.
validate_ma_params <- function(fast_period, slow_period) {
  # Fast period
  if (is.null(fast_period) || length(fast_period) == 0) {
    stop(validation_error("fast_period is required for MA_Crossover strategy."))
  }
  
  fp <- tryCatch(as.numeric(fast_period), warning = function(w) NA, 
                 error = function(e) NA)
  
  if (length(fp) != 1 || is.na(fp)) {
    stop(validation_error("fast_period must be a number."))
  }
  
  if (!is.finite(fp)) {
    stop(validation_error("fast_period must be a finite number."))
  }
  
  if (fp < 1 || abs(fp - round(fp)) > 1e-9) {
    stop(validation_error(
      "fast_period must be a positive integer (>= 1)."
    ))
  }
  
  fp_int <- as.integer(round(fp))
  
  # Slow period
  if (is.null(slow_period) || length(slow_period) == 0) {
    stop(validation_error("slow_period is required for MA_Crossover strategy."))
  }
  
  sp <- tryCatch(as.numeric(slow_period), warning = function(w) NA, 
                 error = function(e) NA)
  
  if (length(sp) != 1 || is.na(sp)) {
    stop(validation_error("slow_period must be a number."))
  }
  
  if (!is.finite(sp)) {
    stop(validation_error("slow_period must be a finite number."))
  }
  
  if (sp < 1 || abs(sp - round(sp)) > 1e-9) {
    stop(validation_error(
      "slow_period must be a positive integer (>= 1)."
    ))
  }
  
  sp_int <- as.integer(round(sp))
  
  # Ordering constraint
  if (fp_int >= sp_int) {
    stop(validation_error(
      "fast_period must be strictly less than slow_period."
    ))
  }
  
  list(fast_period = fp_int, slow_period = sp_int)
}

# ---------------------------------------------------------------------------
# RSI parameter validation
# ---------------------------------------------------------------------------
#' Validate RSI parameters.
#' @param period Raw RSI period.
#' @param oversold Raw oversold threshold.
#' @param overbought Raw overbought threshold.
#' @return list(period = integer, oversold = numeric, overbought = numeric).
#' @throws validation_error on any violation.
validate_rsi_params <- function(period, oversold, overbought) {
  # Period
  if (is.null(period) || length(period) == 0) {
    stop(validation_error("period is required for RSI strategy."))
  }
  
  p <- tryCatch(as.numeric(period), warning = function(w) NA, 
                error = function(e) NA)
  
  if (length(p) != 1 || is.na(p)) {
    stop(validation_error("RSI period must be a number."))
  }
  
  if (!is.finite(p)) {
    stop(validation_error("RSI period must be a finite number."))
  }
  
  if (p < 1 || abs(p - round(p)) > 1e-9) {
    stop(validation_error(
      "RSI period must be a positive integer (>= 1)."
    ))
  }
  
  p_int <- as.integer(round(p))
  
  # Oversold
  if (is.null(oversold) || length(oversold) == 0) {
    stop(validation_error("oversold threshold is required for RSI strategy."))
  }
  
  os <- tryCatch(as.numeric(oversold), warning = function(w) NA, 
                 error = function(e) NA)
  
  if (length(os) != 1 || is.na(os)) {
    stop(validation_error("oversold threshold must be a number."))
  }
  
  if (!is.finite(os)) {
    stop(validation_error("oversold threshold must be a finite number."))
  }
  
  if (os <= 0) {
    stop(validation_error(
      "oversold threshold must be greater than zero."
    ))
  }
  
  if (os >= 100) {
    stop(validation_error(
      "oversold threshold must be less than 100."
    ))
  }
  
  # Overbought
  if (is.null(overbought) || length(overbought) == 0) {
    stop(validation_error("overbought threshold is required for RSI strategy."))
  }
  
  ob <- tryCatch(as.numeric(overbought), warning = function(w) NA, 
                 error = function(e) NA)
  
  if (length(ob) != 1 || is.na(ob)) {
    stop(validation_error("overbought threshold must be a number."))
  }
  
  if (!is.finite(ob)) {
    stop(validation_error("overbought threshold must be a finite number."))
  }
  
  if (ob <= 0) {
    stop(validation_error(
      "overbought threshold must be greater than zero."
    ))
  }
  
  if (ob >= 100) {
    stop(validation_error(
      "overbought threshold must be less than 100."
    ))
  }
  
  # Ordering constraint
  if (os >= ob) {
    stop(validation_error(
      "oversold threshold must be strictly less than overbought threshold."
    ))
  }
  
  list(period = p_int, oversold = os, overbought = ob)
}

# ---------------------------------------------------------------------------
# Complete strategy configuration validation
# ---------------------------------------------------------------------------
#' Validate a complete strategy configuration.
#'
#' This is the top-level validation function called by API endpoints.
#'
#' @param ticker Raw ticker.
#' @param start Raw start date.
#' @param end Raw end date.
#' @param initial_capital Raw initial capital.
#' @param strategy Raw strategy type.
#' @param parameters Named list of strategy-specific parameters.
#' @param today Optional "today" Date (for deterministic testing).
#' @return Named list with normalized and validated configuration:
#'   list(ticker, start, end, initial_capital, strategy, parameters).
#' @throws validation_error on any violation.
validate_strategy_config <- function(ticker, start, end, initial_capital, 
                                     strategy, parameters, today = Sys.Date()) {
  # Validate common parameters
  t <- validate_ticker_param(ticker)
  dates <- validate_date_range(start, end, today)
  cap <- validate_initial_capital(initial_capital)
  strat <- validate_strategy_type(strategy)
  
  # Validate strategy-specific parameters
  if (strat == "MA_Crossover") {
    if (is.null(parameters) || !is.list(parameters)) {
      stop(validation_error(
        "parameters must be provided as a list for MA_Crossover strategy."
      ))
    }
    params <- validate_ma_params(
      parameters$fast_period,
      parameters$slow_period
    )
  } else if (strat == "RSI") {
    if (is.null(parameters) || !is.list(parameters)) {
      stop(validation_error(
        "parameters must be provided as a list for RSI strategy."
      ))
    }
    params <- validate_rsi_params(
      parameters$period,
      parameters$oversold,
      parameters$overbought
    )
  } else {
    stop(validation_error(sprintf("Unknown strategy type: %s", strat)))
  }
  
  list(
    ticker = t,
    start = dates$start,
    end = dates$end,
    initial_capital = cap,
    strategy = strat,
    parameters = params
  )
}
