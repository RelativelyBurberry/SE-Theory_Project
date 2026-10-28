# AlphaForge Insight — market-data route handlers (Phase 3)
#
# These are PLAIN functions (no plumber #* annotations). The annotated route
# wrappers live in backend/plumber.R (same pattern as the health handler),
# because plumber.R cannot mount two routers at "/" — a second mount at the
# same path silently replaces the first (Router$mount stores one router per
# path). Keeping the handlers here preserves separation of concerns while the
# one-line wrappers in plumber.R remain discoverable by plumb(self).
#
#   /stocks/validate   Public. Ticker format + real existence check
#                      (IMPLEMENTATION_PLAN.md section O: left public).
#   /market-data       PROTECTED (valid JWT required, section O). Returns
#                      historical daily OHLCV for a ticker/date range.
#
# Response shapes:
#   GET /stocks/validate
#     200 {"ticker": "...", "valid": true}
#     200 {"ticker": "...", "valid": false, "error": "..."}
#     400 {"success": false, "error": "ticker query parameter is required."}
#
#   GET /market-data
#     200 {"success": true, "ticker", "start", "end", "count",
#          "data": [{"date","open","high","low","close","adjusted_close","volume"}]}
#     400 validation / 401 auth / 502 data source unavailable / 500 internal

#' Handler for GET /stocks/validate (public).
#' @param ticker Raw ticker query parameter.
#' @return list {ticker, valid, error?} — caller sets the HTTP status.
handle_stocks_validate <- function(ticker) {
  if (is.null(ticker) || length(ticker) == 0 || !nzchar(trimws(as.character(ticker)))) {
    return(list(status = 400L, body = list(
      success = FALSE, error = "ticker query parameter is required."
    )))
  }
  result <- validate_ticker(ticker)
  list(status = 200L, body = result)
}

#' Handler for GET /market-data (JWT-protected).
#' @param ticker Raw ticker query parameter.
#' @param start Raw start date query parameter.
#' @param end Raw end date query parameter.
#' @param req Plumber request object (for the auth guard).
#' @return list {status, body} — caller sets the HTTP status.
handle_market_data <- function(ticker, start, end, req) {
  # 1) Authentication guard FIRST — rejects unauthenticated clients before any
  #    data-source work (reuses the Phase 2 JWT middleware; no DB lookup).
  auth <- require_auth(req)
  if (!auth$ok) {
    return(list(status = auth$status, body = auth$body))
  }

  tryCatch(
    {
      params <- validate_market_data_params(ticker, start, end)

      # 2) Retrieve + sanitize (cached). Provider failures stop() with a
      #    data_error carrying 502; "no data"/quality issues carry 400 — both
      #    are converted below (messages are client-safe).
      data_xts <- fetch_ohlcv(params$ticker, params$start, params$end)

      # 3) Serialize to a plain, frontend-friendly structure.
      payload <- ohlcv_to_list(data_xts, params$ticker, params$start, params$end)
      list(status = 200L, body = c(list(success = TRUE), payload))
    },
    data_error = function(e) {
      # Validation / data-source errors: message is client-safe; the status
      # reflects the error type (400 for bad input, 502 for source outage).
      list(status = e$status, body = list(success = FALSE, error = conditionMessage(e)))
    },
    error = function(e) {
      # Internal failure: log details server-side only.
      cat("[market-data] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE, error = "An unexpected server error occurred. Please try again later."
      ))
    }
  )
}

#' Internal market-data endpoint for the Python forecasting service.
#' Protected by a shared server-to-server token, not a browser/JWT token.
#' Configure ALPHAFORGE_INTERNAL_MARKET_DATA_TOKEN in both Render services.
handle_internal_market_data <- function(ticker, start, end, req) {
  expected <- trimws(Sys.getenv("ALPHAFORGE_INTERNAL_MARKET_DATA_TOKEN", unset = ""))
  supplied <- trimws(as.character(req$HTTP_X_ALPHAFORGE_INTERNAL_TOKEN %||% ""))
  if (!nzchar(expected)) {
    return(list(status = 503L, body = list(
      success = FALSE, error = "Internal market-data access is not configured."
    )))
  }
  if (!nzchar(supplied) || !identical(supplied, expected)) {
    return(list(status = 401L, body = list(
      success = FALSE, error = "Unauthorized internal market-data request."
    )))
  }

  tryCatch({
    params <- validate_market_data_params(ticker, start, end)
    data_xts <- fetch_ohlcv(params$ticker, params$start, params$end)
    payload <- ohlcv_to_list(data_xts, params$ticker, params$start, params$end)
    list(status = 200L, body = c(list(success = TRUE), payload))
  }, data_error = function(e) {
    list(status = e$status, body = list(success = FALSE, error = conditionMessage(e)))
  }, error = function(e) {
    cat("[market-data] internal endpoint error:", conditionMessage(e), "\n")
    list(status = 500L, body = list(success = FALSE, error = "Internal market-data retrieval failed."))
  })
}
