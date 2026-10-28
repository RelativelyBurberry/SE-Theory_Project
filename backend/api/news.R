# AlphaForge Insight — Live News API handlers (Phase H.4)
#
# Handler for GET /news/live endpoint.
# Follows the existing backend conventions (see backend/api/backtest.R):
#   - JWT via require_auth(req) as the FIRST step for protected routes
#   - validation_error -> 400, news_error -> e$status, error -> 500 (safe msg)
#   - Plain-R list bodies; NA removed via na_to_null
#
#   GET /news/live   Fetch recent news for tickers (public for now)

# ---------------------------------------------------------------------------
# Request-parsing helpers
# ---------------------------------------------------------------------------

#' Parse and validate tickers parameter for live news request.
#' @param tickers Raw tickers query parameter (comma-separated or array).
#' @return Character vector of validated tickers.
#' @throws validation_error on invalid input.
.parse_news_tickers_param <- function(tickers) {
  if (is.null(tickers) || length(tickers) == 0) {
    stop(validation_error("tickers query parameter is required."))
  }
  
  # Handle comma-separated string or array
  if (length(tickers) == 1 && grepl(",", tickers[1], fixed = TRUE)) {
    tickers <- strsplit(tickers[1], ",", fixed = TRUE)[[1]]
  }
  
  # Normalize to uppercase, trim whitespace
  tickers <- toupper(trimws(as.character(tickers)))
  tickers <- tickers[nzchar(tickers)]
  
  if (length(tickers) == 0) {
    stop(validation_error("At least one valid ticker is required."))
  }
  
  if (length(tickers) > 10) {
    stop(validation_error("Maximum 10 tickers allowed per request."))
  }
  
  tickers
}

# ---------------------------------------------------------------------------
# Handler for GET /news/live
# ---------------------------------------------------------------------------

#' Handler for GET /news/live (public endpoint).
#' @param tickers Comma-separated or array of ticker symbols.
#' @param limit Maximum articles to return (default 3, max 3 on free tier).
#' @param req Plumber request object.
#' @return list {status, body} — caller sets the HTTP status.
handle_news_live <- function(tickers = NULL, limit = 3L, req) {
  # Note: This endpoint is PUBLIC (no auth required) for simplicity.
  # If authentication is needed later, uncomment the following:
  # auth <- require_auth(req)
  # if (!auth$ok) {
  #   return(list(status = auth$status, body = auth$body))
  # }
  
  tryCatch(
    {
      # Validate tickers
      validated_tickers <- .parse_news_tickers_param(tickers)
      
      # Validate limit
      limit <- max(1L, min(as.integer(limit), 3L))
      
      # Fetch live news
      result <- fetch_live_news(validated_tickers, limit)
      
      # Return success response
      list(status = 200L, body = result)
    },
    validation_error = function(e) {
      list(status = 400L, body = list(
        success = FALSE,
        error = conditionMessage(e)
      ))
    },
    news_error = function(e) {
      # News-specific errors carry their own status
      list(status = e$status, body = list(
        success = FALSE,
        error = conditionMessage(e)
      ))
    },
    error = function(e) {
      # Unexpected internal error: log server-side only
      cat("[news-live] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE,
        error = "An unexpected server error occurred while fetching news. Please try again later."
      ))
    }
  )
}
