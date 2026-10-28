# AlphaForge Insight — ticker-search route handler (Phase 7 UX fix)
#
# Plain function (no plumber annotations), same pattern as the other handlers
# in backend/api/data.R: the annotated wrapper lives in backend/plumber.R.
#
#   GET /market/tickers/search   Public. Lightweight Yahoo Finance symbol
#                                lookup for the ticker autocomplete UI.
#
# Response shape:
#   GET /market/tickers/search?q=RELI
#     200 {"query": "RELI", "count": 3, "results": [
#            {"symbol": "RELIANCE.NS", "name": "Reliance Industries Ltd", "exchange": "NSE"},
#            ...]}
#     400 {"success": false, "error": "q (search query) is required."}
#     502 {"success": false, "error": "Ticker search is temporarily unavailable..."}

#' Handler for GET /market/tickers/search (public).
#' @param q Raw search query parameter.
#' @param limit Optional maximum number of suggestions (default 5, max 10).
#' @return list {status, body} — caller sets the HTTP status.
handle_ticker_search <- function(q, limit = NULL) {
  if (is.null(q) || length(q) == 0 || !nzchar(trimws(as.character(q)))) {
    return(list(status = 400L, body = list(
      success = FALSE, error = "q (search query) is required."
    )))
  }
  tryCatch(
    {
      max_n <- if (is.null(limit)) 5L else suppressWarnings(as.integer(limit))
      if (is.na(max_n)) max_n <- 5L
      out <- search_tickers(q, limit = max_n)
      list(status = 200L, body = c(list(success = TRUE), out))
    },
    data_error = function(e) {
      list(status = e$status, body = list(success = FALSE, error = conditionMessage(e)))
    },
    error = function(e) {
      cat("[ticker-search] ERROR:", conditionMessage(e), "\n")
      list(status = 500L, body = list(
        success = FALSE,
        error = "An unexpected server error occurred during ticker search."
      ))
    }
  )
}
