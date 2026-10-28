# AlphaForge Insight — ticker-search unit tests (autocomplete backend)
#
# Covers the ticker-search layer in backend/analytics/data_retrieval.R:
#   - search_tickers() argument validation (empty/missing query)
#   - .parse_ticker_search() response shaping from a FIXTURED Yahoo Finance
#     search payload (quote-type filtering, symbol normalization,
#     de-duplication, name fallbacks) — pure function, no network
#   - graceful data_error (502) when the source is unreachable
#
# Run from the repository root:
#   Rscript tests/analytics/test_ticker_search.R
#
# Exit code 0 = pass, 1 = fail. Requires: jsonlite (and the data layer).

suppressPackageStartupMessages(source("backend/analytics/data_retrieval.R"))

results <- list()
record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

# Helper: try a call and report whether it raised a data_error with a status.
is_data_error <- function(expr, status) {
  e <- tryCatch({ expr; NULL }, data_error = function(e) e, error = function(e) e)
  !is.null(e) && inherits(e, "data_error") && identical(e$status, as.integer(status))
}

# ---------------------------------------------------------------------------
# 1. search_tickers argument validation (no network)
# ---------------------------------------------------------------------------
record("search: empty query rejected (400)",
       is_data_error(search_tickers(""), 400L))
record("search: whitespace-only query rejected (400)",
       is_data_error(search_tickers("   "), 400L))
record("search: NULL query rejected (400)",
       is_data_error(search_tickers(NULL), 400L))

# ---------------------------------------------------------------------------
# 2. Response shaping from a fixture (pure .parse_ticker_search, no network)
# ---------------------------------------------------------------------------
# Fixture mirroring the real Yahoo Finance search response (verified live:
# query1.finance.yahoo.com/v1/finance/search?q=RELI returns exactly this
# shape — symbol/shortname/longname/quoteType/exchDisp per quote).
FIXTURE_JSON <- '{
  "quotes": [
    {"symbol": "RELIANCE.NS", "shortname": "RELIANCE INDUSTRIES LTD",
     "longname": "Reliance Industries Limited", "quoteType": "EQUITY",
     "exchDisp": "NSE", "score": 21554.0},
    {"symbol": "BTC-USD", "shortname": "Bitcoin USD", "quoteType": "CRYPTOCURRENCY",
     "exchDisp": "CCC", "score": 20000.0},
    {"symbol": "reliance.bo", "shortname": "RELIANCE INDUSTRIES", "quoteType": "EQUITY",
     "exchDisp": "BSE", "score": 19000.0},
    {"symbol": "RS", "shortname": "Reliance, Inc.", "quoteType": "EQUITY",
     "exchDisp": "NYSE", "score": 18000.0},
    {"symbol": "BRK.B", "shortname": "Berkshire Hathaway", "quoteType": "EQUITY",
     "exchDisp": "NYSE", "score": 17500.0},
    {"symbol": "RELI.NS", "shortname": "", "longname": "Reliance Industries Limited",
     "quoteType": "EQUITY", "exchDisp": "NSE", "score": 17000.0},
    {"symbol": "WIPRO.NS", "shortname": "Wipro Limited", "quoteType": "EQUITY",
     "exchDisp": "NSE", "score": 16000.0},
    {"symbol": "NIFTY 50", "shortname": "Nifty 50 index", "quoteType": "INDEX",
     "exchDisp": "NSE", "score": 15000.0},
    {"symbol": "TCS.NS", "shortname": "Tata Consultancy Services", "quoteType": "EQUITY",
     "exchDisp": "NSE", "score": 14000.0}
  ]
}'

r <- .parse_ticker_search(FIXTURE_JSON, 5)
syms <- vapply(r, function(x) x$symbol, character(1))

record("parse: fixture yields 5 rows (limit respected)",
       length(r) == 5)
record("parse: first result is the top relevance hit",
       identical(syms[[1]], "RELIANCE.NS") &&
         identical(r[[1]]$name, "RELIANCE INDUSTRIES LTD"))
record("parse: every row carries symbol + name",
       all(vapply(r, function(x) nzchar(x$symbol) && nzchar(x$name), logical(1))))
record("parse: symbols normalized to uppercase (reliance.bo -> RELIANCE.BO)",
       "RELIANCE.BO" %in% syms && all(syms == toupper(syms)))
record("parse: duplicate symbols removed after normalization",
       !anyDuplicated(syms))
record("parse: non-EQUITY/ETF/INDEX quote types filtered out (BTC-USD dropped)",
       !any(grepl("BTC", syms, fixed = TRUE)))
record("parse: symbols failing the format rule filtered out (NIFTY 50 dropped)",
       !any(grepl(" ", syms, fixed = TRUE)))
record("parse: empty shortname falls back to longname",
       syms[which(syms == "RELI.NS")] == "RELI.NS" &&
         r[[which(syms == "RELI.NS")]]$name == "Reliance Industries Limited")
record("parse: exchange labels carried through",
       all(vapply(r, function(x) !is.null(x$exchange), logical(1))))
record("parse: relevance order preserved from the source",
       identical(syms, c("RELIANCE.NS", "RELIANCE.BO", "RS", "BRK.B", "RELI.NS")))

# Limit clamping inside the parser.
r2 <- .parse_ticker_search(FIXTURE_JSON, 3)
record("parse: limit=3 returns exactly 3 rows", length(r2) == 3)

r0 <- .parse_ticker_search(FIXTURE_JSON, 0)
record("parse: limit=0 returns no rows", length(r0) == 0)

# Malformed payloads never throw.
record("parse: unparseable JSON yields empty results",
       length(.parse_ticker_search("not json at all", 5)) == 0)
record("parse: JSON without quotes yields empty results",
       length(.parse_ticker_search("{\"foo\": 1}", 5)) == 0)
record("parse: empty string yields empty results",
       length(.parse_ticker_search("", 5)) == 0)

# ---------------------------------------------------------------------------
# 3. Failure behavior (deterministic, no network)
# ---------------------------------------------------------------------------
# search_tickers is defined in the global environment, so a globalenv stub of
# base::url shadows the connection opener for this call path — simulating a
# source outage without any network access. The failure must surface as a
# clean data_error (502), never an unhandled crash.
url_orig <- url
url <- function(...) stop("simulated source outage")
e <- tryCatch({ search_tickers("AAPL", 5); NULL },
              data_error = function(e) e, error = function(e) e)
url <- url_orig
record("search: simulated source outage raises handled data_error 502",
       !is.null(e) && inherits(e, "data_error") && identical(e$status, 502L))

# Handler-level contract (backend/api/search.R conventions, no network):
source("backend/api/search.R")
h400 <- handle_ticker_search(NULL)
record("handler: missing q parameter returns HTTP 400",
       identical(h400$status, 400L) && !isTRUE(h400$body$success))
hblank <- handle_ticker_search("   ")
record("handler: blank q parameter returns HTTP 400",
       identical(hblank$status, 400L) && !isTRUE(hblank$body$success))
hfail <- tryCatch({
  url <- function(...) stop("simulated source outage")
  handle_ticker_search("AAPL")
}, finally = url <- url_orig)
record("handler: source outage returns HTTP 502 with a client-safe message",
       identical(hfail$status, 502L) && !isTRUE(hfail$body$success) &&
         nzchar(hfail$body$error))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d ticker-search tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL TICKER-SEARCH TESTS PASSED\n")
quit(status = 0)
