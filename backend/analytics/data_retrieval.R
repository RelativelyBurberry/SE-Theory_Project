# AlphaForge Insight — market-data service (Phase 3)
#
# Historical daily OHLCV retrieval via quantmod.
# Provider selection: Alpha Vantage when ALPHAVANTAGE_API_KEY is configured;
# Yahoo Finance otherwise (legacy fallback). Configure MARKET_DATA_PROVIDER to
# 'alphavantage' or 'yahoo' to explicitly select a provider.
#
# Functions:
#   validate_ticker(ticker)            Format + real existence via the data layer
#   validate_market_data_params(...)   Tick/date argument validation
#   fetch_ohlcv(ticker, start, end)    xts of daily OHLCV (cached, sanitized)
#   sanitize_ohlcv(xts)                Missing-data policy (plan section L)
#   ohlcv_to_list(xts, ticker, start, end)  Frontend-friendly JSON structure
#   clear_data_cache()                 Test/ops helper to drop the in-memory cache
#
# Error convention: all functions raise errors of class "data_error" carrying
# an HTTP status; the API layer converts them into structured JSON responses.
# Raw provider/network errors are caught here, logged server-side and re-thrown
# as generic "data_error" messages so internal details never reach clients.

# ---------------------------------------------------------------------------
# Structured errors
# ---------------------------------------------------------------------------
library(xts)      # xts/zoo (index, cbind, [,)
library(zoo)      # na.approx for interpolation
library(quantmod) # getSymbols provider integrations
data_error <- function(message, status = 400L) {
  structure(
    list(message = message, status = as.integer(status)),
    class = c("data_error", "error", "condition")
  )
}

# ---------------------------------------------------------------------------
# Ticker validation
# ---------------------------------------------------------------------------
# Hyphen sits at the END of the class (literal by position; R's TRE regex
# engine does not reliably support \- escapes inside bracket expressions).
# Caret '^' is allowed as the first character for index symbols (e.g. ^GSPC).
# Maximum length 20: Yahoo Finance symbols legitimately exceed 10 characters
# because of the exchange suffix (RELIANCE.NS = 11, HDFCBANK.NS = 10,
# MOTHERSON.NS = 12; NSE/LSE symbols with suffixes are common).
TICKER_MAX_LENGTH <- 20L
TICKER_REGEX <- sprintf("^[A-Z0-9^][A-Z0-9.^-]{0,%d}$", TICKER_MAX_LENGTH - 1L)

#' Normalize a ticker to the canonical uppercase form.
#' @param ticker Raw ticker string from a client.
#' @return Uppercase trimmed ticker.
normalize_ticker <- function(ticker) {
  toupper(trimws(as.character(ticker)))
}

#' Validate ticker FORMAT only (no network access).
#' @param ticker Raw ticker string.
#' @return Normalized ticker on success.
#' @throws data_error (400) if the format is unacceptable.
validate_ticker_format <- function(ticker) {
  if (is.null(ticker) || length(ticker) == 0 || !nzchar(trimws(as.character(ticker)))) {
    stop(data_error("ticker is required."))
  }
  t <- normalize_ticker(ticker)
  if (!grepl(TICKER_REGEX, t)) {
    stop(data_error(
      sprintf(
        "ticker must be 1-%d characters: letters, digits, '.', '-', or '^' (got '%s').",
        TICKER_MAX_LENGTH, t
      )
    ))
  }
  t
}

# ---------------------------------------------------------------------------
# Date validation
# ---------------------------------------------------------------------------
DATE_REGEX <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"

#' Parse and validate a YYYY-MM-DD date string.
#' @param x Raw date string.
#' @param name Label ("start" or "end") for error messages.
#' @return A Date object.
#' @throws data_error (400) on malformed/absent input.
parse_request_date <- function(x, name) {
  if (is.null(x) || length(x) == 0 || !nzchar(trimws(as.character(x)))) {
    stop(data_error(sprintf("%s date is required (format YYYY-MM-DD).", name)))
  }
  s <- trimws(as.character(x))
  if (!grepl(DATE_REGEX, s)) {
    stop(data_error(sprintf(
      "%s date must use YYYY-MM-DD format (got '%s').", name, s
    )))
  }
  d <- tryCatch(as.Date(s, format = "%Y-%m-%d"), error = function(e) NA)
  if (is.na(d)) {
    stop(data_error(sprintf("Invalid %s date: '%s'.", name, s)))
  }
  d
}

#' Validate the full set of market-data parameters.
#' @param ticker Raw ticker.
#' @param start Raw start date.
#' @param end Raw end date.
#' @param today Optional "today" Date (injected for deterministic tests).
#' @return list(ticker, start, end) with normalized values.
#' @throws data_error (400) on any violation.
validate_market_data_params <- function(ticker, start, end, today = Sys.Date()) {
  t <- validate_ticker_format(ticker)
  s <- parse_request_date(start, "start")
  e <- parse_request_date(end, "end")
  today <- as.Date(today)
  if (s > today) {
    stop(data_error("start date cannot be in the future."))
  }
  if (e > today) {
    stop(data_error("end date cannot be in the future."))
  }
  if (s > e) {
    stop(data_error("start date must be on or before the end date."))
  }
  list(ticker = t, start = s, end = e)
}

# ---------------------------------------------------------------------------
# In-memory cache (optional performance optimization, plan section L)
# ---------------------------------------------------------------------------
.data_cache <- new.env(parent = emptyenv())

CACHE_MAX_ENTRIES <- 64L

#' Drop all cached market data. Exposed for tests and ops.
clear_data_cache <- function() {
  if (exists("entries", envir = .data_cache, inherits = FALSE)) {
    rm(list = "entries", envir = .data_cache)
  }
  invisible(NULL)
}

.data_cache_key <- function(ticker, start, end) {
  paste(ticker, format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d"), sep = "|")
}

.data_cache_get <- function(key) {
  if (!exists("entries", envir = .data_cache, inherits = FALSE)) {
    return(NULL)
  }

  entries <- get("entries", envir = .data_cache)

  # An environment's [[key]] throws "subscript out of bounds" when the
  # binding is absent. Check existence before reading so a normal cache miss
  # never becomes an HTTP 500.
  if (!exists(key, envir = entries, inherits = FALSE)) {
    return(NULL)
  }

  value <- get(key, envir = entries, inherits = FALSE)

  # Refresh insertion order (LRU-ish).
  rm(list = key, envir = entries)
  assign(key, value, envir = entries)
  value
}

.data_cache_set <- function(key, value) {
  if (!exists("entries", envir = .data_cache, inherits = FALSE)) {
    assign("entries", new.env(parent = emptyenv()), envir = .data_cache)
  }
  entries <- get("entries", envir = .data_cache)
  assign(key, value, envir = entries)
  n <- length(ls(entries, all.names = TRUE))
  if (n > CACHE_MAX_ENTRIES) {
    # Evict oldest keys (first in insertion order).
    keys <- ls(entries, all.names = TRUE)
    for (k in utils::head(keys, n - CACHE_MAX_ENTRIES)) {
      rm(list = k, envir = entries)
    }
  }
  invisible(value)
}

# ---------------------------------------------------------------------------
# Retrieval + sanitization
# ---------------------------------------------------------------------------

#' Fetch daily OHLCV with an ordered provider fallback chain.
#'
#' Provider order: Twelve Data -> Financial Modeling Prep -> Alpha Vantage ->
#' Stooq -> Yahoo Finance. Providers without configured keys are skipped.
#' Successful results are cached by ticker + date range in memory and optionally
#' on disk. Set MARKET_DATA_CACHE_DIR to a persistent mounted directory on the
#' hosting platform to keep the cache across process restarts/redeploys.

.provider_error <- function(message) structure(list(message = message), class = c("provider_error", "error", "condition"))

.cache_dir <- function() {
  d <- trimws(Sys.getenv("MARKET_DATA_CACHE_DIR", unset = file.path(tempdir(), "alphaforge_market_cache")))
  if (!nzchar(d)) d <- file.path(tempdir(), "alphaforge_market_cache")
  d
}

.disk_cache_path <- function(key) {
  safe <- gsub("[^A-Za-z0-9_.-]", "_", key)
  file.path(.cache_dir(), paste0(safe, ".rds"))
}

.disk_cache_get <- function(key) {
  path <- .disk_cache_path(key)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

.disk_cache_set <- function(key, value) {
  path <- .disk_cache_path(key)
  tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(path, ".tmp")
    saveRDS(value, tmp)
    file.rename(tmp, path)
  }, error = function(e) invisible(FALSE))
  invisible(value)
}

.json_get <- function(url, query) {
  if (!requireNamespace("httr", quietly = TRUE)) stop(.provider_error("R package 'httr' is not installed"))
  response <- httr::GET(url, query = query, httr::timeout(25))
  status <- httr::status_code(response)
  body <- httr::content(response, as = "text", encoding = "UTF-8")
  if (status >= 400) stop(.provider_error(sprintf("HTTP %s: %s", status, substr(body, 1, 300))))
  parsed <- jsonlite::fromJSON(body, simplifyDataFrame = TRUE)
  if (is.list(parsed) && !is.data.frame(parsed)) {
    notices <- unlist(parsed[intersect(c("Note", "Information", "Error Message", "message"), names(parsed))], use.names = FALSE)
    if (length(notices)) stop(.provider_error(paste(notices, collapse = " ")))
  }
  parsed
}

.rows_to_ohlcv <- function(df, date_col = "date", adjusted_col = NULL) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  names(df) <- tolower(names(df))
  date_col <- tolower(date_col)
  required <- c(date_col, "open", "high", "low", "close", "volume")
  if (!all(required %in% names(df))) stop(.provider_error(paste("Response is missing columns:", paste(setdiff(required, names(df)), collapse = ", "))))
  dates <- as.Date(as.character(df[[date_col]]))
  keep <- !is.na(dates)
  df <- df[keep, , drop = FALSE]; dates <- dates[keep]
  if (!nrow(df)) return(NULL)
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  adjusted <- if (!is.null(adjusted_col) && tolower(adjusted_col) %in% names(df)) num(df[[tolower(adjusted_col)]]) else num(df$close)
  values <- cbind(Open = num(df$open), High = num(df$high), Low = num(df$low),
                  Close = num(df$close), Adjusted = adjusted, Volume = num(df$volume))
  rownames(values) <- as.character(dates)
  xt <- xts::xts(values, order.by = dates)
  xt <- xt[order(zoo::index(xt)), , drop = FALSE]
  xt[!duplicated(zoo::index(xt)), , drop = FALSE]
}

.fetch_twelve_data <- function(ticker, start, end) {
  key <- trimws(Sys.getenv("TWELVEDATA_API_KEY", unset = ""))
  if (!nzchar(key)) stop(.provider_error("TWELVEDATA_API_KEY not configured"))
  # Ask for the full supported daily history once per ticker; the outer cache
  # prevents subsequent calls for this ticker/range from spending more credits.
  full_key <- paste0("PROVIDER_FULL_TWELVE_DATA|", ticker)
  cached <- .data_cache_get(full_key)
  if (is.null(cached)) cached <- .disk_cache_get(full_key)
  if (is.null(cached)) {
    parsed <- .json_get("https://api.twelvedata.com/time_series", list(
      symbol = ticker, interval = "1day", outputsize = 5000, order = "ASC", apikey = key
    ))
    if (!is.null(parsed$status) && identical(tolower(as.character(parsed$status)), "error")) stop(.provider_error(parsed$message %||% "Twelve Data returned an error"))
    values <- parsed$values
    if (is.null(values) || !nrow(values)) stop(.provider_error("Twelve Data returned no daily values"))
    cached <- .rows_to_ohlcv(values, "datetime")
    if (is.null(cached) || !NROW(cached)) stop(.provider_error("Twelve Data returned no usable OHLCV rows"))
    .data_cache_set(full_key, cached); .disk_cache_set(full_key, cached)
  } else { .data_cache_set(full_key, cached) }
  cached[paste0(format(start, "%Y-%m-%d"), "/", format(end, "%Y-%m-%d"))]
}

.fetch_fmp <- function(ticker, start, end) {
  key <- trimws(Sys.getenv("FMP_API_KEY", unset = ""))
  if (!nzchar(key)) stop(.provider_error("FMP_API_KEY not configured"))
  full_key <- paste0("PROVIDER_FULL_FMP|", ticker)
  cached <- .data_cache_get(full_key)
  if (is.null(cached)) cached <- .disk_cache_get(full_key)
  if (is.null(cached)) {
    parsed <- .json_get("https://financialmodelingprep.com/stable/historical-price-eod/full", list(
      symbol = ticker, from = "1990-01-01", to = format(Sys.Date(), "%Y-%m-%d"), apikey = key
    ))
    rows <- if (is.data.frame(parsed)) parsed else if (is.data.frame(parsed$historical)) parsed$historical else if (is.data.frame(parsed$data)) parsed$data else NULL
    cached <- .rows_to_ohlcv(rows, "date", "adjClose")
    if (is.null(cached) || !NROW(cached)) stop(.provider_error("FMP returned no usable daily OHLCV rows (the endpoint may not be included in your plan)"))
    .data_cache_set(full_key, cached); .disk_cache_set(full_key, cached)
  } else { .data_cache_set(full_key, cached) }
  cached[paste0(format(start, "%Y-%m-%d"), "/", format(end, "%Y-%m-%d"))]
}

.fetch_alpha_vantage <- function(ticker, start, end) {
  key <- trimws(Sys.getenv("ALPHAVANTAGE_API_KEY", unset = ""))
  if (!nzchar(key)) stop(.provider_error("ALPHAVANTAGE_API_KEY not configured"))
  full_key <- paste0("PROVIDER_FULL_ALPHA_VANTAGE|", ticker)
  cached <- .data_cache_get(full_key)
  if (is.null(cached)) cached <- .disk_cache_get(full_key)
  if (is.null(cached)) {
    av <- quantmod::getSymbols(ticker, src = "av", api.key = key, auto.assign = FALSE, warnings = FALSE)
    if (is.null(av) || !NROW(av)) stop(.provider_error("Alpha Vantage returned no daily data"))
    cached <- av
    .data_cache_set(full_key, cached); .disk_cache_set(full_key, cached)
  } else { .data_cache_set(full_key, cached) }
  cached[paste0(format(start, "%Y-%m-%d"), "/", format(end, "%Y-%m-%d"))]
}

.fetch_stooq <- function(ticker, start, end) {
  # Stooq's downloadable daily CSV endpoint does not require an API key.
  # Do not attach an apikey parameter: this provider is keyless.
  symbol <- if (grepl("\\.", ticker)) ticker else paste0(ticker, ".US")
  url <- "https://stooq.com/q/d/l/"
  if (!requireNamespace("httr", quietly = TRUE)) stop(.provider_error("R package 'httr' is not installed"))
  response <- httr::GET(
    url,
    query = list(s = tolower(symbol), d1 = format(start, "%Y%m%d"),
                 d2 = format(end, "%Y%m%d"), i = "d"),
    httr::timeout(25)
  )
  body <- httr::content(response, as = "text", encoding = "UTF-8")
  if (httr::status_code(response) >= 400 ||
      grepl("no data|error|api key", body, ignore.case = TRUE)) {
    stop(.provider_error(paste("Stooq response:", substr(body, 1, 200))))
  }
  df <- tryCatch(utils::read.csv(text = body, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  out <- .rows_to_ohlcv(df, "date")
  if (is.null(out) || !NROW(out)) stop(.provider_error("Stooq returned no usable daily OHLCV rows"))
  out
}

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(as.character(x[[1]]))) y else x

fetch_ohlcv <- function(ticker, start, end) {
  params <- validate_market_data_params(ticker, start, end)
  t <- params$ticker; s <- params$start; e <- params$end
  key <- .data_cache_key(t, s, e)
  cached <- .data_cache_get(key)
  if (is.null(cached)) cached <- .disk_cache_get(key)
  if (!is.null(cached)) {
    .data_cache_set(key, cached)
    return(cached)
  }

  # Explicit provider selection can be used for debugging; 'auto' walks the
  # configured providers in order and continues after quota/network failures.
  setting <- tolower(trimws(Sys.getenv("MARKET_DATA_PROVIDER", unset = "auto")))
  allowed <- c("auto", "twelvedata", "fmp", "alphavantage", "stooq", "yahoo")
  if (!setting %in% allowed) stop(data_error(paste0("Invalid MARKET_DATA_PROVIDER. Use: ", paste(allowed, collapse = ", ")), 500L))
  providers <- if (setting == "auto") c("twelvedata", "fmp", "alphavantage", "stooq", "yahoo") else setting
  api_keys <- c(twelvedata = "TWELVEDATA_API_KEY", fmp = "FMP_API_KEY", alphavantage = "ALPHAVANTAGE_API_KEY")
  fetchers <- list(twelvedata = .fetch_twelve_data, fmp = .fetch_fmp, alphavantage = .fetch_alpha_vantage,
                   stooq = .fetch_stooq, yahoo = function(ticker, start, end) quantmod::getSymbols(ticker, src = "yahoo", from = start, to = end, auto.assign = FALSE, warnings = FALSE))
  failures <- character()
  raw <- NULL; selected <- NULL
  for (provider in providers) {
    env_key <- api_keys[[provider]]
    if (!is.null(env_key) && !nzchar(trimws(Sys.getenv(env_key, unset = "")))) {
      failures <- c(failures, paste0(provider, ": API key not configured"))
      next
    }
    candidate <- tryCatch(fetchers[[provider]](t, s, e), error = function(err) {
      msg <- conditionMessage(err)
      cat(sprintf("[market-data] provider=%s ticker=%s range=%s..%s failed: %s\n", provider, t, format(s), format(e), msg))
      failures <<- c(failures, paste0(provider, ": ", msg))
      NULL
    })
    if (!is.null(candidate) && NROW(candidate) > 0) {
      raw <- candidate; selected <- provider; break
    }
  }
  if (is.null(raw) || NROW(raw) == 0) {
    detail <- paste(failures, collapse = " | ")
    stop(data_error(sprintf("No market-data provider could retrieve '%s'. Providers tried: %s. Check configured API keys, free-tier access, and provider status.", t, detail), 502L))
  }

  # Ensure the requested dates are respected even if a provider returned full history.
  raw <- raw[paste0(format(s, "%Y-%m-%d"), "/", format(e, "%Y-%m-%d"))]
  if (is.null(raw) || NROW(raw) == 0) stop(data_error(sprintf("No market data returned for '%s' in the requested date range.", t), 404L))

  cn <- colnames(raw)
  field_of <- function(nm) utils::tail(strsplit(nm, ".", fixed = TRUE)[[1]], 1)
  field_names <- vapply(cn, field_of, character(1))
  col_index <- function(want) { hit <- which(field_names == want); if (!length(hit)) NULL else hit[[1]] }
  i_open <- col_index("Open"); i_high <- col_index("High"); i_low <- col_index("Low")
  i_close <- col_index("Close"); i_volume <- col_index("Volume"); i_adjusted <- col_index("Adjusted")
  if (is.null(i_open) || is.null(i_high) || is.null(i_low) || is.null(i_close) || is.null(i_volume)) {
    stop(data_error(sprintf("Market data from %s for '%s' is missing required OHLCV columns.", selected, t), 502L))
  }
  adjusted_col <- if (!is.null(i_adjusted)) i_adjusted else i_close
  combined <- cbind(Open = raw[, i_open], High = raw[, i_high], Low = raw[, i_low],
                    Close = raw[, i_close], Adjusted = raw[, adjusted_col], Volume = raw[, i_volume])
  colnames(combined) <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")
  combined <- combined[order(zoo::index(combined)), , drop = FALSE]
  sanitized <- sanitize_ohlcv(combined, ticker = t)
  .data_cache_set(key, sanitized)
  .disk_cache_set(key, sanitized)
  attr(sanitized, "market_data_provider") <- selected
  sanitized
}

#' Apply the missing-data policy (IMPLEMENTATION_PLAN.md section L).
#'
#' - Calendar gaps (weekends/holidays) are normal and untouched.
#' - NA prices: if total NA < 5% of rows and NAs are isolated (no run longer
#'   than 1), linearly interpolate. Otherwise the request is rejected.
#' - NA volume is set to 0 when price data is present; pervasive NA volume
#'   also triggers rejection.
#'
#' @param x Canonical xts (Open/High/Low/Close/Adjusted/Volume).
#' @param ticker Ticker label for error messages.
#' @return Sanitized xts, or throws data_error.
sanitize_ohlcv <- function(x, ticker = "?") {
  n <- NROW(x)
  price_cols <- c("Open", "High", "Low", "Close", "Adjusted")
  total_na <- sum(is.na(x[, price_cols]))
  pct_na <- total_na / (n * length(price_cols)) * 100

  if (n > 0 && pct_na >= 5) {
    stop(data_error(sprintf(
      "Market data for '%s' has too many missing values (%.1f%% of price points) to be usable; adjust the date range.",
      ticker, pct_na
    )))
  }

  # Per-column interpolation only when NA runs are isolated (maxgap = 1) and
  # under the 5% total budget. zoo::na.approx leaves longer runs as NA, which
  # the callers must handle; with runs of 1 the totals stay far below 5%.
  for (col in price_cols) {
    x[, col] <- zoo::na.approx(x[, col], na.rm = FALSE, maxgap = 1)
  }
  remaining <- sum(is.na(x[, price_cols]))
  if (remaining > 0) {
    stop(data_error(sprintf(
      "Market data for '%s' contains clustered missing values that cannot be filled safely; adjust the date range.",
      ticker
    )))
  }

  # Volume: NA -> 0; pervasive NA volume (>= 5%) rejects.
  if (sum(is.na(x[, "Volume"])) / n >= 0.05) {
    stop(data_error(sprintf(
      "Market data for '%s' has pervasive missing volume data.", ticker
    )))
  }
  x[is.na(x[, "Volume"]), "Volume"] <- 0

  x
}

# ---------------------------------------------------------------------------
# Serialization
# ---------------------------------------------------------------------------

#' Convert a canonical OHLCV xts into a frontend-friendly R list.
#'
#' @param x Canonical xts (from fetch_ohlcv).
#' @param ticker Normalized ticker.
#' @param start Requested start date (Date).
#' @param end Requested end date (Date).
#' @return Nested list ready for jsonlite; dates are "YYYY-MM-DD".
ohlcv_to_list <- function(x, ticker, start, end) {
  idx <- zoo::index(x)
  rows <- lapply(seq_len(NROW(x)), function(i) {
    list(
      date = format(idx[i], "%Y-%m-%d"),
      open = as.numeric(x[i, "Open"]),
      high = as.numeric(x[i, "High"]),
      low = as.numeric(x[i, "Low"]),
      close = as.numeric(x[i, "Close"]),
      adjusted_close = as.numeric(x[i, "Adjusted"]),
      volume = as.numeric(x[i, "Volume"])
    )
  })
  list(
    ticker = ticker,
    start = format(as.Date(start), "%Y-%m-%d"),
    end = format(as.Date(end), "%Y-%m-%d"),
    count = length(rows),
    data = rows
  )
}

# ---------------------------------------------------------------------------
# Ticker search (Yahoo Finance symbol lookup, autocomplete endpoint)
# ---------------------------------------------------------------------------

#' Parse a raw Yahoo Finance search JSON payload into suggestion rows
#' (pure function — no network; unit-testable).
#'
#' Filters to EQUITY/ETF/INDEX quote types, drops symbols that fail the
#' shared ticker format rule, normalizes to uppercase, de-duplicates and
#' falls back from shortname to longname to the symbol itself for names.
#'
#' @param txt Raw JSON text from the search endpoint (or anything else —
#'   unparseable input yields an empty result, never an error).
#' @param limit Maximum number of rows to return.
#' @return A (possibly empty) list of {symbol, name, exchange} lists.
.parse_ticker_search <- function(txt, limit = 5L) {
  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                     error = function(e) NULL)
  quotes <- parsed$quotes
  if (is.null(quotes)) quotes <- list()

  # Keep only tradable/index quotes whose symbol satisfies the same format
  # rule the rest of the API enforces, then normalize + de-duplicate.
  seen <- new.env(parent = emptyenv())
  results <- list()
  for (item in quotes) {
    if (length(results) >= limit) break
    if (is.null(item$symbol) || is.null(item$shortname)) next
    qt <- toupper(trimws(as.character(item$quoteType)))
    if (!qt %in% c("EQUITY", "ETF", "INDEX")) next
    sym <- tryCatch(normalize_ticker(item$symbol), error = function(e) NULL)
    if (is.null(sym) || !grepl(TICKER_REGEX, sym) || !is.null(seen[[sym]])) next
    seen[[sym]] <- TRUE
    name <- trimws(as.character(item$shortname))
    if (!nzchar(name)) name <- trimws(as.character(item$longname))
    if (is.null(name) || is.na(name) || !nzchar(name)) name <- sym
    results[[length(results) + 1L]] <- list(
      symbol = sym,
      name = name,
      exchange = if (is.null(item$exchDisp)) NULL else trimws(as.character(item$exchDisp))
    )
  }
  results
}

#' Search Yahoo Finance for ticker symbols matching a query.
#'
#' Uses the public Yahoo Finance search endpoint (query1.finance.yahoo.com),
#' the same data source quantmod retrieves prices from. Results are cached
#' in memory (query-keyed, LRU) to stay lightweight under keystroke traffic.
#'
#' @param query Raw user query (e.g. "RELI", "AAPL").
#' @param limit Maximum number of suggestions to return (default 5, max 10).
#' @return list(query, count, results = [{symbol, name, exchange}]).
#' @throws data_error (400 on bad query, 502 on source failure).
search_tickers <- function(query, limit = 5L) {
  if (is.null(query) || length(query) == 0 || !nzchar(trimws(as.character(query)))) {
    stop(data_error("q (search query) is required."))
  }
  q <- trimws(as.character(query))
  limit <- max(1L, min(as.integer(limit), 10L, na.rm = TRUE))

  cache_key <- paste0("search|", toupper(q), "|", limit)
  cached <- .data_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  url <- sprintf(
    "https://query1.finance.yahoo.com/v1/finance/search?q=%s&quotesCount=%d&newsCount=0",
    utils::URLencode(q, reserved = TRUE), limit * 3L
  )
  txt <- tryCatch(
    {
      con <- url(url, method = "libcurl", headers = c(
        "User-Agent" = "Mozilla/5.0 (AlphaForge Insight ticker search)"
      ))
      on.exit(close(con), add = TRUE)
      paste(readLines(con, warn = FALSE), collapse = "\n")
    },
    error = function(err) {
      cat("[ticker-search] fetch error for query '", q, "': ",
          conditionMessage(err), "\n", sep = "")
      stop(data_error(
        "Ticker search is temporarily unavailable; please try again later.",
        status = 502L
      ))
    }
  )

  results <- .parse_ticker_search(txt, limit)

  out <- list(query = q, count = length(results), results = results)
  .data_cache_set(cache_key, out)
  out
}

#' Validate that a ticker actually yields usable market data.
#'
#' Format is checked first (client error). If the format is acceptable, a real
#' (small) data window is retrieved to confirm the ticker resolves. Validity is
#' never fabricated; a data-source outage is reported honestly.
#'
#' @param ticker Raw ticker.
#' @return list(valid = TRUE, ticker = <normalized>) or
#'   list(valid = FALSE, ticker = <normalized>, error = <message>).
validate_ticker <- function(ticker) {
  t <- tryCatch(validate_ticker_format(ticker), data_error = function(e) NULL)
  if (is.null(t)) {
    msg <- tryCatch({
      validate_ticker_format(ticker)
      NULL
    }, data_error = function(e) conditionMessage(e))
    return(list(valid = FALSE, ticker = normalize_ticker(ticker), error = msg))
  }

  # Small recent window is enough to prove the ticker resolves.
  to_date <- Sys.Date()
  from_date <- to_date - 400L
  tryCatch(
    {
      xt <- fetch_ohlcv(t, format(from_date, "%Y-%m-%d"), format(to_date, "%Y-%m-%d"))
      if (NROW(xt) == 0) {
        list(valid = FALSE, ticker = t,
             error = sprintf("No market data found for ticker '%s'.", t))
      } else {
        list(valid = TRUE, ticker = t)
      }
    },
    data_error = function(e) {
      list(valid = FALSE, ticker = t, error = conditionMessage(e))
    },
    error = function(e) {
      list(valid = FALSE, ticker = t,
           error = "Unable to verify ticker at this time; the data source may be unavailable.")
    }
  )
}