# AlphaForge Insight — Live News Provider Adapter (Phase H.4)
#
# Fetches recent financial news from Marketaux API (free tier).
# - Headlines, URLs, publishers, timestamps, ticker relevance
# - Sentiment when available (field may be absent)
# - In-memory caching to respect 100 requests/day limit
# - Graceful handling of rate limits, missing fields, errors
#
# Functions:
#   fetch_live_news(tickers, limit)    Get recent news for tickers
#   clear_news_cache()                 Drop cached responses (for tests)
#
# Environment:
#   MARKETAUX_API_KEY — API token from https://www.marketaux.com/register
#
# Constraints:
#   - Free tier: 100 requests/day, 3 articles/request
#   - API key must be set in MARKETAUX_API_KEY environment variable
#   - No client-side API calls (key must stay on backend)
#   - Caching required to stay within daily rate limit

# ---------------------------------------------------------------------------
# Structured errors
# ---------------------------------------------------------------------------
library(jsonlite)

news_error <- function(message, status = 400L) {
  structure(
    list(message = message, status = as.integer(status)),
    class = c("news_error", "error", "condition")
  )
}

# ---------------------------------------------------------------------------
# In-memory cache (LRU with TTL)
# ---------------------------------------------------------------------------
.news_cache <- new.env(parent = emptyenv())
NEWS_CACHE_TTL_SECONDS <- 900L  # 15 minutes (freshness vs. rate limit tradeoff)
NEWS_CACHE_MAX_ENTRIES <- 64L

clear_news_cache <- function() {
  if (exists("entries", envir = .news_cache, inherits = FALSE)) {
    rm(list = "entries", envir = .news_cache)
  }
  invisible(NULL)
}

.news_cache_key <- function(tickers, limit) {
  paste(toupper(paste(sort(unique(tickers)), collapse = ",")), limit, sep = "|")
}

.news_cache_get <- function(key) {
  if (!exists("entries", envir = .news_cache, inherits = FALSE)) {
    return(NULL)
  }
  entries <- get("entries", envir = .news_cache)
  cached <- entries[[key]]
  if (is.null(cached)) return(NULL)
  
  # Check TTL
  age_seconds <- as.numeric(difftime(Sys.time(), cached$timestamp, units = "secs"))
  if (age_seconds > NEWS_CACHE_TTL_SECONDS) {
    rm(list = key, envir = .news_cache$entries)
    return(NULL)
  }
  
  # Refresh insertion order (LRU)
  rm(list = key, envir = .news_cache$entries)
  assign(key, cached, envir = .news_cache$entries)
  cached
}

.news_cache_set <- function(key, value) {
  if (!exists("entries", envir = .news_cache, inherits = FALSE)) {
    assign("entries", new.env(parent = emptyenv()), envir = .news_cache)
  }
  entries <- get("entries", envir = .news_cache)
  assign(key, value, envir = entries)
  
  # Evict oldest if over limit
  n <- length(ls(entries, all.names = TRUE))
  if (n > NEWS_CACHE_MAX_ENTRIES) {
    keys <- ls(entries, all.names = TRUE)
    for (k in utils::head(keys, n - NEWS_CACHE_MAX_ENTRIES)) {
      rm(list = k, envir = entries)
    }
  }
  invisible(value)
}

# ---------------------------------------------------------------------------
# API configuration
# ---------------------------------------------------------------------------
MARKETAUX_BASE_URL <- "https://api.marketaux.com/v1/news/all"
MARKETAUX_FREE_TIER_LIMIT <- 3L  # Max articles per request on free tier

.get_api_key <- function() {
  key <- Sys.getenv("MARKETAUX_API_KEY", unset = "")
  if (!nzchar(key)) {
    stop(news_error(
      "MARKETAUX_API_KEY is not configured. Get a free API token at https://www.marketaux.com/register",
      status = 500L
    ))
  }
  key
}

# ---------------------------------------------------------------------------
# Ticker validation
# ---------------------------------------------------------------------------
.validate_news_tickers <- function(tickers) {
  if (is.null(tickers) || length(tickers) == 0) {
    stop(news_error("tickers parameter is required."))
  }
  
  # Normalize to uppercase, trim whitespace
  tickers <- toupper(trimws(as.character(tickers)))
  tickers <- tickers[nzchar(tickers)]
  
  if (length(tickers) == 0) {
    stop(news_error("At least one valid ticker is required."))
  }
  
  # Basic format validation (letters, digits, dots, hyphens)
  valid_pattern <- "^[A-Z0-9][A-Z0-9.-]{0,19}$"
  invalid <- tickers[!grepl(valid_pattern, tickers)]
  if (length(invalid) > 0) {
    stop(news_error(sprintf(
      "Invalid ticker format: %s. Tickers must be 1-20 characters (letters, digits, ., -).",
      paste(invalid, collapse = ", ")
    )))
  }
  
  tickers
}

# ---------------------------------------------------------------------------
# API request
# ---------------------------------------------------------------------------
.fetch_from_marketaux <- function(tickers, limit) {
  api_key <- .get_api_key()
  
  # Build URL
  symbols_param <- paste(tickers, collapse = ",")
  limit_param <- min(as.integer(limit), MARKETAUX_FREE_TIER_LIMIT)
  
  url <- sprintf(
    "%s?api_token=%s&symbols=%s&limit=%d",
    MARKETAUX_BASE_URL,
    utils::URLencode(api_key, reserved = TRUE),
    utils::URLencode(symbols_param, reserved = TRUE),
    limit_param
  )
  
  # Make request
  response <- tryCatch(
    {
      con <- url(url, method = "libcurl", headers = c(
        "User-Agent" = "AlphaForge-Insight/1.0 (Live News)"
      ))
      on.exit(close(con), add = TRUE)
      txt <- paste(readLines(con, warn = FALSE), collapse = "\n")
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    },
    error = function(err) {
      msg <- conditionMessage(err)
      cat("[live-news] API request failed:", msg, "\n")
      stop(news_error(
        "Unable to fetch live news. The news provider may be temporarily unavailable.",
        status = 502L
      ))
    }
  )
  
  response
}

# ---------------------------------------------------------------------------
# Response parsing
# ---------------------------------------------------------------------------
.parse_marketaux_response <- function(response, tickers, retrieval_time) {
  # Check for API errors
  if (!is.null(response$error)) {
    msg <- response$error$message
    if (is.null(msg)) msg <- "Unknown API error"
    
    # Rate limit
    if (response$error$code == "rate_limit_exceeded" || 
        grepl("rate limit", msg, ignore.case = TRUE)) {
      stop(news_error(
        "News API rate limit exceeded. Please try again later.",
        status = 429L
      ))
    }
    
    # Invalid API key
    if (response$error$code == "invalid_api_key" ||
        grepl("invalid.*key", msg, ignore.case = TRUE)) {
      stop(news_error(
        "Invalid MARKETAUX_API_KEY. Please check your API token.",
        status = 401L
      ))
    }
    
    stop(news_error(sprintf("News API error: %s", msg), status = 502L))
  }
  
  # Extract articles
  articles <- response$data
  if (is.null(articles)) articles <- list()
  
  # Normalize each article
  normalized <- lapply(articles, function(article) {
    # Extract entity/ticker relevance
    entities <- article$entities
    related_tickers <- c()
    if (!is.null(entities) && length(entities) > 0) {
      for (entity in entities) {
        if (!is.null(entity$symbol) && entity$symbol %in% tickers) {
          related_tickers <- c(related_tickers, entity$symbol)
        }
      }
    }
    related_tickers <- unique(related_tickers)
    
    # Build normalized article
    list(
      headline = article$title,
      url = article$url,
      publisher = article$source,
      published_at = article$published_at,
      related_tickers = related_tickers,
      snippet = article$description,
      sentiment = article$sentiment,  # May be NULL
      provider = "marketaux",
      retrieved_at = retrieval_time
    )
  })
  
  # Filter to articles with required fields
  filtered <- Filter(function(a) {
    !is.null(a$headline) && !is.null(a$url) && !is.null(a$publisher)
  }, normalized)
  
  filtered
}

# ---------------------------------------------------------------------------
# Main API function
# ---------------------------------------------------------------------------

#' Fetch recent live news for specified tickers from Marketaux.
#'
#' @param tickers Character vector of ticker symbols (e.g., c("AAPL", "MSFT")).
#' @param limit Maximum number of articles to return (default 3, max 3 on free tier).
#' @return List with: success, provider, tickers, count, retrieval_time,
#'         cached, articles = [{headline, url, publisher, published_at,
#'         related_tickers, snippet, sentiment, provider, retrieved_at}]
#' @throws news_error on validation errors, missing API key, rate limit, or API failure.
fetch_live_news <- function(tickers, limit = 3L) {
  # Validate tickers
  tickers <- .validate_news_tickers(tickers)
  
  # Validate limit
  limit <- max(1L, min(as.integer(limit), MARKETAUX_FREE_TIER_LIMIT))
  
  # Check cache
  cache_key <- .news_cache_key(tickers, limit)
  cached <- .news_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached$data)
  }
  
  # Record retrieval time
  retrieval_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # Fetch from API
  response <- .fetch_from_marketaux(tickers, limit)
  
  # Parse response
  articles <- .parse_marketaux_response(response, tickers, retrieval_time)
  
  # Build result
  result <- list(
    success = TRUE,
    provider = "marketaux",
    tickers = tickers,
    count = length(articles),
    retrieval_time = retrieval_time,
    cached = FALSE,
    articles = articles
  )
  
  # Cache result
  .news_cache_set(cache_key, list(data = result, timestamp = Sys.time()))
  
  result
}

# ---------------------------------------------------------------------------
# Utility: redact API key from logs/strings
# ---------------------------------------------------------------------------
redact_api_key <- function(text, api_key = NULL) {
  if (is.null(text)) return(NULL)
  if (is.null(api_key)) {
    api_key <- Sys.getenv("MARKETAUX_API_KEY", unset = "")
  }
  if (!nzchar(api_key) || !nzchar(text)) return(text)
  
  # Replace API key with [REDACTED]
  gsub(api_key, "[REDACTED]", text, fixed = TRUE)
}
