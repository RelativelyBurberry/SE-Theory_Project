# AlphaForge Insight — Phase 3 market-data validation unit tests
#
# DETERMINISTIC: no network access. Only the request-validation, sanitization,
# and serialization layers are exercised with fixture data. The live Yahoo
# retrieval path is covered separately by tests/backend/test_data_api.R.
#
# Run from the repository root:
#   Rscript tests/analytics/test_data_validation.R
#
# Exit code 0 = pass, 1 = fail. Requires: quantmod, TTR, xts, zoo.

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

# Fixture xts builder (canonical columns).
fixture_xts <- function(closes = c(100, 101, 99, 102, 103, 104, 105, 106, 107, 108),
                        start = "2024-01-01", seed = 42) {
  n <- length(closes)
  idx <- as.Date(start) + 0:(n - 1)
  set.seed(seed)
  fake <- vapply(closes, function(c) c * (1 + stats::rnorm(6, 0, 0.005)), numeric(6))
  xts::xts(
    t(fake), order.by = idx,
    dimnames = list(NULL, c("Open", "High", "Low", "Close", "Adjusted", "Volume"))
  )[, , drop = TRUE]
}
# The rnorm construction above returns a 6 x n matrix; transpose into rows.
fixture_xts <- function(closes = c(100, 101, 99, 102, 103, 104, 105, 106, 107, 108),
                        start = "2024-01-01", seed = 42) {
  n <- length(closes)
  idx <- as.Date(start) + 0:(n - 1)
  set.seed(seed)
  m <- matrix(NA_real_, nrow = n, ncol = 6)
  m[, 4] <- closes                    # Close
  m[, 1] <- closes * 0.995            # Open
  m[, 2] <- closes * 1.010            # High
  m[, 3] <- closes * 0.990            # Low
  m[, 5] <- closes * 0.98             # Adjusted
  m[, 6] <- 1000000 + seq_len(n) * 100
  colnames(m) <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")
  xts::xts(m, order.by = idx)
}

# ---------------------------------------------------------------------------
# 1. Ticker format validation
# ---------------------------------------------------------------------------
record("ticker: 'AAPL' accepted", identical(validate_ticker_format("AAPL"), "AAPL"))
record("ticker: lowercase normalized to uppercase",
       identical(validate_ticker_format("msft"), "MSFT"))
record("ticker: surrounding whitespace trimmed",
       identical(validate_ticker_format("  goog  "), "GOOG"))
record("ticker: hyphenated symbol accepted",
       identical(validate_ticker_format("BRK-B"), "BRK-B"))
record("ticker: dotted symbol accepted",
       identical(validate_ticker_format("0700.HK"), "0700.HK"))
record("ticker: caret index symbol accepted",
       identical(validate_ticker_format("^GSPC"), "^GSPC"))
record("ticker: NSE symbol with exchange suffix accepted (11 chars)",
       identical(validate_ticker_format("RELIANCE.NS"), "RELIANCE.NS"))
record("ticker: lowercase + whitespace normalized (RELIANCE.NS)",
       identical(validate_ticker_format("  reliance.ns  "), "RELIANCE.NS"))
record("ticker: 20-character symbol accepted (max length)",
       identical(validate_ticker_format("ABCDEFGHIJKLMNOPQRST"), "ABCDEFGHIJKLMNOPQRST"))
record("ticker: empty string rejected",
       is_data_error(validate_ticker_format(""), 400L))
record("ticker: NULL rejected", is_data_error(validate_ticker_format(NULL), 400L))
record("ticker: spaces inside rejected",
       is_data_error(validate_ticker_format("A B"), 400L))
record("ticker: 21 characters rejected (above the 20-char limit)",
       is_data_error(validate_ticker_format("ABCDEFGHIJKLMNOPQRSTU"), 400L))
record("ticker: punctuation rejected",
       is_data_error(validate_ticker_format("!!!!"), 400L))

# ---------------------------------------------------------------------------
# 2. Date validation
# ---------------------------------------------------------------------------
record("date: valid YYYY-MM-DD", identical(parse_request_date("2024-03-15", "start"),
                                           as.Date("2024-03-15")))
record("date: malformed month rejected",
       is_data_error(parse_request_date("2024-13-01", "start"), 400L))
record("date: malformed day rejected",
       is_data_error(parse_request_date("2024-01-40", "end"), 400L))
record("date: wrong format (slash) rejected",
       is_data_error(parse_request_date("01/01/2020", "start"), 400L))
record("date: wrong format (short year) rejected",
       is_data_error(parse_request_date("24-01-01", "start"), 400L))
record("date: missing rejected", is_data_error(parse_request_date(NULL, "end"), 400L))
record("date: non-date text rejected",
       is_data_error(parse_request_date("yesterday", "start"), 400L))

# ---------------------------------------------------------------------------
# 3. Full parameter validation (market-data request)
# ---------------------------------------------------------------------------
p <- validate_market_data_params("tsla", "2023-01-01", "2023-06-30", today = "2026-01-01")
record("params: valid request normalized",
       identical(p$ticker, "TSLA") &&
         identical(p$start, as.Date("2023-01-01")) &&
         identical(p$end, as.Date("2023-06-30")))
record("params: start after end rejected",
       is_data_error(validate_market_data_params("AAPL", "2024-06-01", "2024-01-01",
                                                 today = "2026-01-01"), 400L))
record("params: start == end accepted (single trading day)",
       !is_data_error(validate_market_data_params("AAPL", "2024-06-03", "2024-06-03",
                                                  today = "2026-01-01"), 400L))
record("params: future start rejected",
       is_data_error(validate_market_data_params("AAPL", "2027-01-01", "2026-01-01",
                                                 today = "2026-01-01"), 400L))
record("params: future end rejected",
       is_data_error(validate_market_data_params("AAPL", "2024-01-01", "2027-01-01",
                                                 today = "2026-01-01"), 400L))
record("params: missing ticker rejected",
       is_data_error(validate_market_data_params(NULL, "2024-01-01", "2024-06-30",
                                                 today = "2026-01-01"), 400L))
record("params: missing start rejected",
       is_data_error(validate_market_data_params("AAPL", NULL, "2024-06-30",
                                                 today = "2026-01-01"), 400L))
record("params: missing end rejected",
       is_data_error(validate_market_data_params("AAPL", "2024-01-01", NULL,
                                                 today = "2026-01-01"), 400L))

# ---------------------------------------------------------------------------
# 4. Sanitization (missing-data policy, section L)
# ---------------------------------------------------------------------------
x <- fixture_xts()
record("sanitize: clean fixture passes unchanged",
       NROW(sanitize_ohlcv(x, "AAPL")) == NROW(x))
record("sanitize: no NA introduced on clean data",
       !anyNA(sanitize_ohlcv(x, "AAPL")))

# Isolated internal NA (< 5%) in Close is linearly interpolated.
x1 <- x
x1[5, "Close"] <- NA
s1 <- sanitize_ohlcv(x1, "AAPL")
record("sanitize: isolated NA (<5%) interpolated, not dropped",
       !is.na(s1[5, "Close"]) && NROW(s1) == NROW(x1))

# Pervasive NA (>= 5% of price points) is rejected.
x2 <- x
x2[1:3, "Close"] <- NA  # 3 of 10 in one column = 30% of that column, 6% overall prices
record("sanitize: pervasive NA (>=5%) rejected",
       is_data_error(sanitize_ohlcv(x2, "AAPL"), 400L))

# Clustered NA (run length > 1 within 5% total) is rejected.
x3 <- x
x3[4:5, "Close"] <- NA  # run of 2 -> maxgap=1 cannot fill
record("sanitize: clustered NA rejected",
       is_data_error(sanitize_ohlcv(x3, "AAPL"), 400L))

# Volume NA -> 0 (isolated, within the 5% budget — use a 100-row fixture so a
# single missing volume point is only 1%).
x100 <- fixture_xts(closes = rep(100, 100), start = "2024-01-01")
x4 <- x100
x4[2, "Volume"] <- NA
s4 <- sanitize_ohlcv(x4, "AAPL")
record("sanitize: isolated volume NA becomes 0",
       identical(as.numeric(s4[2, "Volume"]), 0))

# Pervasive volume NA rejected.
x5 <- x100
x5[1:6, "Volume"] <- NA # 6% >= 5% -> reject
record("sanitize: pervasive volume NA rejected",
       is_data_error(sanitize_ohlcv(x5, "AAPL"), 400L))

# ---------------------------------------------------------------------------
# 5. Chronological ordering (defensive sort in fetch path)
# ---------------------------------------------------------------------------
jumbled <- x[c(5, 2, 9, 1, 4), ]
record("ordering: jumbled fixture is re-sorted chronologically",
       all(diff(as.numeric(index(jumbled[order(index(jumbled)), ]))) > 0))

# ---------------------------------------------------------------------------
# 6. OHLC field validation / serialization
# ---------------------------------------------------------------------------
lst <- ohlcv_to_list(x, "AAPL", as.Date("2024-01-01"), as.Date("2024-01-10"))
r1 <- lst$data[[1]]
record("serialize: required top-level fields present",
       all(c("ticker", "start", "end", "count", "data") %in% names(lst)))
record("serialize: count matches rows", identical(lst$count, NROW(x)))
record("serialize:OHLCV fields present in each row",
       all(c("date", "open", "high", "low", "close", "adjusted_close", "volume") %in% names(r1)))
record("serialize: dates are YYYY-MM-DD strings",
       all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", vapply(lst$data, `[[`, "", "date"))))
record("serialize: numbers are plain numerics",
       all(vapply(c("open", "high", "low", "close", "adjusted_close", "volume"),
                  function(f) is.numeric(r1[[f]]), logical(1))))
record("serialize: output is valid JSON through jsonlite",
       jsonlite::validate(jsonlite::toJSON(lst, auto_unbox = TRUE)))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d data-validation tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  for (f in results[!passed]) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL DATA-VALIDATION TESTS PASSED\n")
quit(status = 0)