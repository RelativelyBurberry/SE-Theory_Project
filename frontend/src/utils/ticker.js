/**
 * Shared ticker normalization + format validation (single frontend source).
 *
 * Mirrors the backend rule in backend/analytics/data_retrieval.R
 * (TICKER_REGEX / TICKER_MAX_LENGTH): Yahoo Finance symbols may include an
 * exchange suffix such as ".NS" / ".BO" / ".HK" and therefore legitimately
 * exceed 10 characters (RELIANCE.NS = 11, HDFCBANK.NS = 10). Maximum length
 * is 20; allowed characters are letters, digits, ".", "-" and "^" (caret as
 * the first character, for index symbols like ^GSPC).
 *
 * Used by BOTH ML pages (mlDefaults.validateMlForm) and the Strategy Builder
 * (strategyDefaults.validateStrategyForm) — do not duplicate the regex.
 */

/** Same constant as the backend's TICKER_MAX_LENGTH. */
export const TICKER_MAX_LENGTH = 20

/**
 * Ticker format rule (Yahoo Finance symbols).
 * Kept in one place; imported by mlDefaults.js and strategyDefaults.js.
 */
export const TICKER_REGEX = new RegExp(`^[A-Z0-9^][A-Z0-9.^-]{0,${TICKER_MAX_LENGTH - 1}}$`)

/** Maximum number of tickers in the ML portfolio universe (backend contract). */
export const MAX_UNIVERSE_SIZE = 10

/** Normalize a ticker: trim surrounding whitespace and uppercase. */
export function normalizeTicker(ticker) {
  return String(ticker ?? '').trim().toUpperCase()
}

/**
 * Validate a ticker's format only (no network call).
 * @param {string} ticker Raw user input.
 * @returns {boolean} true when the format is acceptable.
 */
export function isValidTickerFormat(ticker) {
  return TICKER_REGEX.test(normalizeTicker(ticker))
}

/**
 * Format-check error message shared by every ticker input.
 * @returns {string} Human-readable message for an invalid ticker.
 */
export function tickerFormatError() {
  return `Ticker must be 1–${TICKER_MAX_LENGTH} characters: letters, digits, ".", "-" or "^" (Yahoo Finance symbols like RELIANCE.NS are accepted).`
}
