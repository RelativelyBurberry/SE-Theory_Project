import apiClient from './apiClient'

/**
 * Market-data API service (Phase 3) — uses the single centralized Axios
 * client (src/services/apiClient.js). No second client is created.
 *
 * /stocks/validate is public; /market-data requires a valid JWT (attached
 * automatically by the apiClient request interceptor).
 */

/**
 * GET /stocks/validate?ticker=...
 * Returns { ticker, valid } or { ticker, valid: false, error }.
 */
export async function validateTicker(ticker) {
  const { data } = await apiClient.get('/stocks/validate', { params: { ticker } })
  return data
}

/**
 * GET /market/tickers/search?q=...&limit=5
 * Lightweight Yahoo Finance symbol lookup backing the ticker autocomplete.
 * Returns { success, query, count, results: [{ symbol, name, exchange? }] }.
 * Never fabricated: an unavailable source rejects, and the autocomplete UI
 * degrades gracefully (manual entry still works).
 */
export async function searchTickers(query, limit = 5) {
  const { data } = await apiClient.get('/market/tickers/search', {
    params: { q: query, limit },
    // Autocomplete requests are tiny; use a short timeout so a stalled
    // backend cannot leave the dropdown hanging.
    timeout: 8000,
  })
  return data
}

/**
 * GET /market-data?ticker=&start=&end=
 * Returns {
 *   success, ticker, start, end, count,
 *   data: [{ date, open, high, low, close, adjusted_close, volume }, ...]
 * }
 */
export async function fetchMarketData(ticker, start, end) {
  const { data } = await apiClient.get('/market-data', {
    params: { ticker, start, end },
  })
  return data
}

export default { validateTicker, searchTickers, fetchMarketData }