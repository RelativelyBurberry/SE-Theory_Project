import apiClient from './apiClient'

/**
 * Strategy API service (Phase 4) — uses the single centralized Axios client
 * (src/services/apiClient.js).
 *
 *   GET  /strategies/defaults  (public)  documented parameter defaults
 *   POST /strategies/preview   (JWT)     validate + indicators + signals
 *
 * POST /strategies/preview does NOT execute trades and does NOT return
 * portfolio performance — those arrive with the backtesting engine (Phase 5).
 */

/**
 * GET /strategies/defaults
 * Returns { success, defaults: { ma_crossover: {fast_period, slow_period},
 *                                rsi: {period, oversold, overbought} } }
 */
export async function getStrategyDefaults() {
  const { data } = await apiClient.get('/strategies/defaults')
  return data
}

/**
 * POST /strategies/preview
 * Request: {
 *   ticker, start, end, initialCapital, strategy,
 *   parameters: { fast_period, slow_period } | { period, oversold, overbought }
 * }
 * Returns {
 *   success, ticker, start, end, strategy, parameters,
 *   row_count, signal_count,
 *   signals: [{ date, close, [fast_sma, slow_sma | rsi], [signal] }, ...]
 * }
 * Rows omit indicator/signal keys when the value is unavailable (warm-up
 * period or HOLD); a missing "signal" key means no new signal that day.
 */
export async function previewStrategy(payload) {
  const { data } = await apiClient.post('/strategies/preview', payload)
  return data
}

export default { getStrategyDefaults, previewStrategy }