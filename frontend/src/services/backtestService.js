import apiClient from './apiClient'

/**
 * Backtest API service (Phase 6).
 *
 * POST /backtest/run        Run a full backtest (JWT required)
 * GET  /backtest/history    Paginated user backtest history (JWT required)
 * GET  /backtest/:id        Full backtest record by ID (JWT required)
 */

/**
 * POST /backtest/run
 * Request: {
 *   ticker, start, end, initialCapital, strategy,
 *   parameters: { fast_period, slow_period } | { period, oversold, overbought }
 * }
 * Response: {
 *   success, backtestId, ticker, strategy, start, end, parameters,
 *   metrics: { initialCapital, finalValue, totalReturn, cagr, sharpe, sortino,
 *              maximumDrawdown, winRate, volatility, totalTrades },
 *   equityCurve: [{ date, portfolioValue, cash, shares, close, dailyReturn? }],
 *   tradeHistory: [{ tradeId, signalDate, executionDate, side, executionPrice,
 *                    shares, cost, cashAfter, positionAfter }]
 * }
 */
export async function runBacktest(payload) {
  const { data } = await apiClient.post('/backtest/run', payload)
  return data
}

/**
 * GET /backtest/history
 * Query params: limit (default 20), offset (default 0)
 * Response: {
 *   success, items: [{id, ticker, strategy, startDate, endDate, initialCapital,
 *                     createdAt, totalReturn?, sharpeRatio?, totalTrades?}],
 *   total, limit, offset
 * }
 */
export async function getBacktestHistory(limit = 20, offset = 0) {
  const { data } = await apiClient.get('/backtest/history', {
    params: { limit, offset },
  })
  return data
}

/**
 * GET /backtest/:id
 * Response: {
 *   success, id, ticker, strategy, startDate, endDate, initialCapital,
 *   parameters, createdAt,
 *   metrics: { totalReturn, cagr, sharpe, sortino, maximumDrawdown, winRate,
 *              volatility, totalTrades },
 *   equityCurve: [...], tradeHistory: [...]
 * }
 */
export async function getBacktest(backtestId) {
  const { data } = await apiClient.get(`/backtest/${backtestId}`)
  return data
}

export default { runBacktest, getBacktestHistory, getBacktest }
