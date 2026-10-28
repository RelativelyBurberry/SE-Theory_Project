import { describe, it, expect, vi, beforeEach } from 'vitest'
import { runBacktest, getBacktestHistory, getBacktest } from '../services/backtestService'

// Mock the centralized Axios client so no network is touched; verify the
// request construction (URL, method, payload, query params) instead.
vi.mock('../services/apiClient', () => ({
  default: {
    post: vi.fn(),
    get: vi.fn(),
  },
}))
import apiClient from '../services/apiClient'

const PAYLOAD = {
  ticker: 'AAPL',
  start: '2024-01-01',
  end: '2024-03-31',
  initialCapital: 10000,
  strategy: 'MA_Crossover',
  parameters: { fast_period: 50, slow_period: 200 },
}

const RUN_RESPONSE = {
  data: {
    success: true,
    backtestId: 7,
    ticker: 'AAPL',
    metrics: { totalReturn: 0.02 },
    equityCurve: [],
    tradeHistory: [],
  },
}

describe('backtestService (Phase 6)', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('runBacktest POSTs the payload to /backtest/run and returns data', async () => {
    apiClient.post.mockResolvedValue(RUN_RESPONSE)
    const result = await runBacktest(PAYLOAD)
    expect(apiClient.post).toHaveBeenCalledWith('/backtest/run', PAYLOAD)
    expect(result.success).toBe(true)
    expect(result.backtestId).toBe(7)
  })

  it('getBacktestHistory GETs /backtest/history with limit/offset params', async () => {
    apiClient.get.mockResolvedValue({
      data: { success: true, items: [], total: 0, limit: 20, offset: 0 },
    })
    const result = await getBacktestHistory(20, 0)
    expect(apiClient.get).toHaveBeenCalledWith('/backtest/history', {
      params: { limit: 20, offset: 0 },
    })
    expect(result.total).toBe(0)
  })

  it('getBacktestHistory uses defaults when called without arguments', async () => {
    apiClient.get.mockResolvedValue({
      data: { success: true, items: [], total: 0, limit: 20, offset: 0 },
    })
    await getBacktestHistory()
    expect(apiClient.get).toHaveBeenCalledWith('/backtest/history', {
      params: { limit: 20, offset: 0 },
    })
  })

  it('getBacktest GETs /backtest/:id', async () => {
    apiClient.get.mockResolvedValue({
      data: { success: true, id: 7, metrics: {}, equityCurve: [], tradeHistory: [] },
    })
    const result = await getBacktest(7)
    expect(apiClient.get).toHaveBeenCalledWith('/backtest/7')
    expect(result.id).toBe(7)
  })

  it('propagates API errors to the caller', async () => {
    apiClient.post.mockRejectedValue({
      response: { data: { success: false, error: 'boom' } },
    })
    await expect(runBacktest(PAYLOAD)).rejects.toMatchObject({
      response: { data: { error: 'boom' } },
    })
  })
})