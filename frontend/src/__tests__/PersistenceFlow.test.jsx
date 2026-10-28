import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, Routes, Route } from 'react-router-dom'
import MLBacktestPage from '../pages/MLBacktestPage'
import BacktestResultsPage from '../pages/BacktestResultsPage'

// The ML backtest service is mocked (the real backend call is covered by the
// R API integration tests); the ROUTER IS REAL — unlike MLFlow.test.jsx this
// file intentionally does NOT mock useNavigate, so it can prove that
// handleRun() navigates to `/results/<returned backtestId>` and that the
// Results page then loads the persisted record from GET /backtest/:id using
// ONLY the URL id (a hard refresh has no other state).
vi.mock('../services/mlService', () => ({
  runMlBacktest: vi.fn(),
}))

vi.mock('../services/dataService', () => ({
  validateTicker: vi.fn(async () => ({ success: true })),
  searchTickers: vi.fn(async () => ({ success: true, count: 0, results: [] })),
  fetchMarketData: vi.fn(),
}))

vi.mock('../services/backtestService', () => ({
  getBacktest: vi.fn(),
  getBacktestHistory: vi.fn(),
  runBacktest: vi.fn(),
}))

import { runMlBacktest } from '../services/mlService'
import {
  getBacktest,
  getBacktestHistory,
} from '../services/backtestService'

const ML_BACKTEST_RESPONSE = {
  success: true,
  backtestId: 77,
  model: 'random_forest',
  model_name: 'Random Forest',
  tickers: ['RELIANCE.NS', 'TCS.NS', 'INFY.NS', 'HDFCBANK.NS', 'ICICIBANK.NS'],
  start: '2022-01-01',
  end: '2025-12-31',
  config: { top_n: 5, rebalance_days: 5 },
  portfolio: {
    metrics: {
      totalReturn: 0.12,
      cagr: 0.08,
      sharpe: 1.1,
      sortino: 1.4,
      maximumDrawdown: -0.09,
      winRate: 0.57,
      volatility: 0.11,
      totalTrades: 14,
    },
  },
  perTicker: [],
}

// The persisted row the Results page re-loads from GET /backtest/77.
// Deliberately DIFFERENT from the in-memory ML response (different ticker,
// different metrics) so the tests can prove the Results page rendered the
// DB-fetched record, not the router state passed by the ML page.
const PERSISTED_RESULT_77 = {
  success: true,
  id: 77,
  ticker: 'AAPL',
  strategy: 'ML_Strategy',
  startDate: '2022-01-03',
  endDate: '2025-12-31',
  initialCapital: 10000,
  parameters: {},
  createdAt: '2026-09-20 10:00:00',
  metrics: {
    initialCapital: 10000,
    finalValue: 11234.56,
    totalReturn: 0.123456,
    cagr: 0.077,
    sharpe: 1.05,
    sortino: 1.3,
    maximumDrawdown: -0.0812,
    winRate: 0.561,
    volatility: 0.13,
    totalTrades: 14,
  },
  equityCurve: [
    { date: '2022-01-03', portfolioValue: 10000, cash: 10000, shares: 0, close: 180.5 },
    { date: '2022-01-04', portfolioValue: 10050, cash: 0, shares: 55, close: 182.7 },
    { date: '2022-01-05', portfolioValue: 11234.56, cash: 0, shares: 55, close: 184.06 },
  ],
  tradeHistory: [
    {
      tradeId: 1,
      side: 'BUY',
      signalDate: '2022-01-04',
      executionDate: '2022-01-05',
      executionPrice: 182.7,
      shares: 55,
      cost: 0,
      cashAfter: 50.0,
      positionAfter: 'LONG',
    },
  ],
}

function renderFlow() {
  return render(
    <MemoryRouter initialEntries={['/ml-backtest']}>
      <Routes>
        <Route path="/ml-backtest" element={<MLBacktestPage />} />
        <Route
          path="/results/:backtestId"
          element={<BacktestResultsPage />}
        />
      </Routes>
    </MemoryRouter>,
  )
}

describe('Persistence flow: ML backtest -> /results/:id -> API re-fetch (hard-refresh safe)', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    getBacktestHistory.mockResolvedValue({ success: true, items: [], total: 0 })
  })

  it('navigates to /results/{returned backtestId} and the Results page re-loads THAT id from the API', async () => {
    runMlBacktest.mockResolvedValue(ML_BACKTEST_RESPONSE)
    getBacktest.mockResolvedValue(PERSISTED_RESULT_77)
    const user = userEvent.setup()

    renderFlow()

    // Run the portfolio backtest on the /ml-backtest page.
    await user.click(screen.getByTestId('mlbt-run-button'))

    // The ML page sent the Top-N payload over POST /ml/backtest.
    await waitFor(() => expect(runMlBacktest).toHaveBeenCalledTimes(1))
    expect(runMlBacktest).toHaveBeenCalledWith(
      expect.objectContaining({
        top_n: 5,
        tickers: [
          'RELIANCE.NS',
          'TCS.NS',
          'INFY.NS',
          'HDFCBANK.NS',
          'ICICIBANK.NS',
        ],
      }),
    )

    // handleRun() uses the RETURNED backtestId to navigate, not a hardcoded id.
    // The ML page itself never rendered an in-page result panel for THIS run
    // (it navigates away immediately on success), so the results title must
    // come from the Results page's own GET /backtest/77 fetch.
    await screen.findByTestId('results-title', {}, { timeout: 5000 })

    // The Results page read `77` from the URL and called GET /backtest/77.
    expect(getBacktest).toHaveBeenCalledWith('77')

    // The rendered record is the API-fetched persisted row (AAPL, #77),
    // NOT the in-memory ML response (which had no AAPL anywhere) — proving
    // the page depends on the persisted record, not on router state. The
    // "#77 · 2022-01-03 → 2025-12-31" chip is built from result.id/startDate/
    // endDate, all of which come from GET /backtest/77.
    expect(screen.getByTestId('results-title')).toHaveTextContent(/AAPL/i)
    expect(screen.getByTestId('results-title')).toHaveTextContent(/#77 · 2022-01-03 → 2025-12-31/)

    // A metric from the persisted record (return 0.123456 -> +12.35%).
    expect(screen.getByText('+12.35%')).toBeInTheDocument()
  })

  it('re-loads the persisted backtest purely from the URL id on a fresh mount (hard-refresh simulation)', async () => {
    getBacktest.mockResolvedValue(PERSISTED_RESULT_77)

    // The two "pages" in this test share no state whatsoever: the Results page
    // is mounted directly at /results/77 with nothing in memory (equivalent to
    // pressing F5 in the browser at /results/77). The previous page's state
    // object is deliberately absent.
    render(
      <MemoryRouter initialEntries={['/results/77']}>
        <Routes>
          <Route path="/results/:backtestId" element={<BacktestResultsPage />} />
        </Routes>
      </MemoryRouter>,
    )

    expect(await screen.findByTestId('results-title')).toHaveTextContent(/AAPL/i)
    expect(getBacktest).toHaveBeenCalledWith('77')
    expect(getBacktest).toHaveBeenCalledTimes(1)
  })
})