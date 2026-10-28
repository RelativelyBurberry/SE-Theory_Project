import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, Routes, Route } from 'react-router-dom'
import BacktestResultsPage from '../pages/BacktestResultsPage'

// Mock the backtest service (no network).
vi.mock('../services/backtestService', () => ({
  getBacktest: vi.fn(),
  getBacktestHistory: vi.fn(),
  runBacktest: vi.fn(),
}))
import { getBacktest, getBacktestHistory } from '../services/backtestService'

const FAKE_RESULT = {
  success: true,
  id: 42,
  ticker: 'AAPL',
  strategy: 'MA_Crossover',
  startDate: '2024-01-01',
  endDate: '2024-03-31',
  initialCapital: 10000,
  parameters: { fast_period: 50, slow_period: 200 },
  createdAt: '2026-09-12 12:00:00',
  metrics: {
    initialCapital: 10000,
    finalValue: 10123.45,
    totalReturn: 0.012345,
    cagr: 0.05,
    sharpe: 0.42,
    sortino: 0.63,
    maximumDrawdown: -0.0312,
    winRate: 0.5,
    volatility: 0.201,
    totalTrades: 2,
  },
  equityCurve: [
    { date: '2024-01-02', portfolioValue: 10000, cash: 10000, shares: 0, close: 180.5 },
    { date: '2024-01-03', portfolioValue: 10050, cash: 0, shares: 55, close: 182.7 },
    { date: '2024-01-04', portfolioValue: 10123.45, cash: 0, shares: 55, close: 184.06 },
  ],
  tradeHistory: [
    {
      tradeId: 1,
      side: 'BUY',
      signalDate: '2024-01-02',
      executionDate: '2024-01-03',
      executionPrice: 182.7,
      shares: 55,
      cost: 0,
      cashAfter: 50.0,
      positionAfter: 'LONG',
    },
    {
      tradeId: 2,
      side: 'SELL',
      signalDate: '2024-01-03',
      executionDate: '2024-01-04',
      executionPrice: 184.06,
      shares: 55,
      cost: 0,
      cashAfter: 10123.45,
      positionAfter: 'FLAT',
    },
  ],
}

const HISTORY_ITEMS = [
  {
    id: 1,
    ticker: 'AAPL',
    strategy: 'MA_Crossover',
    startDate: '2024-01-01',
    endDate: '2024-03-31',
    initialCapital: 10000,
    createdAt: '2026-09-12 12:00:00',
    totalReturn: 0.0532,
    sharpeRatio: 0.62,
    totalTrades: 4,
  },
  {
    id: 2,
    ticker: 'MSFT',
    strategy: 'RSI',
    startDate: '2024-04-01',
    endDate: '2024-06-30',
    initialCapital: 5000,
    createdAt: '2026-09-12 13:00:00',
    totalReturn: null,
    sharpeRatio: null,
    totalTrades: 0,
  },
]

function renderPage(backtestId = '42') {
  return render(
    <MemoryRouter initialEntries={[`/results/${backtestId}`]}>
      <Routes>
        <Route
          path="/results/:backtestId"
          element={<BacktestResultsPage />}
        />
      </Routes>
    </MemoryRouter>,
  )
}

function renderWithoutId(routes = [
  <Route key="list" path="/results" element={<BacktestResultsPage />} />,
]) {
  return render(
    <MemoryRouter initialEntries={['/results']}>
      <Routes>{routes}</Routes>
    </MemoryRouter>,
  )
}

describe('BacktestResultsPage (Phase 6)', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  // ---- /results (no :backtestId) is history-driven, not a static empty state

  it('loads the persisted history via GET /backtest/history when no id is present', async () => {
    getBacktestHistory.mockResolvedValue({ success: true, items: [], total: 0 })
    renderWithoutId()
    expect(screen.getByTestId('results-history-loading')).toBeInTheDocument()
    expect(await screen.findByTestId('results-empty')).toHaveTextContent(/no results yet/i)
    expect(getBacktestHistory).toHaveBeenCalledWith(20, 0)
    expect(getBacktest).not.toHaveBeenCalled()
  })

  it('renders persisted backtests and does NOT say "No results yet" when history has records', async () => {
    getBacktestHistory.mockResolvedValue({ success: true, items: HISTORY_ITEMS, total: 2 })
    renderWithoutId()
    expect(await screen.findByTestId('results-history-list')).toBeInTheDocument()
    expect(screen.getAllByTestId('results-history-item')).toHaveLength(2)
    expect(screen.getByText('AAPL')).toBeInTheDocument()
    expect(screen.getByText('MSFT')).toBeInTheDocument()
    expect(screen.queryByText(/no results yet/i)).not.toBeInTheDocument()
  })

  it('opens a selected persisted backtest through its real URL id (GET /backtest/:id)', async () => {
    const user = userEvent.setup()
    getBacktestHistory.mockResolvedValue({ success: true, items: HISTORY_ITEMS, total: 2 })
    getBacktest.mockResolvedValue(FAKE_RESULT)
    renderWithoutId([
      <Route key="list" path="/results" element={<BacktestResultsPage />} />,
      <Route key="detail" path="/results/:backtestId" element={<BacktestResultsPage />} />,
    ])

    const rows = await screen.findAllByTestId('results-history-item')
    expect(rows).toHaveLength(2)
    await user.click(rows[0]) // item id 1

    // Selection navigates to /results/1, which fetches that record by id.
    expect(getBacktest).toHaveBeenCalledWith('1')
    expect(await screen.findByTestId('results-title')).toHaveTextContent(/AAPL/i)
    // The list was populated from the history endpoint (EquityCurveComparison
    // later calls it again with its own params, so only the list call is
    // asserted here).
    expect(getBacktestHistory).toHaveBeenCalledWith(20, 0)
  })

  it('reports a session problem (not an empty database) when the history fetch is 401', async () => {
    getBacktestHistory.mockRejectedValue({
      response: { status: 401, data: { error: 'Invalid or expired token.' } },
    })
    renderWithoutId()
    expect(await screen.findByTestId('results-history-error')).toHaveTextContent(
      /session has expired/i,
    )
    expect(screen.queryByText(/no results yet/i)).not.toBeInTheDocument()
  })

  it('reports a connection problem (not an empty database) when the history fetch cannot reach the server', async () => {
    getBacktestHistory.mockRejectedValue(new TypeError('Network Error'))
    renderWithoutId()
    expect(await screen.findByTestId('results-history-error')).toHaveTextContent(
      /cannot reach the server/i,
    )
    expect(screen.queryByText(/no results yet/i)).not.toBeInTheDocument()
  })

  it('reports the backend error (not "No results yet") when the history fetch fails', async () => {
    getBacktestHistory.mockRejectedValue({
      response: { data: { success: false, error: 'Database connection failed.' } },
    })
    renderWithoutId()
    expect(await screen.findByTestId('results-history-error')).toHaveTextContent(
      /database connection failed/i,
    )
    expect(screen.queryByText(/no results yet/i)).not.toBeInTheDocument()
  })

  // ---- /results/:id single-record loading (existing behavior preserved)

  it('shows a loading state while fetching, then renders the header', async () => {
    getBacktest.mockResolvedValue(FAKE_RESULT)
    renderPage()
    expect(screen.getByTestId('results-loading')).toBeInTheDocument()
    expect(await screen.findByTestId('results-title')).toHaveTextContent(/AAPL/i)
  })

  it('renders the eight metric cards from the fetched result', async () => {
    getBacktest.mockResolvedValue(FAKE_RESULT)
    renderPage()
    // Total Return +1.23%
    expect(await screen.findByText('+1.23%')).toBeInTheDocument()
    // CAGR +5.00%
    expect(screen.getByText('+5.00%')).toBeInTheDocument()
    // Sharpe / Sortino
    expect(screen.getByText('0.42')).toBeInTheDocument()
    expect(screen.getByText('0.63')).toBeInTheDocument()
    // Max drawdown -3.12%
    expect(screen.getByText('-3.12%')).toBeInTheDocument()
    // Win rate +50.00%
    expect(screen.getByText('+50.00%')).toBeInTheDocument()
    // Volatility +20.10%
    expect(screen.getByText('+20.10%')).toBeInTheDocument()
    // Total trades
    expect(screen.getByText('2')).toBeInTheDocument()
  })

  it('renders initial capital and final value cards', async () => {
    getBacktest.mockResolvedValue(FAKE_RESULT)
    renderPage()
    expect(await screen.findByText('$10,000.00')).toBeInTheDocument()
    // $10,123.45 also appears in the trade table (Cash After on the SELL row).
    const finalValues = await screen.findAllByText('$10,123.45')
    expect(finalValues.length).toBeGreaterThanOrEqual(1)
  })

  it('renders the equity curve chart with portfolio values', async () => {
    getBacktest.mockResolvedValue(FAKE_RESULT)
    renderPage()
    expect(await screen.findByTestId('equity-chart')).toBeInTheDocument()
  })

  it('renders the trade table with BUY and SELL rows', async () => {
    getBacktest.mockResolvedValue(FAKE_RESULT)
    renderPage()
    expect(await screen.findByText('BUY')).toBeInTheDocument()
    expect(screen.getByText('SELL')).toBeInTheDocument()
    // 2024-01-03 appears as both a signal date and an execution date.
    expect(screen.getAllByText('2024-01-03').length).toBeGreaterThanOrEqual(1)
    expect(screen.getByText('$182.70')).toBeInTheDocument()
  })

  it('shows "no trades executed" when tradeHistory is empty', async () => {
    getBacktest.mockResolvedValue({ ...FAKE_RESULT, tradeHistory: [] })
    renderPage()
    expect(await screen.findByText(/no trades were executed/i)).toBeInTheDocument()
  })

  it('shows the session-expired message on a 401 (stale JWT, backend still fine)', async () => {
    getBacktest.mockRejectedValue({
      response: { status: 401, data: { error: 'Invalid or expired token.' } },
    })
    renderPage()
    expect(await screen.findByTestId('results-error')).toHaveTextContent(
      /session has expired/i,
    )
  })

  it('shows an offline-style message when the request fails without a response', async () => {
    // No `response` on the error = network/CORS failure — the backend was
    // never reached, so the UI must not blame the backtest or the session.
    getBacktest.mockRejectedValue(new TypeError('Network Error'))
    renderPage()
    expect(await screen.findByTestId('results-error')).toHaveTextContent(
      /cannot reach the server/i,
    )
  })

  it('renders the backend error when the fetch fails', async () => {
    getBacktest.mockRejectedValue({
      response: { data: { success: false, error: 'Backtest not found.' } },
    })
    renderPage()
    expect(await screen.findByTestId('results-error')).toHaveTextContent(
      /backtest not found/i,
    )
  })

  it('shows a clean 404 message when the persisted backtest does not exist', async () => {
    getBacktest.mockRejectedValue({
      response: { status: 404, data: { success: false, error: 'Backtest not found.' } },
    })
    renderPage()
    expect(await screen.findByTestId('results-error')).toHaveTextContent(
      /backtest not found/i,
    )
  })

  it('shows a server-level message (not the generic one) on a 500', async () => {
    getBacktest.mockRejectedValue({
      response: { status: 500, data: { success: false, error: 'boom' } },
    })
    renderPage()
    expect(await screen.findByTestId('results-error')).toHaveTextContent(
      /server hit an error/i,
    )
  })

  it('handles an invalid (non-numeric) backtest id gracefully', async () => {
    getBacktest.mockResolvedValue(FAKE_RESULT)
    renderPage('abc')
    // The page still renders after the fetch resolves with the mocked result.
    expect(await screen.findByTestId('results-title')).toBeInTheDocument()
  })

  it('formats undefined metrics as an em dash, never NaN', async () => {
    getBacktest.mockResolvedValue({
      ...FAKE_RESULT,
      metrics: { ...FAKE_RESULT.metrics, winRate: null, sharpe: {}, sortino: undefined },
    })
    renderPage()
    // Several cards (null winRate, {} sharpe, undefined sortino) show "—".
    expect((await screen.findAllByText('—')).length).toBeGreaterThanOrEqual(1)
    expect(screen.queryByText('NaN')).not.toBeInTheDocument()
    expect(screen.queryByText('0.00%')).not.toBeInTheDocument()
  })
})