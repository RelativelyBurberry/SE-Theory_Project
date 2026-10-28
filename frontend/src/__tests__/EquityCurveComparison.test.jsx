import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen } from '@testing-library/react'
import EquityCurveComparison from '../components/EquityCurveComparison'

// Mock the backtest service (no network). The comparison must ONLY plot
// values that come from these mocked API responses — the tests assert real
// rendering of persisted (mocked) data, never fabricated numbers.
vi.mock('../services/backtestService', () => ({
  getBacktest: vi.fn(),
  getBacktestHistory: vi.fn(),
  runBacktest: vi.fn(),
}))
import { getBacktest, getBacktestHistory } from '../services/backtestService'

const MA_CURVE = [
  { date: '2015-01-02', portfolioValue: 10000 },
  { date: '2015-01-05', portfolioValue: 11000 },
  { date: '2025-12-30', portfolioValue: 43718.97 },
  { date: '2025-12-31', portfolioValue: 43718.97 },
]

const RSI_CURVE = [
  { date: '2015-01-02', portfolioValue: 10000 },
  { date: '2015-01-05', portfolioValue: 10500 },
  { date: '2025-12-31', portfolioValue: 15000 },
]

const BH_CURVE = [
  { date: '2015-01-02', portfolioValue: 10000 },
  { date: '2015-01-05', portfolioValue: 10750 },
  { date: '2025-12-31', portfolioValue: 100776.76 },
]

const MA_BT = {
  success: true,
  id: 11,
  ticker: 'AAPL',
  strategy: 'MA_Crossover',
  startDate: '2015-01-01',
  endDate: '2025-12-31',
  parameters: { fast_period: 50, slow_period: 200 },
  metrics: { initialCapital: 10000, finalValue: 43718.97, totalReturn: 3.3719 },
  equityCurve: MA_CURVE,
  tradeHistory: [],
}

const RSI_BT = {
  success: true,
  id: 12,
  ticker: 'AAPL',
  strategy: 'RSI',
  startDate: '2015-01-01',
  endDate: '2025-12-31',
  parameters: { period: 14, oversold: 30, overbought: 70 },
  metrics: { initialCapital: 10000, finalValue: 15000, totalReturn: 0.5 },
  equityCurve: RSI_CURVE,
  tradeHistory: [],
}

const BH_BT = {
  success: true,
  id: 13,
  ticker: 'AAPL',
  strategy: 'Buy_and_Hold',
  startDate: '2015-01-01',
  endDate: '2025-12-31',
  parameters: {},
  metrics: { initialCapital: 10000, finalValue: 100776.76, totalReturn: 9.0777 },
  equityCurve: BH_CURVE,
  tradeHistory: [],
}

function historyItems(runs) {
  return runs.map((run) => ({
    id: run.id,
    ticker: run.ticker,
    strategy: run.strategy,
  }))
}

describe('EquityCurveComparison (Figure 4)', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('shows a loading state while history is being fetched', () => {
    getBacktestHistory.mockImplementation(() => new Promise(() => {}))
    render(<EquityCurveComparison backtest={MA_BT} />)
    expect(screen.getByTestId('equity-comparison-loading')).toBeInTheDocument()
  })

  it('renders one normalized line per persisted strategy with a caption', async () => {
    getBacktestHistory.mockResolvedValue({
      success: true,
      items: historyItems([MA_BT, RSI_BT, BH_BT]),
    })
    getBacktest.mockImplementation((id) =>
      Promise.resolve([MA_BT, RSI_BT, BH_BT].find((b) => b.id === id)),
    )
    render(<EquityCurveComparison backtest={MA_BT} />)

    // Caption with ticker and indexed note.
    expect(await screen.findByText(/Equity Curve Comparison — AAPL/i)).toBeInTheDocument()
    expect(screen.getByText(/Indexed to \$10,000 initial capital/i)).toBeInTheDocument()

    // All three strategy labels appear (legend + per-strategy summary chips).
    expect((await screen.findAllByText('MA Cross (50/200)')).length).toBeGreaterThanOrEqual(1)
    expect(screen.getAllByText('RSI (14, 30/70)').length).toBeGreaterThanOrEqual(1)
    expect(screen.getAllByText('Buy & Hold').length).toBeGreaterThanOrEqual(1)

    // Normalized final values (indexed to $10,000) come from the persisted curves.
    expect(screen.getByText('$43,719')).toBeInTheDocument()
    expect(screen.getByText('$15,000')).toBeInTheDocument()
    expect(screen.getByText('$100,777')).toBeInTheDocument()

    // Nothing is missing when all three strategies are persisted.
    expect(screen.queryByTestId('comparison-missing')).not.toBeInTheDocument()
  })

  it('reports strategies that are not present in the saved backtests instead of inventing them', async () => {
    // Only the current MA run exists for AAPL.
    getBacktestHistory.mockResolvedValue({
      success: true,
      items: historyItems([MA_BT]),
    })
    getBacktest.mockImplementation((id) =>
      Promise.resolve([MA_BT].find((b) => b.id === id)),
    )
    render(<EquityCurveComparison backtest={MA_BT} />)

    const missing = await screen.findByTestId('comparison-missing')
    expect(missing).toHaveTextContent('Showing 1 of 3 strategies for AAPL')
    expect(missing).toHaveTextContent('RSI (14, 30/70)')
    expect(missing).toHaveTextContent('Buy & Hold')
    expect(missing).toHaveTextContent('no data is fabricated')
  })

  it('shows the asset selector when more than one ticker has persisted runs', async () => {
    const MSFT_ITEM = { id: 99, ticker: 'MSFT', strategy: 'MA_Crossover' }
    getBacktestHistory.mockResolvedValue({
      success: true,
      items: [...historyItems([MA_BT]), MSFT_ITEM],
    })
    getBacktest.mockResolvedValue(MA_BT)
    render(<EquityCurveComparison backtest={MA_BT} />)

    const select = await screen.findByLabelText(/asset/i)
    expect(select).toHaveValue('AAPL')
    expect(Array.from(select.options).map((o) => o.value)).toEqual(
      expect.arrayContaining(['AAPL', 'MSFT']),
    )
  })

  it('renders the backend error when the history fetch fails', async () => {
    getBacktestHistory.mockRejectedValue({
      response: { data: { success: false, error: 'Database connection failed.' } },
    })
    render(<EquityCurveComparison backtest={MA_BT} />)
    expect(await screen.findByTestId('equity-comparison-error')).toHaveTextContent(
      /database connection failed/i,
    )
  })
})