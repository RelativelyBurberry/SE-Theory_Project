import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import StrategyBuilderPage from '../pages/StrategyBuilderPage'

// Capture the navigation target so tests can assert the redirect after a
// successful backtest run.
const { navigateMock } = vi.hoisted(() => ({ navigateMock: vi.fn() }))
vi.mock('react-router-dom', async (importOriginal) => {
  const actual = await importOriginal()
  return {
    ...actual,
    useNavigate: () => navigateMock,
  }
})

// Mock the API services so tests never hit the network. Validation logic is
// NOT mocked — the page's own validateStrategyForm runs in every test, so
// frontend validation bugs would surface here.
vi.mock('../services/dataService', () => ({
  validateTicker: vi.fn(),
  fetchMarketData: vi.fn(),
}))
vi.mock('../services/strategyService', () => ({
  getStrategyDefaults: vi.fn(),
  previewStrategy: vi.fn(),
}))
vi.mock('../services/backtestService', () => ({
  runBacktest: vi.fn(),
  getBacktest: vi.fn(),
  getBacktestHistory: vi.fn(),
}))
import * as dataService from '../services/dataService'
import * as strategyService from '../services/strategyService'
import * as backtestService from '../services/backtestService'

const FAKE_PREVIEW = {
  success: true,
  ticker: 'AAPL',
  start: '2024-01-01',
  end: '2024-03-31',
  strategy: 'MA_Crossover',
  parameters: { fast_period: 50, slow_period: 200 },
  row_count: 3,
  signal_count: 2,
  signals: [
    { date: '2024-01-02', close: 185.64, fast_sma: 181.2, slow_sma: 183.4 },
    { date: '2024-01-03', close: 184.25, fast_sma: 182.1, slow_sma: 183.5, signal: 'BUY' },
    { date: '2024-01-04', close: 181.91, fast_sma: 182.9, slow_sma: 183.6, signal: 'SELL' },
  ],
}

const FAKE_RUN = {
  success: true,
  backtestId: 42,
  ticker: 'AAPL',
  strategy: 'MA_Crossover',
  start: '2024-01-01',
  end: '2024-03-31',
  parameters: { fast_period: 50, slow_period: 200 },
  metrics: {
    initialCapital: 10000,
    finalValue: 10123.45,
    totalReturn: 0.0123,
    cagr: 0.05,
    sharpe: 0.4,
    sortino: 0.6,
    maximumDrawdown: -0.03,
    winRate: 0.5,
    volatility: 0.2,
    totalTrades: 2,
  },
  equityCurve: [
    { date: '2024-01-01', portfolioValue: 10000 },
    { date: '2024-01-02', portfolioValue: 10100 },
  ],
  tradeHistory: [
    {
      tradeId: 1,
      side: 'BUY',
      signalDate: '2024-01-02',
      executionDate: '2024-01-03',
      executionPrice: 150.25,
      shares: 66,
      cost: 0,
      cashAfter: 84.5,
    },
  ],
}

function renderPage() {
  return render(
    <MemoryRouter initialEntries={['/strategy-builder']}>
      <StrategyBuilderPage />
    </MemoryRouter>,
  )
}

describe('StrategyBuilderPage (Phase 4)', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('shows "Preview Strategy" and "Run Backtest" buttons, both enabled on load', () => {
    renderPage()
    const previewBtn = screen.getByRole('button', { name: /preview strategy/i })
    const runBtn = screen.getByRole('button', { name: /run backtest/i })
    expect(previewBtn).toBeInTheDocument()
    expect(runBtn).toBeInTheDocument()
    expect(previewBtn).toBeEnabled()
    expect(runBtn).toBeEnabled()
  })

  it('pre-fills the documented MA defaults: fast 50, slow 200', () => {
    renderPage()
    expect(screen.getByLabelText(/fast ma period/i)).toHaveValue(50)
    expect(screen.getByLabelText(/slow ma period/i)).toHaveValue(200)
  })

  it('shows MA parameter fields when MA_Crossover is selected', () => {
    renderPage()
    expect(screen.getByLabelText(/fast ma period/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/slow ma period/i)).toBeInTheDocument()
  })

  it('switching to RSI swaps in RSI fields pre-filled with 14 / 30 / 70', async () => {
    const user = userEvent.setup()
    renderPage()
    await user.selectOptions(screen.getByLabelText(/^strategy$/i), 'RSI')

    expect(screen.getByLabelText(/rsi period/i)).toHaveValue(14)
    expect(screen.getByLabelText(/oversold/i)).toHaveValue(30)
    expect(screen.getByLabelText(/overbought/i)).toHaveValue(70)

    // MA fields are gone while RSI is selected.
    expect(screen.queryByLabelText(/fast ma period/i)).not.toBeInTheDocument()
    expect(screen.queryByLabelText(/slow ma period/i)).not.toBeInTheDocument()
  })

  it('switching back to MA restores the MA fields', async () => {
    const user = userEvent.setup()
    renderPage()
    await user.selectOptions(screen.getByLabelText(/^strategy$/i), 'RSI')
    await user.selectOptions(screen.getByLabelText(/^strategy$/i), 'MA_Crossover')
    expect(screen.getByLabelText(/fast ma period/i)).toHaveValue(50)
    expect(screen.getByLabelText(/slow ma period/i)).toHaveValue(200)
  })

  // ---- validation ---------------------------------------------------------

  it('rejects an empty ticker on blur', async () => {
    const user = userEvent.setup()
    renderPage()
    const input = screen.getByLabelText(/stock ticker/i)
    await user.clear(input)
    await user.tab()
    expect(await screen.findByTestId('error-ticker')).toHaveTextContent(/ticker is required/i)
  })

  it('rejects a malformed ticker format', async () => {
    const user = userEvent.setup()
    renderPage()
    const input = screen.getByLabelText(/stock ticker/i)
    await user.clear(input)
    await user.type(input, 'bad!ticker')
    await user.tab()
    expect(await screen.findByTestId('error-ticker')).toHaveTextContent(/1–20 characters/i)
  })

  it('accepts NSE tickers with an exchange suffix (RELIANCE.NS)', async () => {
    const user = userEvent.setup()
    renderPage()
    const input = screen.getByLabelText(/stock ticker/i)
    await user.clear(input)
    await user.type(input, 'RELIANCE.NS')
    await user.tab()
    expect(screen.queryByTestId('error-ticker')).not.toBeInTheDocument()
  })

  it('rejects start date after end date', async () => {
    const user = userEvent.setup()
    renderPage()
    const start = screen.getByLabelText(/start date/i)
    await user.clear(start)
    await user.type(start, '2024-06-01')
    await user.tab()
    expect(await screen.findByTestId('error-start')).toHaveTextContent(/on or before/i)
  })

  it('rejects a non-positive initial capital', async () => {
    const user = userEvent.setup()
    renderPage()
    const capital = screen.getByLabelText(/initial capital/i)
    await user.clear(capital)
    await user.type(capital, '-500')
    await user.tab()
    expect(await screen.findByTestId('error-capital')).toHaveTextContent(/greater than zero/i)
  })

  it('rejects fast period >= slow period', async () => {
    const user = userEvent.setup()
    renderPage()
    const fast = screen.getByLabelText(/fast ma period/i)
    await user.clear(fast)
    await user.type(fast, '200')
    await user.tab()
    expect(await screen.findByTestId('error-fastPeriod')).toHaveTextContent(
      /strictly less than/i,
    )
  })

  it('rejects a non-integer RSI period', async () => {
    const user = userEvent.setup()
    renderPage()
    await user.selectOptions(screen.getByLabelText(/^strategy$/i), 'RSI')
    const period = screen.getByLabelText(/rsi period/i)
    await user.clear(period)
    await user.type(period, '14.5')
    await user.tab()
    expect(await screen.findByTestId('error-rsiPeriod')).toHaveTextContent(/positive integer/i)
  })

  it('rejects oversold >= overbought', async () => {
    const user = userEvent.setup()
    renderPage()
    await user.selectOptions(screen.getByLabelText(/^strategy$/i), 'RSI')
    const os = screen.getByLabelText(/oversold/i)
    await user.clear(os)
    await user.type(os, '80')
    await user.tab()
    expect(await screen.findByTestId('error-oversold')).toHaveTextContent(
      /strictly less than/i,
    )
  })

  it('rejects an overbought threshold >= 100', async () => {
    const user = userEvent.setup()
    renderPage()
    await user.selectOptions(screen.getByLabelText(/^strategy$/i), 'RSI')
    const ob = screen.getByLabelText(/overbought/i)
    await user.clear(ob)
    await user.type(ob, '100')
    await user.tab()
    expect(await screen.findByTestId('error-overbought')).toHaveTextContent(/between 0 and 100/i)
  })

  // ---- ticker validation (real backend contract) ---------------------------

  it('calls the backend ticker validation and shows the valid state', async () => {
    dataService.validateTicker.mockResolvedValue({ ticker: 'AAPL', valid: true })
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /check/i }))
    expect(dataService.validateTicker).toHaveBeenCalledWith('AAPL')
    expect(await screen.findByTestId('ticker-valid')).toHaveTextContent(/resolves/i)
  })

  it('shows the loading state while the ticker check is in flight', async () => {
    let resolveCheck
    dataService.validateTicker.mockImplementation(
      () => new Promise((resolve) => { resolveCheck = resolve }),
    )
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /check/i }))
    expect(screen.getByRole('button', { name: /checking/i })).toBeDisabled()
    resolveCheck({ ticker: 'AAPL', valid: true })
    expect(await screen.findByTestId('ticker-valid')).toHaveTextContent(/resolves/i)
  })

  it('shows the invalid state when the backend reports an unknown ticker', async () => {
    dataService.validateTicker.mockResolvedValue({
      ticker: 'NOPE',
      valid: false,
      error: 'No market data found for ticker NOPE.',
    })
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /check/i }))
    expect(await screen.findByTestId('ticker-invalid')).toHaveTextContent(/no market data/i)
  })

  it('handles network/backend failures during ticker validation', async () => {
    dataService.validateTicker.mockRejectedValue({
      response: { data: { success: false, error: 'data source unavailable' } },
    })
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /check/i }))
    expect(await screen.findByTestId('ticker-invalid')).toHaveTextContent(
      /data source unavailable/i,
    )
  })

  // ---- preview -------------------------------------------------------------

  it('sends the correct API payload to previewStrategy', async () => {
    strategyService.previewStrategy.mockResolvedValue(FAKE_PREVIEW)
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /preview strategy/i }))

    expect(strategyService.previewStrategy).toHaveBeenCalledWith({
      ticker: 'AAPL',
      start: '2024-01-01',
      end: '2024-03-31',
      initialCapital: 10000,
      strategy: 'MA_Crossover',
      parameters: { fast_period: 50, slow_period: 200 },
    })
    expect(await screen.findByText(/2 signals/i)).toBeInTheDocument()
  })

  it('sends the RSI payload when the RSI strategy is selected', async () => {
    strategyService.previewStrategy.mockResolvedValue({ ...FAKE_PREVIEW, strategy: 'RSI' })
    const user = userEvent.setup()
    renderPage()
    await user.selectOptions(screen.getByLabelText(/^strategy$/i), 'RSI')
    await user.click(screen.getByRole('button', { name: /preview strategy/i }))

    expect(strategyService.previewStrategy).toHaveBeenCalledWith(
      expect.objectContaining({
        strategy: 'RSI',
        parameters: { period: 14, oversold: 30, overbought: 70 },
      }),
    )
  })

  it('shows the loading state on the submit button while previewing', async () => {
    let resolvePreview
    strategyService.previewStrategy.mockImplementation(
      () => new Promise((resolve) => { resolvePreview = resolve }),
    )
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /preview strategy/i }))
    expect(screen.getByRole('button', { name: /generating signals/i })).toBeDisabled()
    resolvePreview(FAKE_PREVIEW)
    expect(await screen.findByText(/2 signals/i)).toBeInTheDocument()
  })

  it('renders the returned signal rows with BUY and SELL badges', async () => {
    strategyService.previewStrategy.mockResolvedValue(FAKE_PREVIEW)
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /preview strategy/i }))

    // Values come from the (mocked) backend response — nothing fabricated.
    expect(await screen.findByText('2024-01-03')).toBeInTheDocument()
    expect(screen.getByText('BUY')).toBeInTheDocument()
    expect(screen.getByText('SELL')).toBeInTheDocument()
    expect(screen.getByText('181.20')).toBeInTheDocument()
  })

  it('renders the backend error message when the preview request fails', async () => {
    strategyService.previewStrategy.mockRejectedValue({
      response: {
        data: {
          success: false,
          error: 'fast_period must be strictly less than slow_period.',
        },
      },
    })
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /preview strategy/i }))
    expect(await screen.findByTestId('preview-error')).toHaveTextContent(
      /strictly less than/i,
    )
  })

  it('does not call the API when the form is invalid', async () => {
    const user = userEvent.setup()
    renderPage()
    const fast = screen.getByLabelText(/fast ma period/i)
    await user.clear(fast)
    await user.type(fast, '250')
    await user.tab()
    // After blur validation the submit should be disabled; simulating a
    // click on a disabled button must NOT fire the service call.
    const btn = screen.getByRole('button', { name: /preview strategy/i })
    expect(btn).toBeDisabled()
    await user.click(btn)
    expect(strategyService.previewStrategy).not.toHaveBeenCalled()
  })

  // ---- backtest execution (Phase 6) ----------------------------------------

  it('sends the correct payload to runBacktest', async () => {
    backtestService.runBacktest.mockResolvedValue(FAKE_RUN)
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /run backtest/i }))

    expect(backtestService.runBacktest).toHaveBeenCalledWith({
      ticker: 'AAPL',
      start: '2024-01-01',
      end: '2024-03-31',
      initialCapital: 10000,
      strategy: 'MA_Crossover',
      parameters: { fast_period: 50, slow_period: 200 },
    })
  })

  it('shows the loading state while the backtest is running', async () => {
    let resolveRun
    backtestService.runBacktest.mockImplementation(
      () => new Promise((resolve) => { resolveRun = resolve }),
    )
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /run backtest/i }))
    expect(screen.getByRole('button', { name: /running backtest/i })).toBeDisabled()
    resolveRun(FAKE_RUN)
    // Navigation happens in a microtask after the promise resolves.
    await waitFor(() => expect(navigateMock).toHaveBeenCalledWith('/results/42'))
  })

  it('navigates to the results page after a successful run', async () => {
    backtestService.runBacktest.mockResolvedValue(FAKE_RUN)
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /run backtest/i }))
    expect(navigateMock).toHaveBeenCalledWith('/results/42')
  })

  it('renders the backend error when the backtest run fails', async () => {
    backtestService.runBacktest.mockRejectedValue({
      response: {
        data: { success: false, error: 'No market data found for ticker AAPL.' },
      },
    })
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /run backtest/i }))
    expect(await screen.findByTestId('run-error')).toHaveTextContent(/no market data/i)
    expect(navigateMock).not.toHaveBeenCalled()
  })

  it('does not call runBacktest when the form is invalid', async () => {
    const user = userEvent.setup()
    renderPage()
    const capital = screen.getByLabelText(/initial capital/i)
    await user.clear(capital)
    await user.type(capital, '0')
    await user.tab()
    const runBtn = screen.getByRole('button', { name: /run backtest/i })
    expect(runBtn).toBeDisabled()
    await user.click(runBtn)
    expect(backtestService.runBacktest).not.toHaveBeenCalled()
  })
})