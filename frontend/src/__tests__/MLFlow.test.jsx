import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor, fireEvent } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, Routes, Route } from 'react-router-dom'
import StrategyBuilderPage from '../pages/StrategyBuilderPage'
import MLComparisonPage from '../pages/MLComparisonPage'
import MLBacktestPage from '../pages/MLBacktestPage'
import {
  ML_FORM_DEFAULTS, validateMlForm, buildMlTrainPayload, buildMlBacktestPayload,
} from '../utils/mlDefaults'

vi.mock('react-router-dom', async (importOriginal) => {
  const actual = await importOriginal()
  return { ...actual, useNavigate: () => vi.fn() }
})

vi.mock('../services/dataService', () => ({
  validateTicker: vi.fn(),
  searchTickers: vi.fn(),
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
vi.mock('../services/mlService', () => ({
  trainMlModel: vi.fn(),
  compareMlModels: vi.fn(),
  predictMlModel: vi.fn(),
  runMlBacktest: vi.fn(),
  ML_MODEL_OPTIONS: [
    { value: 'random_forest', label: 'Random Forest' },
    { value: 'lstm', label: 'LSTM' },
    { value: 'logistic_regression', label: 'Logistic Regression' },
  ],
  ML_FEATURE_GROUP_OPTIONS: [
    { value: 'returns', label: 'Returns' },
    { value: 'trend', label: 'Trend (SMA / EMA)' },
    { value: 'rsi', label: 'RSI' },
  ],
  REBALANCE_OPTIONS: [
    { value: 'daily', label: 'Daily' },
    { value: 'weekly', label: 'Weekly' },
    { value: 'monthly', label: 'Monthly' },
  ],
}))
import * as mlService from '../services/mlService'
import * as dataService from '../services/dataService'

// ---------------------------------------------------------------- helpers

// The form validator rejects future end dates, so tests use dates slightly in
// the future relative to "today" only via an injectable today in unit tests;
// page tests instead pick a wide historical window that always validates.
function wideWindow() {
  return { start: '2020-01-01', end: '2023-12-31' }
}

function validForm(overrides = {}) {
  return {
    ...ML_FORM_DEFAULTS,
    ...wideWindow(),
    ...overrides,
  }
}

function renderPage(ui, initialPath) {
  return render(
    <MemoryRouter initialEntries={[initialPath]}>
      <Routes>
        <Route path="/strategy-builder" element={<StrategyBuilderPage />} />
        <Route path="/ml-compare" element={ui} />
        <Route path="/ml-backtest" element={ui} />
        <Route path="/results/:backtestId" element={<div data-testid="results-page" />} />
      </Routes>
    </MemoryRouter>,
  )
}

beforeEach(() => {
  vi.clearAllMocks()
  // ML Comparison persists snapshots in sessionStorage; each test must start
  // with a clean slate or a restored comparison would leak across tests.
  sessionStorage.clear()
})

// ------------------------------------------------- defaults + validation

describe('mlDefaults', () => {
  it('has sensible defaults for the ML form', () => {
    expect(ML_FORM_DEFAULTS.model).toBe('random_forest')
    expect(ML_FORM_DEFAULTS.horizon).toBe('5')
    expect(ML_FORM_DEFAULTS.topN).toBe('5')
    expect(ML_FORM_DEFAULTS.rebalance).toBe('weekly')
    expect(ML_FORM_DEFAULTS.featureGroups.length).toBeGreaterThan(0)
  })

  it('accepts a valid ML form', () => {
    const r = validateMlForm(validForm(), '2024-06-01')
    expect(r.errors).toEqual({})
    expect(r.valid).toBe(true)
  })

  it('rejects a future end date', () => {
    const r = validateMlForm(validForm({ end: '2030-01-01' }), '2024-06-01')
    expect(r.valid).toBe(false)
    expect(r.errors.end).toMatch(/future/i)
  })

  it('rejects ranges shorter than one year (no room for splits)', () => {
    const r = validateMlForm(validForm({ start: '2023-06-01', end: '2023-12-31' }), '2024-06-01')
    expect(r.valid).toBe(false)
    expect(r.errors.start).toMatch(/year/i)
  })

  it('rejects an invalid horizon and topN', () => {
    expect(validateMlForm(validForm({ horizon: '0' })).errors.horizon).toBeTruthy()
    expect(validateMlForm(validForm({ horizon: '61' })).errors.horizon).toBeTruthy()
    expect(validateMlForm(validForm({ topN: '11' })).errors.topN).toBeTruthy()
    expect(validateMlForm(validForm({ topN: '0' })).errors.topN).toBeTruthy()
  })

  it('rejects thresholds outside (0, 100)', () => {
    expect(validateMlForm(validForm({ buyThreshold: '0' })).errors.buyThreshold).toBeTruthy()
    expect(validateMlForm(validForm({ sellThreshold: '150' })).errors.sellThreshold).toBeTruthy()
  })

  it('requires at least one feature group', () => {
    const r = validateMlForm(validForm({ featureGroups: [] }))
    expect(r.valid).toBe(false)
    expect(r.errors.featureGroups).toBeTruthy()
  })
})

// ------------------------------------------------------ payload builders

describe('payload builders', () => {
  it('sends thresholds as decimals and numeric fields as numbers', () => {
    const p = buildMlTrainPayload(validForm({ buyThreshold: '2', sellThreshold: '3', horizon: '5' }))
    expect(p.buy_threshold).toBeCloseTo(0.02)
    expect(p.sell_threshold).toBeCloseTo(0.03)
    expect(p.horizon).toBe(5)
    expect(p.ticker).toBe('AAPL')
    expect(p.initialCapital).toBe(10000)
  })

  it('adds the ticker universe and top_n to the backtest payload', () => {
    const p = buildMlBacktestPayload(validForm({ topN: '3' }), ['aapl.ns', ' tcs.ns '])
    expect(p.tickers).toEqual(['AAPL.NS', 'TCS.NS'])
    expect(p.top_n).toBe(3)
    expect(p.rebalance).toBe('weekly')
  })
})

// ------------------------------------------- strategy builder ML section

describe('StrategyBuilderPage ML strategy', () => {
  it('shows ML model options when ML Strategy is selected', async () => {
    const user = userEvent.setup()
    renderPage(<StrategyBuilderPage />, '/strategy-builder')
    const mlRadio = screen.getByRole('radio', { name: /ML Strategy/i })
    await user.click(mlRadio)
    expect(await screen.findByText(/Random Forest/)).toBeInTheDocument()
    expect(screen.getByText(/LSTM/)).toBeInTheDocument()
  })

  it('submits /ml/train with the configured payload and renders metrics', async () => {
    const user = userEvent.setup()
    mlService.trainMlModel.mockResolvedValue({
      success: true,
      model: 'random_forest',
      model_name: 'Random Forest',
      ticker: 'AAPL',
      validation_metrics: {
        accuracy: 0.61, precision: 0.55, recall: 0.48, f1: 0.51,
        roc_auc_macro: 0.58, pr_auc_macro: 0.42,
        confusion_matrix: [
          { predicted: 'BUY', actual_counts: { BUY: 5, HOLD: 3, SELL: 1 } },
          { predicted: 'HOLD', actual_counts: { BUY: 2, HOLD: 9, SELL: 2 } },
          { predicted: 'SELL', actual_counts: { BUY: 0, HOLD: 4, SELL: 6 } },
        ],
        curves: [],
      },
      backtest: {
        engine: 'existing t+1-open engine (run_backtest)',
        metrics: { totalReturn: 0.08, cagr: 0.05, sharpe: 0.7, sortino: 0.9, maximumDrawdown: -0.12, winRate: 0.52, volatility: 0.14, totalTrades: 9 },
      },
      feature_importance: [],
      predictions: [],
    })

    renderPage(<StrategyBuilderPage />, '/strategy-builder')
    await user.click(screen.getByRole('radio', { name: /ML Strategy/i }))

    const tickerInput = await screen.findByLabelText(/^Stock ticker$/i)
    await user.clear(tickerInput)
    await user.type(tickerInput, 'aapl')

    // Native date inputs: set values via fireEvent (typing produces a
    // partial string that the input may reject mid-way).
    fireEvent.change(screen.getByLabelText(/start date/i), { target: { value: '2020-01-01' } })
    fireEvent.change(screen.getByLabelText(/end date/i), { target: { value: '2023-12-31' } })

    const runButton = screen.getByRole('button', { name: /train model/i })
    await user.click(runButton)

    await waitFor(() => {
      expect(mlService.trainMlModel).toHaveBeenCalledTimes(1)
    })
    const payload = mlService.trainMlModel.mock.calls[0][0]
    expect(payload.model).toBe('random_forest')
    expect(payload.ticker).toBe('AAPL')
    expect(payload.buy_threshold).toBeCloseTo(0.02)
    expect(payload.horizon).toBe(5)

    // Classification + trading metrics both rendered.
    expect(await screen.findByText('+61.00%')).toBeInTheDocument()
    expect(screen.getByTestId('ml-train-result')).toBeInTheDocument()
  })

  it('surfaces a training failure without breaking the form', async () => {
    const user = userEvent.setup()
    mlService.trainMlModel.mockRejectedValue({
      response: { data: { error: 'Not enough complete rows for the requested window.' } },
    })
    renderPage(<StrategyBuilderPage />, '/strategy-builder')
    await user.click(screen.getByRole('radio', { name: /ML Strategy/i }))
    fireEvent.change(screen.getByLabelText(/start date/i), { target: { value: '2020-01-01' } })
    fireEvent.change(screen.getByLabelText(/end date/i), { target: { value: '2023-12-31' } })
    await user.click(screen.getByRole('button', { name: /train model/i }))
    expect(await screen.findByTestId('ml-train-error')).toHaveTextContent(/Not enough complete rows/i)
    // The form remains interactive after the failure.
    expect(screen.getByRole('button', { name: /train model/i })).toBeEnabled()
  })
})

// ------------------------------------------------------ comparison page

describe('MLComparisonPage', () => {
  const FAKE_COMPARE = {
    success: true,
    ticker: 'AAPL',
    models_run: 2,
    models_failed: 1,
    best_by_f1: 'Random Forest',
    comparison: [
      { Model: 'Random Forest', Status: 'ok', Accuracy: 0.61, Precision: 0.55, Recall: 0.48, F1: 0.51, ROC_AUC: 0.58, PR_AUC: 0.42, CAGR: 0.05, Sharpe: 0.7, Sortino: 0.9, MaxDrawdown: -0.12, WinRate: 0.52, Volatility: 0.14, TotalTrades: 9 },
      { Model: 'LSTM', Status: 'ok', Accuracy: 0.4, Precision: 0.34, Recall: 0.35, F1: 0.34, ROC_AUC: 0.49, PR_AUC: 0.34, CAGR: -0.02, Sharpe: -0.2, Sortino: -0.3, MaxDrawdown: -0.2, WinRate: 0.44, Volatility: 0.16, TotalTrades: 12 },
      { Model: 'Logistic Regression', Status: 'failed', Error: 'singular fit' },
    ],
    per_model: {
      random_forest: {
        model: 'random_forest',
        model_name: 'Random Forest',
        validation_metrics: {
          accuracy: 0.61, f1: 0.51,
          confusion_matrix: [],
          curves: [
            { class: 'BUY', roc: { fpr: { 1: 0, 2: 0.5, 3: 1 }, tpr: { 1: 0, 2: 0.7, 3: 1 }, auc: 0.7 }, pr: { recall: { 1: 0, 2: 0.5, 3: 1 }, precision: { 1: 0.4, 2: 0.5, 3: 0.4 }, auc: 0.45 } },
          ],
        },
        backtest: { engine: 'e', metrics: {} },
      },
    },
  }

  it('runs the comparison and renders the unified table + failures', async () => {
    const user = userEvent.setup()
    mlService.compareMlModels.mockResolvedValue(FAKE_COMPARE)
    renderPage(<MLComparisonPage />, '/ml-compare')

    await user.type(await screen.findByLabelText(/ticker/i), 'X')
    await user.click(screen.getByRole('button', { name: /compare all 12 models/i }))

    await waitFor(() => expect(mlService.compareMlModels).toHaveBeenCalledTimes(1))
    expect(await screen.findByText(/Comparison · AAPL · 2 model\(s\)/i)).toBeInTheDocument()
    expect(screen.getByText(/failed: singular fit/)).toBeInTheDocument()
    expect(screen.getByText(/best F1: Random Forest/i)).toBeInTheDocument()
  })

  it('shows an API error and stays interactive', async () => {
    const user = userEvent.setup()
    mlService.compareMlModels.mockRejectedValue({
      response: { data: { error: 'Could not fetch market data for ticker.' } },
    })
    renderPage(<MLComparisonPage />, '/ml-compare')
    await user.type(await screen.findByLabelText(/ticker/i), 'X')
    await user.click(screen.getByRole('button', { name: /compare all 12 models/i }))
    expect(await screen.findByText(/Could not fetch market data/i)).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /compare all 12 models/i })).toBeEnabled()
  })

  it('populates the ticker via the shared autocomplete suggestions', async () => {
    dataService.searchTickers.mockResolvedValue({
      success: true,
      query: 'RELI',
      count: 2,
      results: [
        { symbol: 'RELIANCE.NS', name: 'RELIANCE INDUSTRIES LTD', exchange: 'NSE' },
        { symbol: 'RELIANCE.BO', name: 'RELIANCE INDUSTRIES', exchange: 'BSE' },
      ],
    })
    const user = userEvent.setup()
    mlService.compareMlModels.mockResolvedValue(FAKE_COMPARE)
    renderPage(<MLComparisonPage />, '/ml-compare')

    const tickerInput = await screen.findByLabelText(/ticker/i)
    await user.type(tickerInput, 'RELI')
    const items = await screen.findAllByTestId('ticker-suggestion')
    await user.click(items[0])
    expect(tickerInput).toHaveValue('RELIANCE.NS')

    await user.click(screen.getByRole('button', { name: /compare all 12 models/i }))
    await waitFor(() => expect(mlService.compareMlModels).toHaveBeenCalledTimes(1))
    expect(mlService.compareMlModels.mock.calls[0][0].ticker).toBe('RELIANCE.NS')
  })

  it('accepts a manually typed NSE ticker (search unavailable)', async () => {
    dataService.searchTickers.mockRejectedValue(new Error('search endpoint down'))
    const user = userEvent.setup()
    mlService.compareMlModels.mockResolvedValue(FAKE_COMPARE)
    renderPage(<MLComparisonPage />, '/ml-compare')

    const tickerInput = await screen.findByLabelText(/ticker/i)
    await user.clear(tickerInput)
    await user.type(tickerInput, 'RELIANCE.NS')
    // Suggestion dropdown never appears; manual entry is untouched.
    expect(screen.queryByTestId('ticker-suggestions')).not.toBeInTheDocument()
    await waitFor(() =>
      expect(screen.getByTestId('ticker-search-error')).toHaveTextContent(/manually/i),
    )

    await user.click(screen.getByRole('button', { name: /compare all 12 models/i }))
    await waitFor(() => expect(mlService.compareMlModels).toHaveBeenCalledTimes(1))
    expect(mlService.compareMlModels.mock.calls[0][0].ticker).toBe('RELIANCE.NS')
  })

  it('persists the completed comparison to the namespaced session storage', async () => {
    const user = userEvent.setup()
    mlService.compareMlModels.mockResolvedValue(FAKE_COMPARE)
    renderPage(<MLComparisonPage />, '/ml-compare')

    await user.type(await screen.findByLabelText(/ticker/i), 'X')
    await user.click(screen.getByRole('button', { name: /compare all 12 models/i }))
    await screen.findByText(/Comparison · AAPL · 2 model\(s\)/i)

    const raw = sessionStorage.getItem('alphaforge_ml_compare_result')
    expect(raw).toBeTruthy()
    const saved = JSON.parse(raw)
    expect(saved.result.ticker).toBe('AAPL')
    expect(saved.result.comparison.length).toBe(3)
    expect(saved.form.horizon).toBe(ML_FORM_DEFAULTS.horizon)
  })

  it('restores the completed comparison after the page unmounts (route change)', async () => {
    const user = userEvent.setup()
    mlService.compareMlModels.mockResolvedValue(FAKE_COMPARE)
    const { unmount } = renderPage(<MLComparisonPage />, '/ml-compare')

    await user.type(await screen.findByLabelText(/ticker/i), 'X')
    await user.click(screen.getByRole('button', { name: /compare all 12 models/i }))
    await screen.findByText(/Comparison · AAPL · 2 model\(s\)/i)
    expect(screen.getByTestId('mlcmp-table')).toBeInTheDocument()

    // Navigating to /results unmounts the page; navigating back mounts a fresh
    // instance with zero in-memory state — exactly what a route transition
    // does in the running app. The previous comparison must come back.
    unmount()
    renderPage(<MLComparisonPage />, '/ml-compare')

    expect(await screen.findByTestId('mlcmp-table')).toBeInTheDocument()
    expect(screen.getByTestId('mlcmp-chart')).toBeInTheDocument()
    expect(screen.getByTestId('mlcmp-roc')).toBeInTheDocument()
    expect(screen.getByText(/best F1: Random Forest/i)).toBeInTheDocument()
    // The restored comparison was NOT re-run.
    expect(mlService.compareMlModels).toHaveBeenCalledTimes(1)
  })

  it('restores the comparison purely from session storage on a browser refresh', async () => {
    // Simulates a hard refresh at /ml-compare: page state is gone, only the
    // namespaced sessionStorage snapshot survives (it is written on save).
    sessionStorage.setItem(
      'alphaforge_ml_compare_result',
      JSON.stringify({ result: FAKE_COMPARE, form: validForm() }),
    )
    renderPage(<MLComparisonPage />, '/ml-compare')

    expect(await screen.findByTestId('mlcmp-table')).toBeInTheDocument()
    expect(screen.getByTestId('mlcmp-chart')).toBeInTheDocument()
    expect(screen.getByTestId('mlcmp-roc')).toBeInTheDocument()
    expect(screen.getByText(/best F1: Random Forest/i)).toBeInTheDocument()
    expect(mlService.compareMlModels).not.toHaveBeenCalled()
  })
})

// ---------------------------------------------------- backtest page

describe('MLBacktestPage', () => {
  const FAKE_BT = {
    success: true,
    backtestId: 77,
    model_name: 'Random Forest',
    config: { top_n: 2, rebalance_days: 5 },
    portfolio: {
      metrics: { totalReturn: 0.12, cagr: 0.08, sharpe: 1.1, sortino: 1.4, maximumDrawdown: -0.09, winRate: 0.57, volatility: 0.11, totalTrades: 14 },
    },
    rebalances: [
      { date: '2023-02-01', holdings: [{ ticker: 'INFY.NS', prob_buy: 0.82, rank: 1 }, { ticker: 'TCS.NS', prob_buy: 0.74, rank: 2 }] },
    ],
  }

  it('manages the ticker universe and submits the Top-N payload', async () => {
    const user = userEvent.setup()
    mlService.runMlBacktest.mockResolvedValue(FAKE_BT)
    renderPage(<MLBacktestPage />, '/ml-backtest')

    // Default preset tickers are present; add one more.
    expect(screen.getByText('RELIANCE.NS')).toBeInTheDocument()
    const input = screen.getByPlaceholderText(/add ticker/i)
    await user.type(input, 'wipro.ns{Enter}')
    expect(screen.getByText('WIPRO.NS')).toBeInTheDocument()

    await user.click(screen.getByTestId('mlbt-run-button'))
    await waitFor(() => expect(mlService.runMlBacktest).toHaveBeenCalledTimes(1))
    const payload = mlService.runMlBacktest.mock.calls[0][0]
    expect(payload.tickers).toContain('WIPRO.NS')
    expect(payload.top_n).toBe(5)
    expect(payload.rebalance).toBe('weekly')
    expect(payload.buy_threshold).toBeCloseTo(0.02)

    // Portfolio metrics + rebalance audit trail rendered.
    expect(await screen.findByText(/12\.00%/)).toBeInTheDocument()
    expect(screen.getByText(/INFY\.NS \(0\.82\)/)).toBeInTheDocument()
    // The run persists to the existing backtests tables; the page signals
    // this in the result panel (navigation is covered by the router mock).
    expect(screen.queryByText(/not persisted/i)).not.toBeInTheDocument()
  })

  it('prevents duplicate tickers in the universe', async () => {
    const user = userEvent.setup()
    mlService.runMlBacktest.mockResolvedValue(FAKE_BT)
    renderPage(<MLBacktestPage />, '/ml-backtest')

    // RELIANCE.NS is a preset; re-adding it (any casing) must be a no-op.
    const input = screen.getByPlaceholderText(/add ticker/i)
    await user.type(input, 'reliance.ns{Enter}')
    expect(screen.getAllByText('RELIANCE.NS').length).toBe(1)

    await user.click(screen.getByTestId('mlbt-run-button'))
    await waitFor(() => expect(mlService.runMlBacktest).toHaveBeenCalledTimes(1))
    const payload = mlService.runMlBacktest.mock.calls[0][0]
    expect(payload.tickers.filter((t) => t === 'RELIANCE.NS').length).toBe(1)
  })

  it('enforces the maximum universe size of 10 tickers', async () => {
    const user = userEvent.setup()
    mlService.runMlBacktest.mockResolvedValue(FAKE_BT)
    renderPage(<MLBacktestPage />, '/ml-backtest')

    const input = screen.getByPlaceholderText(/add ticker/i)
    // 5 presets + 6 attempts: the 6th must be ignored (10 max).
    for (let i = 1; i <= 6; i++) {
      await user.type(input, `newtick${i}.ns{Enter}`)
    }
    const chips = screen.getAllByText(/\.NS$/)
    expect(chips.length).toBe(10)
    expect(screen.queryByText('NEWTICK6.NS')).not.toBeInTheDocument()

    await user.click(screen.getByTestId('mlbt-run-button'))
    await waitFor(() => expect(mlService.runMlBacktest).toHaveBeenCalledTimes(1))
    const payload = mlService.runMlBacktest.mock.calls[0][0]
    expect(payload.tickers.length).toBe(10)
  })

  it('accepts NSE tickers longer than 10 characters in the universe', async () => {
    const user = userEvent.setup()
    mlService.runMlBacktest.mockResolvedValue(FAKE_BT)
    renderPage(<MLBacktestPage />, '/ml-backtest')

    const input = screen.getByPlaceholderText(/add ticker/i)
    await user.type(input, 'MOTHERSON.NS{Enter}') // 12 chars
    expect(screen.getByText('MOTHERSON.NS')).toBeInTheDocument()

    await user.click(screen.getByTestId('mlbt-run-button'))
    await waitFor(() => expect(mlService.runMlBacktest).toHaveBeenCalledTimes(1))
    const payload = mlService.runMlBacktest.mock.calls[0][0]
    expect(payload.tickers).toContain('MOTHERSON.NS')
  })

  it('shows the API error when the portfolio run fails', async () => {
    const user = userEvent.setup()
    mlService.runMlBacktest.mockRejectedValue({
      response: { data: { error: 'No ticker produced enough predictions.' } },
    })
    renderPage(<MLBacktestPage />, '/ml-backtest')
    await user.click(screen.getByTestId('mlbt-run-button'))
    expect(await screen.findByText(/No ticker produced enough predictions/i)).toBeInTheDocument()
  })
})
