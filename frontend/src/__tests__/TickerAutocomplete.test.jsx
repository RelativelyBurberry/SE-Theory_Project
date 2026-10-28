import { describe, it, expect, vi, beforeEach } from 'vitest'
import { useState } from 'react'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import TickerAutocomplete, { MAX_SUGGESTIONS } from '../components/TickerAutocomplete'
import {
  isValidTickerFormat, normalizeTicker, tickerFormatError,
  TICKER_MAX_LENGTH, MAX_UNIVERSE_SIZE,
} from '../utils/ticker'
import {
  ML_FORM_DEFAULTS, validateMlForm, buildMlBacktestPayload,
} from '../utils/mlDefaults'
import { validateStrategyForm } from '../utils/strategyDefaults'

vi.mock('../services/dataService', () => ({
  validateTicker: vi.fn(),
  searchTickers: vi.fn(),
  fetchMarketData: vi.fn(),
}))
import * as dataService from '../services/dataService'

const SUGGESTIONS = {
  success: true,
  query: 'RELI',
  count: 3,
  results: [
    { symbol: 'RELIANCE.NS', name: 'RELIANCE INDUSTRIES LTD', exchange: 'NSE' },
    { symbol: 'RELIANCE.BO', name: 'RELIANCE INDUSTRIES', exchange: 'BSE' },
    { symbol: 'RS', name: 'Reliance, Inc.', exchange: 'NYSE' },
  ],
}

beforeEach(() => {
  vi.clearAllMocks()
})

// ------------------------------------------------- 1-4. format validation

describe('shared ticker validation (utils/ticker.js)', () => {
  it('accepts RELIANCE.NS (11 characters)', () => {
    expect(isValidTickerFormat('RELIANCE.NS')).toBe(true)
  })

  it('accepts HDFCBANK.NS (10 characters)', () => {
    expect(isValidTickerFormat('HDFCBANK.NS')).toBe(true)
  })

  it('normalizes lowercase and whitespace (reliance.ns -> RELIANCE.NS)', () => {
    expect(normalizeTicker('  reliance.ns ')).toBe('RELIANCE.NS')
    expect(isValidTickerFormat('  reliance.ns ')).toBe(true)
  })

  it('rejects invalid characters', () => {
    expect(isValidTickerFormat('BAD!TICKER')).toBe(false)
    expect(isValidTickerFormat('A B')).toBe(false)
    expect(isValidTickerFormat('STOCK#1')).toBe(false)
  })

  it('rejects excessively long tickers', () => {
    expect(isValidTickerFormat('A'.repeat(TICKER_MAX_LENGTH))).toBe(true)
    expect(isValidTickerFormat('A'.repeat(TICKER_MAX_LENGTH + 1))).toBe(false)
    expect(isValidTickerFormat('ABCDEFGHIJKLMNOPQRSTUVW')).toBe(false)
  })

  it('rejects empty and whitespace-only input', () => {
    expect(isValidTickerFormat('')).toBe(false)
    expect(isValidTickerFormat('   ')).toBe(false)
  })

  it('shares the same message across both form validators', () => {
    const ml = validateMlForm({ ...ML_FORM_DEFAULTS, ticker: 'BAD!' })
    const strat = validateStrategyForm({ ticker: 'BAD!' })
    expect(ml.errors.ticker).toBe(tickerFormatError())
    expect(strat.errors.ticker).toBe(tickerFormatError())
    expect(ml.errors.ticker).toMatch(/RELIANCE\.NS/)
  })
})

// ------------------------------------------------ 5-7. search + suggestions + select

/** Stateful harness mirroring how the real pages host the component. */
function AutocompleteHarness({ onSelect, onSelectBehavior, onSubmit, id = 'ta-harness', ...rest }) {
  const [value, setValue] = useState('')
  return (
    <div>
      <TickerAutocomplete
        id={id}
        value={value}
        onChange={setValue}
        onSelect={onSelect}
        onSelectBehavior={onSelectBehavior}
        onSubmit={onSubmit}
        {...rest}
      />
      <button type="button">outside</button>
      <p data-testid="current-value">{value}</p>
    </div>
  )
}

describe('TickerAutocomplete (shared component)', () => {
  function input() {
    return screen.getByRole('combobox')
  }

  it('triggers a debounced ticker search while typing', async () => {
    dataService.searchTickers.mockResolvedValue(SUGGESTIONS)
    const user = userEvent.setup()
    render(<AutocompleteHarness />)

    await user.type(input(), 'RELI')
    // Not called before the debounce window elapses.
    expect(dataService.searchTickers).not.toHaveBeenCalled()
    await waitFor(() => expect(dataService.searchTickers).toHaveBeenCalledTimes(1))
    expect(dataService.searchTickers).toHaveBeenCalledWith('RELI', MAX_SUGGESTIONS)
  })

  it('does not search for 0-1 characters', async () => {
    dataService.searchTickers.mockResolvedValue(SUGGESTIONS)
    const user = userEvent.setup()
    render(<AutocompleteHarness />)
    await user.type(input(), 'R')
    await new Promise((r) => setTimeout(r, 400))
    expect(dataService.searchTickers).not.toHaveBeenCalled()
    expect(screen.queryByTestId('ticker-suggestions')).not.toBeInTheDocument()
  })

  it('renders suggestions from the API response', async () => {
    dataService.searchTickers.mockResolvedValue(SUGGESTIONS)
    const user = userEvent.setup()
    render(<AutocompleteHarness />)
    await user.type(input(), 'RELI')
    const list = await screen.findByTestId('ticker-suggestions')
    expect(list).toBeInTheDocument()
    expect(screen.getByText('RELIANCE.NS')).toBeInTheDocument()
    expect(screen.getByText('RELIANCE.BO')).toBeInTheDocument()
    expect(screen.getByText('RS')).toBeInTheDocument()
    // Company names render next to the symbols.
    expect(screen.getByText(/RELIANCE INDUSTRIES LTD/i)).toBeInTheDocument()
  })

  it('fills the input when a suggestion is clicked (replace behavior)', async () => {
    dataService.searchTickers.mockResolvedValue(SUGGESTIONS)
    const user = userEvent.setup()
    const onSelect = vi.fn()
    render(<AutocompleteHarness id="ta-click" onSelect={onSelect} />)
    await user.type(input(), 'RELI')
    const items = await screen.findAllByTestId('ticker-suggestion')
    await user.click(items[0])
    expect(onSelect).toHaveBeenCalledWith(
      expect.objectContaining({ symbol: 'RELIANCE.NS' }),
    )
    expect(screen.getByTestId('current-value')).toHaveTextContent('RELIANCE.NS')
  })

  it('selects with keyboard: arrows move highlight, Enter fills, Escape closes', async () => {
    dataService.searchTickers.mockResolvedValue(SUGGESTIONS)
    const user = userEvent.setup()
    const onSelect = vi.fn()
    render(<AutocompleteHarness id="ta-kbd" onSelect={onSelect} />)
    await user.type(input(), 'RELI')
    await screen.findByTestId('ticker-suggestions')

    await user.keyboard('{ArrowDown}') // highlight 0
    await user.keyboard('{ArrowDown}') // highlight 1
    const options = screen.getAllByTestId('ticker-suggestion')
    expect(options[1].closest('li')).toHaveAttribute('aria-selected', 'true')

    await user.keyboard('{Enter}')
    expect(onSelect).toHaveBeenCalledWith(expect.objectContaining({ symbol: 'RELIANCE.BO' }))
    expect(screen.getByTestId('current-value')).toHaveTextContent('RELIANCE.BO')
    expect(screen.queryByTestId('ticker-suggestions')).not.toBeInTheDocument()

    // Escape closes an open dropdown without selecting.
    onSelect.mockClear()
    await user.type(input(), 'AMZ')
    await screen.findByTestId('ticker-suggestions')
    await user.keyboard('{Escape}')
    expect(screen.queryByTestId('ticker-suggestions')).not.toBeInTheDocument()
    expect(onSelect).not.toHaveBeenCalled()
  })

  it('submits the chosen symbol in submit mode (portfolio add)', async () => {
    dataService.searchTickers.mockResolvedValue(SUGGESTIONS)
    const user = userEvent.setup()
    const onSelect = vi.fn()
    const onSubmit = vi.fn()
    render(
      <AutocompleteHarness id="ta-submit" onSelect={onSelect} onSelectBehavior="submit" onSubmit={onSubmit} />,
    )
    await user.type(input(), 'RELI')
    await user.click((await screen.findAllByTestId('ticker-suggestion'))[0])
    expect(onSelect).toHaveBeenCalledWith(expect.objectContaining({ symbol: 'RELIANCE.NS' }))
    expect(onSubmit).toHaveBeenCalledWith('RELIANCE.NS')
    expect(screen.getByTestId('current-value')).toHaveTextContent('RELIANCE.NS')
  })

  it('Enter with an exactly typed symbol wins over the first suggestion', async () => {
    // API ranks WIPRO.BO first; typing WIPRO.NS + Enter must add WIPRO.NS.
    dataService.searchTickers.mockResolvedValue({
      success: true,
      query: 'WIPRO',
      count: 2,
      results: [
        { symbol: 'WIPRO.BO', name: 'Wipro Limited', exchange: 'BSE' },
        { symbol: 'WIPRO.NS', name: 'Wipro Limited', exchange: 'NSE' },
      ],
    })
    const user = userEvent.setup()
    const onSubmit = vi.fn()
    render(
      <AutocompleteHarness id="ta-exact" onSelectBehavior="submit" onSubmit={onSubmit} />,
    )
    await user.type(input(), 'WIPRO.NS')
    await screen.findByTestId('ticker-suggestions')
    await user.keyboard('{Enter}')
    expect(onSubmit).toHaveBeenCalledWith('WIPRO.NS')
    expect(onSubmit).not.toHaveBeenCalledWith('WIPRO.BO')
  })

  it('shows no dropdown when there are no results', async () => {
    dataService.searchTickers.mockResolvedValue({ success: true, count: 0, results: [] })
    const user = userEvent.setup()
    render(<AutocompleteHarness id="ta-empty" />)
    await user.type(input(), 'ZZZZZ')
    await waitFor(() => expect(dataService.searchTickers).toHaveBeenCalled())
    await new Promise((r) => setTimeout(r, 50))
    expect(screen.queryByTestId('ticker-suggestions')).not.toBeInTheDocument()
  })

  it('shows a loading state while the search is in flight', async () => {
    let resolveSearch
    dataService.searchTickers.mockImplementation(
      () => new Promise((resolve) => { resolveSearch = resolve }),
    )
    const user = userEvent.setup()
    render(<AutocompleteHarness id="ta-loading" />)
    await user.type(input(), 'AAPL')
    // The indicator appears immediately, then persists until the debounced
    // request completes.
    await waitFor(() =>
      expect(screen.getByTestId('ticker-search-loading')).toBeInTheDocument(),
    )
    await waitFor(() => expect(dataService.searchTickers).toHaveBeenCalledTimes(1))
    resolveSearch(SUGGESTIONS)
    await waitFor(() =>
      expect(screen.queryByTestId('ticker-search-loading')).not.toBeInTheDocument(),
    )
  })

  it('closes the dropdown when clicking outside', async () => {
    dataService.searchTickers.mockResolvedValue(SUGGESTIONS)
    const user = userEvent.setup()
    render(<AutocompleteHarness id="ta-outside" />)
    await user.type(input(), 'RELI')
    await screen.findByTestId('ticker-suggestions')
    await user.click(screen.getByRole('button', { name: /outside/i }))
    expect(screen.queryByTestId('ticker-suggestions')).not.toBeInTheDocument()
  })

  it('degrades gracefully when the search API fails — manual entry still works', async () => {
    dataService.searchTickers.mockRejectedValue(new Error('network down'))
    const user = userEvent.setup()
    render(<AutocompleteHarness id="ta-fail" />)
    await user.type(input(), 'RELIANCE.NS')
    await waitFor(() =>
      expect(screen.getByTestId('ticker-search-error')).toHaveTextContent(/manually/i),
    )
    expect(screen.queryByTestId('ticker-suggestions')).not.toBeInTheDocument()
    // The typed text is preserved — nothing blocks manual submission.
    expect(screen.getByTestId('current-value')).toHaveTextContent('RELIANCE.NS')
  })

  it('caps suggestions at 5 regardless of response size', async () => {
    dataService.searchTickers.mockResolvedValue({
      success: true,
      count: 8,
      results: Array.from({ length: 8 }, (_, i) => ({
        symbol: `SYM${i}.NS`, name: `Company ${i}`,
      })),
    })
    const user = userEvent.setup()
    render(<AutocompleteHarness id="ta-cap" />)
    await user.type(input(), 'SYM')
    const items = await screen.findAllByTestId('ticker-suggestion')
    expect(items.length).toBe(MAX_SUGGESTIONS)
    expect(items.length).toBe(5)
  })
})

// ------------------------------ 8-10. portfolio guards + comparison page

describe('ML portfolio universe guards', () => {
  it('normalizes tickers in the backtest payload', () => {
    const p = buildMlBacktestPayload(
      { ...ML_FORM_DEFAULTS },
      [' reliance.ns ', 'tcs.ns'],
    )
    expect(p.tickers).toEqual(['RELIANCE.NS', 'TCS.NS'])
  })

  it('payload keeps the existing contract (top_n, rebalance, thresholds)', () => {
    const p = buildMlBacktestPayload({ ...ML_FORM_DEFAULTS, topN: '3' }, ['AAPL'])
    expect(p.top_n).toBe(3)
    expect(p.rebalance).toBe('weekly')
    expect(p.buy_threshold).toBeCloseTo(0.02)
  })
})

describe('ML pages use the same autocomplete component', () => {
  it('TICKER_MAX_LENGTH matches the backend constant (20)', () => {
    expect(TICKER_MAX_LENGTH).toBe(20)
    expect(MAX_UNIVERSE_SIZE).toBe(10)
  })
})
