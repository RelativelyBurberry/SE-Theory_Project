import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, within } from '@testing-library/react'
import { MemoryRouter, Routes, Route } from 'react-router-dom'
import DashboardPage from '../pages/DashboardPage'

// Mock the backtest service (no network).
vi.mock('../services/backtestService', () => ({
  getBacktest: vi.fn(),
  getBacktestHistory: vi.fn(),
  runBacktest: vi.fn(),
}))
import { getBacktestHistory } from '../services/backtestService'

// Mock the live news service (Phase H.4 widget on the dashboard; no network).
// The widget resolves with an empty article list so it renders its empty state
// deterministically without touching the backend.
vi.mock('../services/newsService', () => ({
  getLiveNews: vi.fn().mockResolvedValue({
    success: true,
    provider: 'marketaux',
    articles: [],
    retrieval_time: '2026-10-19T12:00:00Z',
  }),
  isNewsStale: vi.fn().mockReturnValue(false),
  formatRelativeTime: vi.fn().mockReturnValue('just now'),
  getSentimentDisplay: vi.fn().mockReturnValue({ label: null, color: null }),
}))

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

function renderPage() {
  return render(
    <MemoryRouter initialEntries={['/dashboard']}>
      <Routes>
        <Route path="/dashboard" element={<DashboardPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

describe('DashboardPage (Phase 6)', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('shows the empty state when the user has no backtests', async () => {
    getBacktestHistory.mockResolvedValue({ success: true, items: [], total: 0 })
    renderPage()
    expect(await screen.findByTestId('history-empty')).toHaveTextContent(
      /no saved backtests/i,
    )
  })

  it('shows a loading state while fetching history', () => {
    getBacktestHistory.mockImplementation(
      () => new Promise(() => {}), // never resolves
    )
    renderPage()
    expect(screen.getByTestId('history-loading')).toBeInTheDocument()
  })

  it('renders the populated history list with links to results', async () => {
    getBacktestHistory.mockResolvedValue({
      success: true,
      items: HISTORY_ITEMS,
      total: 2,
    })
    renderPage()
    const historyList = await screen.findByTestId('history-list')
    expect(historyList).toBeInTheDocument()
    const items = within(historyList).getAllByTestId('history-item')
    expect(items).toHaveLength(2)
    // Scope ticker assertions to history list (Live News widget also shows ticker buttons).
    expect(within(historyList).getByText('AAPL')).toBeInTheDocument()
    expect(within(historyList).getByText('MSFT')).toBeInTheDocument()
    // Total return +5.32% present; the null-total-return row shows an em dash.
    expect(screen.getByText('+5.32%')).toBeInTheDocument()
    // One "View" link per row inside the history list (the page also links to
    // "View results" in the features section, so scope to the list).
    const views = within(historyList).getAllByRole('link', {
      name: /view/i,
    })
    expect(views).toHaveLength(2)
    expect(views[0]).toHaveAttribute('href', '/results/1')
  })

  it('renders the backend error when the history fetch fails', async () => {
    getBacktestHistory.mockRejectedValue({
      response: { data: { success: false, error: 'Database connection failed.' } },
    })
    renderPage()
    expect(await screen.findByTestId('history-error')).toHaveTextContent(
      /database connection failed/i,
    )
  })
})