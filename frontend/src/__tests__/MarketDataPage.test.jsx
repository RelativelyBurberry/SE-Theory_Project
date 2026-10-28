import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import MarketDataPage from '../pages/MarketDataPage'

// Mock the market-data service so tests never hit the network.
vi.mock('../services/dataService', () => ({
  validateTicker: vi.fn(),
  fetchMarketData: vi.fn(),
}))
import * as dataService from '../services/dataService'

const FAKE_PAYLOAD = {
  success: true,
  ticker: 'AAPL',
  start: '2024-01-01',
  end: '2024-01-03',
  count: 2,
  data: [
    {
      date: '2024-01-02',
      open: 187.15,
      high: 188.44,
      low: 183.89,
      close: 185.64,
      adjusted_close: 183.404,
      volume: 82488700,
    },
    {
      date: '2024-01-03',
      open: 184.22,
      high: 185.88,
      low: 183.43,
      close: 184.25,
      adjusted_close: 182.0308,
      volume: 58414500,
    },
  ],
}

function renderPage() {
  return render(
    <MemoryRouter>
      <MarketDataPage />
    </MemoryRouter>,
  )
}

describe('MarketDataPage (Phase 3)', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('renders ticker, date inputs and the Validate / Fetch buttons', () => {
    renderPage()
    expect(screen.getByLabelText(/ticker/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/start date/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/end date/i)).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /validate/i })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /fetch data/i })).toBeInTheDocument()
  })

  it('calls the validate service and shows the validity result', async () => {
    dataService.validateTicker.mockResolvedValue({ ticker: 'AAPL', valid: true })
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /validate/i }))
    expect(dataService.validateTicker).toHaveBeenCalledWith('AAPL')
    expect(await screen.findByTestId('validation-result')).toHaveTextContent(
      /AAPL is a valid ticker/i,
    )
  })

  it('shows the error message when validation fails', async () => {
    dataService.validateTicker.mockResolvedValue({
      ticker: 'NOPE',
      valid: false,
      error: 'No market data found for ticker',
    })
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /validate/i }))
    expect(await screen.findByTestId('validation-result')).toHaveTextContent(/not valid/i)
  })

  it('fetches market data and renders the preview table with real values', async () => {
    dataService.fetchMarketData.mockResolvedValue(FAKE_PAYLOAD)
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /fetch data/i }))
    expect(dataService.fetchMarketData).toHaveBeenCalledWith('AAPL', '2024-01-01', '2024-03-01')
    // Rows from the (mocked) API response are rendered — nothing fabricated.
    expect(await screen.findByText(/2 rows/i)).toBeInTheDocument()
    expect(await screen.findByText('2024-01-02')).toBeInTheDocument()
    expect(await screen.findByText('185.64')).toBeInTheDocument()
  })

  it('renders the backend error message when fetching fails', async () => {
    dataService.fetchMarketData.mockRejectedValue({
      response: { data: { success: false, error: 'start date must be on or before the end date.' } },
    })
    const user = userEvent.setup()
    renderPage()
    await user.click(screen.getByRole('button', { name: /fetch data/i }))
    expect(await screen.findByTestId('fetch-error')).toHaveTextContent(
      /start date must be on or before the end date/i,
    )
  })
})