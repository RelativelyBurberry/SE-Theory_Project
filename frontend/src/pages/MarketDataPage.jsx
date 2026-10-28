import { useState } from 'react'
import { validateTicker, fetchMarketData } from '../services/dataService'

const PREVIEW_MAX_ROWS = 15

/**
 * Market Data page (Phase 3) — minimal stock-selection interface for
 * exercising the market-data service:
 *
 *   - ticker / start / end inputs
 *   - Validate button (GET /stocks/validate)
 *   - Fetch data button (GET /market-data, JWT-protected)
 *   - loading + error states
 *   - a basic preview table of the returned OHLCV rows
 *
 * The complete Strategy Builder arrives in Phase 4+; no market data is ever
 * fabricated in the UI — everything shown comes from the backend response.
 */
export default function MarketDataPage() {
  const [ticker, setTicker] = useState('AAPL')
  const [start, setStart] = useState('2024-01-01')
  const [end, setEnd] = useState('2024-03-01')

  const [validating, setValidating] = useState(false)
  const [fetching, setFetching] = useState(false)
  const [validation, setValidation] = useState(null) // { valid, error }
  const [payload, setPayload] = useState(null) // { ticker, count, data, ... }
  const [error, setError] = useState(null)

  async function handleValidate() {
    setValidating(true)
    setValidation(null)
    setError(null)
    try {
      const result = await validateTicker(ticker)
      setValidation(result)
    } catch (err) {
      setError(err?.response?.data?.error ?? 'Unable to validate ticker.')
    } finally {
      setValidating(false)
    }
  }

  async function handleFetch() {
    setFetching(true)
    setPayload(null)
    setError(null)
    try {
      const result = await fetchMarketData(ticker, start, end)
      setPayload(result)
    } catch (err) {
      setError(err?.response?.data?.error ?? 'Unable to fetch market data.')
    } finally {
      setFetching(false)
    }
  }

  const rows = payload?.data ?? []
  const preview = rows.slice(0, PREVIEW_MAX_ROWS)

  return (
    <section className="space-y-6">
      <div>
        <h1 className="text-2xl font-semibold text-slate-900">Market Data</h1>
        <p className="mt-1 text-slate-600">
          Validate a stock ticker and preview historical daily OHLCV data.
          Data is fetched live from the backend market-data service.
        </p>
      </div>

      <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm">
        <div className="grid gap-4 sm:grid-cols-4">
          <label className="block">
            <span className="text-sm font-medium text-slate-700">Ticker</span>
            <input
              type="text"
              value={ticker}
              onChange={(e) => setTicker(e.target.value.toUpperCase())}
              placeholder="AAPL"
              className="mt-1 w-full rounded border border-slate-300 px-3 py-2 text-sm"
            />
          </label>
          <label className="block">
            <span className="text-sm font-medium text-slate-700">Start date</span>
            <input
              type="date"
              value={start}
              onChange={(e) => setStart(e.target.value)}
              className="mt-1 w-full rounded border border-slate-300 px-3 py-2 text-sm"
            />
          </label>
          <label className="block">
            <span className="text-sm font-medium text-slate-700">End date</span>
            <input
              type="date"
              value={end}
              onChange={(e) => setEnd(e.target.value)}
              className="mt-1 w-full rounded border border-slate-300 px-3 py-2 text-sm"
            />
          </label>
          <div className="flex items-end gap-2">
            <button
              type="button"
              onClick={handleValidate}
              disabled={validating || !ticker}
              className="rounded bg-slate-700 px-3 py-2 text-sm font-medium text-white hover:bg-slate-600 disabled:opacity-50"
            >
              {validating ? 'Checking…' : 'Validate'}
            </button>
            <button
              type="button"
              onClick={handleFetch}
              disabled={fetching || !ticker || !start || !end}
              className="rounded bg-emerald-600 px-3 py-2 text-sm font-medium text-white hover:bg-emerald-500 disabled:opacity-50"
            >
              {fetching ? 'Fetching…' : 'Fetch data'}
            </button>
          </div>
        </div>

        {validation && (
          <p
            className={`mt-3 text-sm font-medium ${
              validation.valid ? 'text-emerald-600' : 'text-red-600'
            }`}
            data-testid="validation-result"
          >
            {validation.valid
              ? `${validation.ticker} is a valid ticker.`
              : `${validation.ticker} is not valid: ${validation.error ?? 'no market data found.'}`}
          </p>
        )}

        {error && (
          <p className="mt-3 text-sm font-medium text-red-600" data-testid="fetch-error">
            {error}
          </p>
        )}
      </div>

      {payload && (
        <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm">
          <h2 className="font-medium text-slate-800">
            {payload.ticker} — {rows.length} rows
          </h2>
          <p className="mt-1 text-sm text-slate-500">
            {payload.start} → {payload.end}
            {rows.length > PREVIEW_MAX_ROWS &&
              ` · showing first ${PREVIEW_MAX_ROWS} rows`}
          </p>
          <div className="mt-3 overflow-x-auto">
            <table className="w-full text-left text-sm">
              <thead>
                <tr className="border-b border-slate-200 text-slate-500">
                  <th className="py-2 pr-3 font-medium">Date</th>
                  <th className="py-2 pr-3 font-medium">Open</th>
                  <th className="py-2 pr-3 font-medium">High</th>
                  <th className="py-2 pr-3 font-medium">Low</th>
                  <th className="py-2 pr-3 font-medium">Close</th>
                  <th className="py-2 pr-3 font-medium">Adj. Close</th>
                  <th className="py-2 font-medium">Volume</th>
                </tr>
              </thead>
              <tbody>
                {preview.map((row) => (
                  <tr key={row.date} className="border-b border-slate-100">
                    <td className="py-2 pr-3 text-slate-700">{row.date}</td>
                    <td className="py-2 pr-3">{row.open?.toFixed(2)}</td>
                    <td className="py-2 pr-3">{row.high?.toFixed(2)}</td>
                    <td className="py-2 pr-3">{row.low?.toFixed(2)}</td>
                    <td className="py-2 pr-3">{row.close?.toFixed(2)}</td>
                    <td className="py-2 pr-3">{row.adjusted_close?.toFixed(2)}</td>
                    <td className="py-2">{row.volume?.toLocaleString()}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {!payload && !error && (
        <p className="text-sm text-slate-500">
          Press “Fetch data” to retrieve historical OHLCV for the selected
          ticker and date range.
        </p>
      )}
    </section>
  )
}