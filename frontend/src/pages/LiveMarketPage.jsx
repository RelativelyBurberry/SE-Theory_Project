import { useCallback, useEffect, useMemo, useState } from 'react'
import {
  CartesianGrid,
  Line,
  LineChart,
  ResponsiveContainer,
  Tooltip,
  XAxis,
  YAxis,
} from 'recharts'
import LiveNewsWidget from '../components/LiveNewsWidget'
import { fetchMarketData } from '../services/dataService'
import { requestLiveForecast } from '../services/forecastService'

const REFRESH_MS = 5 * 60 * 1000

function dateString(date) {
  const year = date.getFullYear()
  const month = String(date.getMonth() + 1).padStart(2, '0')
  const day = String(date.getDate()).padStart(2, '0')
  return `${year}-${month}-${day}`
}

function money(value) {
  if (!Number.isFinite(value)) return '—'
  return value.toLocaleString(undefined, { style: 'currency', currency: 'USD', maximumFractionDigits: 2 })
}

function percent(value) {
  if (!Number.isFinite(value)) return '—'
  return `${value >= 0 ? '+' : ''}${(value * 100).toFixed(2)}%`
}

function MetricCard({ label, value, detail, valueClass = '' }) {
  return (
    <div className="rounded-xl border border-slate-200 bg-white p-4 shadow-sm">
      <p className="text-sm text-slate-500">{label}</p>
      <p className={`mt-2 text-2xl font-semibold tabular-nums ${valueClass}`}>{value}</p>
      {detail && <p className="mt-1 text-xs text-slate-500">{detail}</p>}
    </div>
  )
}

export default function LiveMarketPage() {
  const [tickerInput, setTickerInput] = useState('AAPL')
  const [ticker, setTicker] = useState('AAPL')
  const [marketData, setMarketData] = useState([])
  const [loading, setLoading] = useState(true)
  const [refreshing, setRefreshing] = useState(false)
  const [error, setError] = useState(null)
  const [lastUpdated, setLastUpdated] = useState(null)
  const [forecastData, setForecastData] = useState(null)
  const [forecastLoading, setForecastLoading] = useState(false)
  const [forecastError, setForecastError] = useState(null)
  const [horizons, setHorizons] = useState([1, 5, 10])

  const loadMarketData = useCallback(async ({ quiet = false } = {}) => {
    if (quiet) setRefreshing(true)
    else setLoading(true)
    setError(null)
    try {
      const end = new Date()
      const start = new Date(end)
      start.setDate(start.getDate() - 120)
      const result = await fetchMarketData(ticker, dateString(start), dateString(end))
      const rows = (result.data ?? [])
        .map((row) => ({
          ...row,
          close: Number(row.close),
          open: Number(row.open),
          high: Number(row.high),
          low: Number(row.low),
          volume: Number(row.volume),
        }))
        .filter((row) => row.date && Number.isFinite(row.close))
        .sort((a, b) => String(a.date).localeCompare(String(b.date)))
      if (!rows.length) throw new Error('The data provider returned no usable daily candles for this ticker.')
      setMarketData(rows)
      setLastUpdated(new Date())
    } catch (err) {
      setError(err?.response?.data?.error ?? err.message ?? 'Unable to fetch market data.')
    } finally {
      setLoading(false)
      setRefreshing(false)
    }
  }, [ticker])

  useEffect(() => {
    loadMarketData()
    const interval = setInterval(() => loadMarketData({ quiet: true }), REFRESH_MS)
    return () => clearInterval(interval)
  }, [loadMarketData])

  const stats = useMemo(() => {
    if (!marketData.length) return null
    const latest = marketData[marketData.length - 1]
    const previous = marketData.length > 1 ? marketData[marketData.length - 2] : null
    const smaRows = marketData.slice(-20)
    const sma20 = smaRows.length === 20 ? smaRows.reduce((sum, row) => sum + row.close, 0) / 20 : null
    const change = previous && previous.close !== 0 ? latest.close / previous.close - 1 : null
    return { latest, previous, sma20, change }
  }, [marketData])

  const loadForecasts = useCallback(async () => {
    setForecastLoading(true)
    setForecastError(null)
    try {
      const result = await requestLiveForecast({ ticker, horizons, history_period: '5y' })
      setForecastData(result)
    } catch (err) {
      setForecastError(err?.response?.data?.detail ?? err.message ?? 'Unable to generate forecasts.')
    } finally {
      setForecastLoading(false)
    }
  }, [ticker, horizons])

  useEffect(() => {
    setForecastData(null)
    setForecastError(null)
  }, [ticker])

  function applyTicker(event) {
    event.preventDefault()
    const next = tickerInput.trim().toUpperCase()
    if (/^[A-Z0-9.^-]{1,20}$/.test(next)) setTicker(next)
    else setError('Enter a valid ticker symbol (letters, numbers, dots, hyphens or ^).')
  }

  const chartRows = marketData.slice(-60)

  return (
    <section className="space-y-6">
      <div className="flex flex-col justify-between gap-4 sm:flex-row sm:items-end">
        <div>
          <h1 className="text-2xl font-semibold text-slate-900">Live Market Monitor</h1>
          <p className="mt-1 max-w-2xl text-sm text-slate-600">
            Latest available daily market data, price movement, trend context and financial headlines in one place.
          </p>
        </div>
        <form onSubmit={applyTicker} className="flex gap-2">
          <label className="sr-only" htmlFor="live-market-ticker">Ticker symbol</label>
          <input id="live-market-ticker" value={tickerInput} onChange={(event) => setTickerInput(event.target.value.toUpperCase())}
            className="w-28 rounded-md border border-slate-300 bg-white px-3 py-2 text-sm" maxLength={20} />
          <button type="submit" className="rounded-md bg-emerald-600 px-4 py-2 text-sm font-medium text-white hover:bg-emerald-700">Load ticker</button>
          <button type="button" onClick={() => loadMarketData({ quiet: true })} disabled={refreshing || loading}
            className="rounded-md border border-slate-300 bg-white px-3 py-2 text-sm font-medium text-slate-700 disabled:opacity-50">
            {refreshing ? 'Refreshing…' : 'Refresh'}
          </button>
        </form>
      </div>

      <div className="flex flex-wrap items-center justify-between gap-2 rounded-lg border border-sky-200 bg-sky-50 px-4 py-3 text-sm text-sky-900">
        <span><strong>Data freshness:</strong> Yahoo Finance daily candles; refreshes this page every 5 minutes.</span>
        <span className="text-xs text-sky-700">Not exchange-grade real-time quotes</span>
      </div>

      {error && <div role="alert" className="rounded-lg border border-red-200 bg-red-50 p-4 text-sm text-red-700">{error}</div>}
      {loading ? <div className="rounded-xl border border-slate-200 bg-white p-8 text-sm text-slate-500">Loading latest available market data…</div> : stats && (
        <>
          <div className="grid gap-3 sm:grid-cols-2 xl:grid-cols-4">
            <MetricCard label={`${ticker} latest close`} value={money(stats.latest.close)} detail={`Trading date: ${stats.latest.date}`} />
            <MetricCard label="Daily change" value={percent(stats.change)} detail="Versus previous available close"
              valueClass={stats.change > 0 ? 'text-emerald-600' : stats.change < 0 ? 'text-red-600' : 'text-slate-900'} />
            <MetricCard label="20-day simple average" value={money(stats.sma20)} detail="Trend reference, not a prediction" />
            <MetricCard label="Latest volume" value={Number.isFinite(stats.latest.volume) ? stats.latest.volume.toLocaleString() : '—'} detail="Shares reported for latest candle" />
          </div>

          <div className="rounded-xl border border-slate-200 bg-white p-5 shadow-sm">
            <div className="mb-4 flex flex-wrap items-start justify-between gap-2">
              <div>
                <h2 className="font-semibold text-slate-900">{ticker} closing price</h2>
                <p className="mt-1 text-xs text-slate-500">Last {chartRows.length} available daily observations</p>
              </div>
              {lastUpdated && <p className="text-xs text-slate-500">Fetched {lastUpdated.toLocaleTimeString()}</p>}
            </div>
            <div className="h-72 w-full" role="img" aria-label={`${ticker} closing price chart`}>
              <ResponsiveContainer width="100%" height="100%">
                <LineChart data={chartRows} margin={{ top: 8, right: 12, left: 4, bottom: 4 }}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey="date" tick={{ fontSize: 11 }} minTickGap={30} />
                  <YAxis domain={['auto', 'auto']} tick={{ fontSize: 11 }} width={65} />
                  <Tooltip formatter={(value) => [money(Number(value)), 'Close']} labelFormatter={(label) => `Date: ${label}`} />
                  <Line type="monotone" dataKey="close" stroke="#059669" strokeWidth={2} dot={false} isAnimationActive={false} />
                </LineChart>
              </ResponsiveContainer>
            </div>
            <p className="mt-3 text-xs text-slate-500">The latest candle may be from the previous trading session when the market is closed or the provider has not published a newer candle.</p>
          </div>
        </>
      )}

      <section className="space-y-4 rounded-xl border border-slate-200 bg-white p-5 shadow-sm">
        <div className="flex flex-col justify-between gap-3 sm:flex-row sm:items-start">
          <div>
            <h2 className="text-xl font-semibold text-slate-900">Forecast lab</h2>
            <p className="mt-1 max-w-2xl text-sm text-slate-600">Python forecasting runs independently of the R backend. Models are compared against a historical-mean baseline on a chronological held-out segment.</p>
          </div>
          <button type="button" onClick={loadForecasts} disabled={forecastLoading || loading}
            className="rounded-md bg-indigo-600 px-4 py-2 text-sm font-medium text-white hover:bg-indigo-700 disabled:opacity-50">
            {forecastLoading ? 'Training models…' : 'Generate forecasts'}
          </button>
        </div>
        <div className="flex flex-wrap items-center gap-3 text-sm">
          <span className="font-medium text-slate-700">Horizons:</span>
          {[1, 5, 10, 20].map((h) => (
            <label key={h} className="flex items-center gap-1.5 text-slate-700">
              <input type="checkbox" checked={horizons.includes(h)} onChange={(event) => setHorizons((old) => event.target.checked ? [...old, h].sort((a, b) => a - b) : old.filter((v) => v !== h))} />
              {h} day{h === 1 ? '' : 's'}
            </label>
          ))}
        </div>
        {forecastError && <div role="alert" className="rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-700">{forecastError}<p className="mt-1 text-xs">Check that the Python service is running on port 8001 and that its requirements are installed.</p></div>}
        {forecastLoading && <p role="status" className="text-sm text-slate-500">Fetching daily history and evaluating models. The first run may take a little while.</p>}
        {forecastData && <>
          <div className="rounded-lg border border-slate-200 bg-slate-50 p-3 text-xs text-slate-600">
            Source: {forecastData.source} · Latest candle: {String(forecastData.as_of).slice(0, 10)} · Close: {money(Number(forecastData.latest_close))}
          </div>
          {forecastData.horizons?.map((group) => <div key={group.horizon_days} className="space-y-2">
            <h3 className="font-semibold text-slate-800">{group.horizon_days}-trading-day horizon</h3>
            {group.error && <p className="text-sm text-amber-700">{group.error}</p>}
            {group.status === 'insufficient_data' && <p className="text-sm text-amber-700">Insufficient data: {group.error}</p>}
            {group.models?.length > 0 && <div className="overflow-x-auto"><table className="w-full min-w-[650px] text-left text-sm">
              <thead><tr className="border-b text-xs text-slate-500"><th className="px-2 py-2">Model</th><th className="px-2 py-2">Forecast return</th><th className="px-2 py-2">Implied price</th><th className="px-2 py-2">Test RMSE</th><th className="px-2 py-2">Test MAE</th><th className="px-2 py-2">Direction accuracy</th></tr></thead>
              <tbody>{group.models.map((model) => <tr key={model.model} className="border-b last:border-0"><td className="px-2 py-2 font-medium">{model.model}</td><td className={`px-2 py-2 tabular-nums ${model.predicted_return > 0 ? 'text-emerald-700' : model.predicted_return < 0 ? 'text-red-700' : ''}`}>{Number.isFinite(model.predicted_return) ? percent(model.predicted_return) : '—'}</td><td className="px-2 py-2 tabular-nums">{money(Number(model.predicted_price))}</td><td className="px-2 py-2 tabular-nums">{Number.isFinite(model.metrics?.rmse) ? model.metrics.rmse.toFixed(5) : '—'}</td><td className="px-2 py-2 tabular-nums">{Number.isFinite(model.metrics?.mae) ? model.metrics.mae.toFixed(5) : '—'}</td><td className="px-2 py-2 tabular-nums">{Number.isFinite(model.metrics?.directional_accuracy) ? `${(model.metrics.directional_accuracy * 100).toFixed(1)}%` : '—'}</td></tr>)}</tbody>
            </table></div>}
          </div>)}
          <p className="text-xs text-slate-500">{forecastData.disclaimer} Forecast prices are model estimates, not investment advice. Historical metrics do not guarantee future performance.</p>
          {forecastData.forecast_history?.length > 0 && <details className="rounded-lg border border-slate-200 p-3"><summary className="cursor-pointer text-sm font-medium text-slate-700">Saved forecast runs ({forecastData.forecast_history.length})</summary><div className="mt-3 max-h-64 overflow-auto"><table className="w-full text-left text-xs"><thead><tr className="border-b text-slate-500"><th className="py-2 pr-3">Created</th><th className="py-2 pr-3">Model</th><th className="py-2 pr-3">Horizon</th><th className="py-2 pr-3">Forecast return</th><th className="py-2 pr-3">Actual return</th><th className="py-2">Outcome</th></tr></thead><tbody>{forecastData.forecast_history.map((row) => <tr key={row.id} className="border-b last:border-0"><td className="py-2 pr-3">{String(row.created_at).slice(0, 16).replace('T', ' ')}</td><td className="py-2 pr-3">{row.model}</td><td className="py-2 pr-3">{row.horizon_days}d</td><td className="py-2 pr-3">{percent(Number(row.predicted_return))}</td><td className="py-2 pr-3">{row.actual_return == null ? 'Pending' : percent(Number(row.actual_return))}</td><td className="py-2">{row.evaluated_at ? `Evaluated ${String(row.evaluated_at).slice(0, 10)}` : `Due ${row.target_date}`}</td></tr>)}</tbody></table></div></details>}
        </>}
      </section>

      <div className="space-y-3">
        <div>
          <h2 className="text-xl font-semibold text-slate-900">Market news and sentiment</h2>
          <p className="mt-1 text-sm text-slate-600">Recent headlines can provide context, but sentiment is not treated as a validated price forecast.</p>
        </div>
        <LiveNewsWidget />
      </div>
    </section>
  )
}
