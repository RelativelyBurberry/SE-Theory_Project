import { useEffect, useState } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  Tooltip,
  CartesianGrid,
  ResponsiveContainer,
} from 'recharts'
import { getBacktest, getBacktestHistory } from '../services/backtestService'
import EquityCurveComparison from '../components/EquityCurveComparison'
import {
  formatNumber,
  formatPercent,
  formatCurrency,
  formatDate,
  formatDateTime,
} from '../utils/format'

/** Friendly strategy label for headers/chips (display only). */
function strategyLabel(strategy, parameters) {
  if (strategy === 'MA_Crossover') {
    const fast = parameters?.fast_period
    const slow = parameters?.slow_period
    return fast && slow ? `MA Cross (${fast}/${slow})` : 'MA Cross'
  }
  if (strategy === 'RSI') {
    const period = parameters?.period
    return period ? `RSI (${period}, 30/70)` : 'RSI'
  }
  if (strategy === 'Buy_and_Hold') return 'Buy & Hold'
  return strategy
}

/**
 * Backtest Results (Phase 6) — full results dashboard.
 *
 * Behaviors:
 *  - WITH a :backtestId (e.g. /results/43): loads that record from
 *    GET /backtest/:id and renders the full dashboard (strategy header,
 *    eight performance-metric cards, equity curve, equity-curve comparison,
 *    trade history). This is the hard-refresh-safe path: only the URL id and
 *    the localStorage JWT are needed — no in-memory page state is required.
 *  - WITHOUT an id (e.g. the Results navbar item): loads the user's persisted
 *    backtests from GET /backtest/history and shows them as a selectable list;
 *    selecting one navigates to /results/:id. "No results yet" is rendered
 *    ONLY when the history endpoint genuinely returns an empty list — an API
 *    failure renders an error instead of an empty state.
 *
 * Data always comes from the real JWT-protected endpoints. No fake data.
 */

function LoadingState() {
  return (
    <div className="space-y-4" data-testid="results-loading">
      <div className="h-6 w-48 animate-pulse rounded bg-slate-200" />
      <div className="h-32 animate-pulse rounded-lg bg-slate-100" />
      <div className="grid gap-4 sm:grid-cols-4">
        {Array.from({ length: 8 }).map((_, i) => (
          <div key={i} className="h-24 animate-pulse rounded-lg bg-slate-100" />
        ))}
      </div>
    </div>
  )
}

function ErrorState({ message, testId = 'results-error' }) {
  return (
    <div
      className="rounded-lg border border-red-200 bg-red-50 p-4 text-sm text-red-700"
      data-testid={testId}
    >
      {message}
    </div>
  )
}

function MetricCard({ label, value, hint, accent = 'text-slate-900' }) {
  return (
    <div className="min-h-[5.5rem] rounded-lg border border-slate-200 bg-white p-4 shadow-sm">
      <p className="text-xs font-medium uppercase tracking-wide text-slate-500">
        {label}
      </p>
      <p className={`mt-1 text-lg font-semibold tabular-nums ${accent}`}>{value}</p>
      {hint && <p className="mt-0.5 text-xs text-slate-400">{hint}</p>}
    </div>
  )
}

function paramDescription(strategy, parameters) {
  if (!parameters) return ''
  if (strategy === 'MA_Crossover') {
    return `${parameters.fast_period} / ${parameters.slow_period} SMA`
  }
  if (strategy === 'RSI') {
    return `RSI ${parameters.period} · oversold ${parameters.oversold} · overbought ${parameters.overbought}`
  }
  return ''
}

/**
 * Saved-backtest picker used when the Results page is opened WITHOUT a
 * :backtestId (e.g. from the Results navbar item). Loads GET /backtest/history
 * (JWT-protected, same contract as the Dashboard) and lets the user open a
 * persisted run in the full dashboard at /results/:id.
 */
function HistoryList({ items, onSelect }) {
  return (
    <div className="rounded-lg border border-slate-200 bg-white p-6 shadow-sm">
      <h2 className="font-medium text-slate-800">Saved backtests</h2>
      <p className="mt-1 text-sm text-slate-500">
        Persisted runs from your account (the database is the source of truth).
        Select one to open the full results dashboard.
      </p>
      <div className="mt-3 overflow-x-auto" data-testid="results-history-list">
        <table className="w-full text-left text-sm">
          <thead>
            <tr className="border-b border-slate-200 text-xs uppercase tracking-wide text-slate-500">
              <th className="py-2 pr-4 font-medium">Ticker</th>
              <th className="py-2 pr-4 font-medium">Strategy</th>
              <th className="py-2 pr-4 font-medium">Period</th>
              <th className="py-2 pr-4 font-medium">Created</th>
              <th className="py-2 pr-4 text-right font-medium">Result</th>
              <th className="py-2 pl-4 text-right font-medium">Action</th>
            </tr>
          </thead>
          <tbody>
            {items.map((item) => (
              <tr
                key={item.id}
                data-testid="results-history-item"
                onClick={() => onSelect(item.id)}
                className="cursor-pointer border-b border-slate-100 last:border-0 hover:bg-slate-50"
              >
                <td className="py-3 pr-4 font-semibold text-slate-800">
                  {item.ticker}
                </td>
                <td className="py-3 pr-4">
                  <span className="rounded bg-slate-100 px-2 py-0.5 text-xs font-medium text-slate-600">
                    {strategyLabel(item.strategy, null)}
                  </span>
                </td>
                <td className="py-3 pr-4 text-sm tabular-nums text-slate-600">
                  {formatDate(item.startDate)} → {formatDate(item.endDate)}
                </td>
                <td className="py-3 pr-4 text-sm text-slate-500">
                  {formatDateTime(item.createdAt)}
                </td>
                <td className="py-3 pr-4 text-right">
                  <p className="text-sm font-semibold tabular-nums text-slate-800">
                    {formatPercent(item.totalReturn)}
                  </p>
                  <p className="text-xs text-slate-500">
                    Sharpe{' '}
                    {item.sharpeRatio != null
                      ? Number(item.sharpeRatio).toFixed(2)
                      : '—'}
                    {' · '}
                    {item.totalTrades != null ? `${item.totalTrades} trades` : '—'}
                  </p>
                </td>
                <td className="py-3 pl-4 text-right">
                  <span className="shrink-0 rounded-md border border-slate-200 px-3 py-1.5 text-sm font-medium text-emerald-600">
                    Open render →
                  </span>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  )
}

export default function BacktestResultsPage() {
  const { backtestId } = useParams()
  const navigate = useNavigate()
  const [loading, setLoading] = useState(Boolean(backtestId))
  const [error, setError] = useState(null)
  const [result, setResult] = useState(null)
  // History mode (no :backtestId): the user's persisted backtests.
  const [history, setHistory] = useState(null)
  const [historyLoading, setHistoryLoading] = useState(!backtestId)
  const [historyError, setHistoryError] = useState(null)

  // History fetch for /results (no backtest id). Kept completely separate from
  // the single-record fetch below: an empty database is a VALID state ("No
  // results yet"), while a failed request must render an error instead.
  useEffect(() => {
    if (backtestId) return undefined
    let cancelled = false
    setHistoryLoading(true)
    setHistoryError(null)
    getBacktestHistory(20, 0)
      .then((data) => {
        if (cancelled) return
        setHistory(data.items ?? [])
      })
      .catch((err) => {
        if (cancelled) return
        if (err?.response?.status === 401) {
          setHistoryError('Your session has expired. Please sign in again.')
        } else if (!err?.response) {
          setHistoryError(
            'Cannot reach the server. Check your connection and try again.',
          )
        } else {
          setHistoryError(
            err?.response?.data?.error ?? 'Unable to load backtest history.',
          )
        }
      })
      .finally(() => {
        if (!cancelled) setHistoryLoading(false)
      })
    return () => {
      cancelled = true
    }
  }, [backtestId])

  useEffect(() => {
    if (!backtestId) {
      setLoading(false)
      setError(null)
      return
    }
    let cancelled = false
    setLoading(true)
    setError(null)
    getBacktest(backtestId)
      .then((data) => {
        if (cancelled) return
        setResult(data)
      })
      .catch((err) => {
        if (cancelled) return
        // Classify the failure instead of a single generic message:
        //  - 401: the session token is missing/expired. The axios interceptor
        //    clears the stored token and redirects to /login; the message below
        //    only shows if that redirect is suppressed.
        //  - request without response (CORS/network failure): the backend is
        //    unreachable — do not claim the backtest itself is broken.
        //  - 404: the record does not exist (deleted / belongs to another user).
        //  - 5xx: the server itself failed, not the user's request.
        //  - anything else: surface the backend's error text when provided.
        if (err?.response?.status === 401) {
          setError('Your session has expired. Please sign in again.')
        } else if (!err?.response) {
          // No HTTP response at all: network/CORS failure — the backend was
          // never reached, so do not blame the backtest or the session.
          setError('Cannot reach the server. Check your connection and try again.')
        } else if (err?.response?.status === 404) {
          setError(
            err?.response?.data?.error ??
              'This backtest does not exist or has been deleted.',
          )
        } else if (err?.response?.status >= 500) {
          // The server itself failed; the user's request/backtest is fine.
          setError('The server hit an error while loading this backtest. Please try again later.')
        } else {
          setError(
            err?.response?.data?.error ?? 'Unable to load this backtest right now.',
          )
        }
      })
      .finally(() => {
        if (!cancelled) setLoading(false)
      })
    return () => {
      cancelled = true
    }
  }, [backtestId])

  if (loading) return <LoadingState />

  if (error) {
    return (
      <section className="space-y-6">
        <div>
          <h1 className="text-2xl font-semibold text-slate-900">Backtest Results</h1>
        </div>
        <ErrorState message={error} />
      </section>
    )
  }

  if (!backtestId) {
    return (
      <section className="space-y-6">
        <div>
          <h1 className="text-2xl font-semibold text-slate-900">Backtest Results</h1>
          <p className="mt-1 text-sm text-slate-500">
            Select a persisted backtest to open its metrics, equity curve and
            trade history here.
          </p>
        </div>

        {historyLoading ? (
          <p
            className="text-sm text-slate-500"
            data-testid="results-history-loading"
          >
            Loading your saved backtests…
          </p>
        ) : historyError ? (
          <ErrorState message={historyError} testId="results-history-error" />
        ) : history && history.length > 0 ? (
          <HistoryList
            items={history}
            onSelect={(id) => navigate(`/results/${id}`)}
          />
        ) : (
          <div className="rounded-lg border border-dashed border-slate-300 bg-white p-8 text-center" data-testid="results-empty">
            <h2 className="text-lg font-medium text-slate-700">No results yet</h2>
            <p className="mx-auto mt-2 max-w-md text-sm text-slate-500">
              Go to the Strategy Builder, configure a strategy and press “Run
              Backtest”. Your results dashboard will appear here.
            </p>
          </div>
        )}
      </section>
    )
  }

  // Guard against the param-change render race: when the route param changes
  // (e.g. /results -> /results/43 remounts the same element), React commits a
  // render where `loading` is still false and `result` is still null — the
  // effect that sets loading=true runs only after this commit. Destructuring
  // null here crashed the whole page (no error boundary in this tree).
  if (!result) return <LoadingState />

  const { metrics, equityCurve, tradeHistory, ticker, strategy, parameters } =
    result

  const equityData = equityCurve.map((row) => ({
    ...row,
    date: row.date,
    portfolioValue: Number(row.portfolioValue),
  }))

  return (
    <section className="space-y-6">
      <div>
        <h1 className="text-2xl font-semibold text-slate-900">Backtest Results</h1>
        <div
          className="mt-2 flex flex-wrap items-center gap-2 text-sm"
          data-testid="results-title"
        >
          <span className="rounded-md bg-slate-900 px-2.5 py-1 font-medium text-white">
            {ticker}
          </span>
          <span className="rounded-md bg-emerald-50 px-2.5 py-1 font-medium text-emerald-700 ring-1 ring-emerald-200">
            {strategyLabel(strategy, parameters)}
          </span>
          {paramDescription(strategy, parameters) && (
            <span className="rounded-md bg-slate-100 px-2.5 py-1 font-medium text-slate-600">
              {paramDescription(strategy, parameters)}
            </span>
          )}
          <span className="rounded-md bg-slate-100 px-2.5 py-1 font-medium tabular-nums text-slate-600">
            #{result.id} · {result.startDate} → {result.endDate}
          </span>
        </div>
      </div>

      {/* Metrics cards */}
      <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
        <MetricCard
          label="Total Return"
          value={formatPercent(metrics?.totalReturn)}
          accent={
            (metrics?.totalReturn ?? 0) >= 0 ? 'text-emerald-600' : 'text-red-600'
          }
        />
        <MetricCard
          label="CAGR"
          value={formatPercent(metrics?.cagr)}
          accent={(metrics?.cagr ?? 0) >= 0 ? 'text-emerald-600' : 'text-red-600'}
        />
        <MetricCard
          label="Sharpe Ratio"
          value={formatNumber(metrics?.sharpe, 2)}
        />
        <MetricCard
          label="Sortino Ratio"
          value={formatNumber(metrics?.sortino, 2)}
        />
        <MetricCard
          label="Max Drawdown"
          value={formatPercent(metrics?.maximumDrawdown)}
          accent="text-red-600"
        />
        <MetricCard
          label="Win Rate"
          value={formatPercent(metrics?.winRate)}
        />
        <MetricCard
          label="Volatility"
          value={formatPercent(metrics?.volatility)}
        />
        <MetricCard
          label="Total Trades"
          value={metrics?.totalTrades != null ? String(metrics.totalTrades) : '—'}
        />
      </div>

      {/* Financial summary strip */}
      <div className="grid gap-4 sm:grid-cols-3">
        <div className="rounded-lg border border-slate-200 bg-white p-4 shadow-sm">
          <p className="text-xs font-medium uppercase tracking-wide text-slate-500">
            Initial Capital
          </p>
          <p className="mt-1 text-lg font-semibold tabular-nums text-slate-900">
            {formatCurrency(metrics?.initialCapital)}
          </p>
        </div>
        <div className="rounded-lg border border-slate-200 bg-white p-4 shadow-sm">
          <p className="text-xs font-medium uppercase tracking-wide text-slate-500">
            Final Value
          </p>
          <p className="mt-1 text-lg font-semibold tabular-nums text-slate-900">
            {formatCurrency(metrics?.finalValue)}
          </p>
        </div>
        <div className="rounded-lg border border-slate-200 bg-white p-4 shadow-sm">
          <p className="text-xs font-medium uppercase tracking-wide text-slate-500">
            Period
          </p>
          <p className="mt-1 text-lg font-semibold tabular-nums text-slate-900">
            {formatDate(result.startDate)} — {formatDate(result.endDate)}
          </p>
        </div>
      </div>

      {/* Equity curve */}
      <div className="rounded-lg border border-slate-200 bg-white p-6 shadow-sm">
        <h2 className="font-medium text-slate-800">Equity Curve</h2>
        <p className="mt-1 text-sm text-slate-500">
          Close-to-close marking of the portfolio value. Signals execute at the
          t+1 open.
        </p>
        <div className="mt-4 h-80" data-testid="equity-chart">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={equityData} margin={{ top: 8, right: 16, bottom: 8, left: 8 }}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                dataKey="date"
                tick={{ fontSize: 12, fill: '#475569' }}
                tickFormatter={(d) => d.slice(0, 7)}
                minTickGap={32}
                interval="preserveStartEnd"
                axisLine={{ stroke: '#cbd5e1' }}
                tickLine={false}
              />
              <YAxis
                domain={['auto', 'auto']}
                width={80}
                tick={{ fontSize: 12, fill: '#475569' }}
                tickFormatter={(v) =>
                  `$${Number(v).toLocaleString('en-US', {
                    maximumFractionDigits: 0,
                  })}`
                }
                axisLine={false}
                tickLine={false}
              />
              <Tooltip
                formatter={(value, name) => [
                  `$${Number(value).toLocaleString(undefined, {
                    minimumFractionDigits: 2,
                    maximumFractionDigits: 2,
                  })}`,
                  name === 'portfolioValue' ? 'Portfolio Value' : name,
                ]}
                labelFormatter={(label) => label}
              />
              <Line
                type="monotone"
                dataKey="portfolioValue"
                stroke="#059669"
                strokeWidth={2}
                dot={false}
                isAnimationActive={false}
                name="Portfolio Value"
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </div>

      {/* Equity curve comparison (Figure 4) — 3 strategies for the same asset */}
      <EquityCurveComparison backtest={result} />

      {/* Trade history */}
      <div className="rounded-lg border border-slate-200 bg-white p-6 shadow-sm">
        <h2 className="font-medium text-slate-800">Trade History</h2>
        {tradeHistory && tradeHistory.length > 0 ? (
          <div className="mt-3 overflow-x-auto">
            <table className="w-full text-left text-sm">
              <thead>
                <tr className="border-b border-slate-200 text-xs uppercase tracking-wide text-slate-500">
                  <th className="py-2 pr-3 font-medium">Side</th>
                  <th className="py-2 pr-3 font-medium">Signal Date</th>
                  <th className="py-2 pr-3 font-medium">Execution Date</th>
                  <th className="py-2 pr-3 font-medium">Price</th>
                  <th className="py-2 pr-3 font-medium">Shares</th>
                  <th className="py-2 pr-3 font-medium">Cost</th>
                  <th className="py-2 pr-3 font-medium">Cash After</th>
                </tr>
              </thead>
              <tbody>
                {tradeHistory.map((trade) => (
                  <tr key={trade.tradeId} className="border-b border-slate-100">
                    <td className="py-2 pr-3">
                      <span
                        className={`rounded px-2 py-0.5 text-xs font-semibold ${
                          trade.side === 'BUY'
                            ? 'bg-emerald-100 text-emerald-700'
                            : 'bg-red-100 text-red-700'
                        }`}
                      >
                        {trade.side}
                      </span>
                    </td>
                    <td className="py-2 pr-3 text-slate-700">
                      {trade.signalDate}
                    </td>
                    <td className="py-2 pr-3 text-slate-700">
                      {trade.executionDate}
                    </td>
                    <td className="py-2 pr-3 tabular-nums">
                      {formatCurrency(trade.executionPrice)}
                    </td>
                    <td className="py-2 pr-3 tabular-nums">{formatNumber(trade.shares, 0)}</td>
                    <td className="py-2 pr-3 tabular-nums">{formatCurrency(trade.cost)}</td>
                    <td className="py-2 pr-3 tabular-nums">
                      {formatCurrency(trade.cashAfter)}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <p className="mt-2 text-sm text-slate-500">
            No trades were executed for this configuration.
          </p>
        )}
      </div>
    </section>
  )
}