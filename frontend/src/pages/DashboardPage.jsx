import { useEffect, useState } from 'react'
import { Link } from 'react-router-dom'
import { getBacktestHistory } from '../services/backtestService'
import { formatPercent, formatDate, formatDateTime } from '../utils/format'
import LiveNewsWidget from '../components/LiveNewsWidget'

/**
 * Dashboard (Phase 6) — welcome overview + saved backtest history.
 *
 * The history list loads from GET /backtest/history (JWT-protected) and shows
 * the user's past runs with ticker, strategy, date range, key metrics and a
 * link to the full results page.
 */

function strategyLabel(strategy) {
  if (strategy === 'MA_Crossover') return 'MA Cross (50/200)'
  if (strategy === 'RSI') return 'RSI (14, 30/70)'
  if (strategy === 'Buy_and_Hold') return 'Buy & Hold'
  return strategy
}

function HistoryRow({ item }) {
  return (
    <tr
      className="border-b border-slate-100 last:border-0 hover:bg-slate-50"
      data-testid="history-item"
    >
      <td className="py-3 pr-4">
        <span className="font-semibold text-slate-800">{item.ticker}</span>
      </td>
      <td className="py-3 pr-4">
        <span className="rounded bg-slate-100 px-2 py-0.5 text-xs font-medium text-slate-600">
          {strategyLabel(item.strategy)}
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
        <Link
          to={`/results/${item.id}`}
          className="shrink-0 rounded-md border border-slate-200 px-3 py-1.5 text-sm font-medium text-emerald-600 hover:bg-emerald-50"
        >
          View →
        </Link>
      </td>
    </tr>
  )
}

export default function DashboardPage() {
  const [history, setHistory] = useState([])
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState(null)

  useEffect(() => {
    let cancelled = false
    getBacktestHistory(10, 0)
      .then((data) => {
        if (cancelled) return
        setHistory(data.items ?? [])
      })
      .catch((err) => {
        if (cancelled) return
        setError(err?.response?.data?.error ?? 'Unable to load backtest history.')
      })
      .finally(() => {
        if (!cancelled) setLoading(false)
      })
    return () => {
      cancelled = true
    }
  }, [])

  const features = [
    {
      title: 'Strategy Builder',
      text: 'Configure Moving Average Crossover or RSI strategies without code.',
      to: '/strategy-builder',
      cta: 'Open builder',
    },
    {
      title: 'Backtesting',
      text: 'Run full backtests with the t+1-open execution engine and inspect the equity curve.',
      to: '/strategy-builder',
      cta: 'Run a backtest',
    },
    {
      title: 'Analytics',
      text: 'Sharpe, Sortino, Maximum Drawdown, Win Rate and more on every run.',
      to: '/results',
      cta: 'View results',
    },
  ]

  return (
    <section className="space-y-6">
      <div>
        <h1 className="text-2xl font-semibold text-slate-900">
          Welcome to AlphaForge Insight
        </h1>
        <p className="mt-1 text-slate-600">
          Design, test, analyse and optimise stock trading strategies with
          historical market data.
        </p>
      </div>

      <div className="rounded-lg border border-slate-200 bg-white p-6 shadow-sm">
        <div className="flex items-center justify-between">
          <div>
            <h2 className="font-medium text-slate-800">Backtest history</h2>
            <p className="mt-0.5 text-sm text-slate-500">
              Your saved runs, with the key metrics and a link to the full
              results dashboard.
            </p>
          </div>
          <Link
            to="/strategy-builder"
            className="text-sm font-medium text-emerald-600 hover:underline"
          >
            + New backtest
          </Link>
        </div>

        {loading ? (
          <p className="mt-3 text-sm text-slate-500" data-testid="history-loading">
            Loading your backtests…
          </p>
        ) : error ? (
          <p className="mt-3 text-sm text-red-600" data-testid="history-error">
            {error}
          </p>
        ) : history.length > 0 ? (
          <div className="mt-3 overflow-x-auto" data-testid="history-list">
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
                {history.map((item) => (
                  <HistoryRow key={item.id} item={item} />
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <p className="mt-3 text-sm text-slate-500" data-testid="history-empty">
            No saved backtests yet. Press “Run Backtest” in the Strategy Builder
            to create your first one.
          </p>
        )}
      </div>

      <div className="grid gap-4 sm:grid-cols-3">
        {features.map((feature) => (
          <div
            key={feature.title}
            className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm"
          >
            <h3 className="font-medium text-slate-800">{feature.title}</h3>
            <p className="mt-1 text-sm text-slate-500">{feature.text}</p>
            <Link
              to={feature.to}
              className="mt-3 inline-block text-sm font-medium text-emerald-600 hover:underline"
            >
              {feature.cta} →
            </Link>
          </div>
        ))}
      </div>

      {/* Live News Section (Phase H.4) */}
      <LiveNewsWidget />
    </section>
  )
}