import { useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { runMlBacktest } from '../services/mlService'
import TickerAutocomplete from '../components/TickerAutocomplete'
import { ML_FORM_DEFAULTS, validateMlForm, buildMlBacktestPayload } from '../utils/mlDefaults'
import { MAX_UNIVERSE_SIZE } from '../utils/ticker'

const PRESET_TICKERS = ['RELIANCE.NS', 'TCS.NS', 'INFY.NS', 'HDFCBANK.NS', 'ICICIBANK.NS']

/**
 * ML Portfolio Backtest page (ML plan sections 11/12/F).
 *
 * Ranks the selected universe by P(BUY) on each rebalance date, holds the
 * Top-N with equal weights, executes every trade through the EXISTING
 * t+1-open engine (via POST /ml/backtest) and renders the resulting equity
 * curve, metrics, per-ticker trades and the rebalance audit trail.
 */
export default function MLBacktestPage() {
  const navigate = useNavigate()
  const [form, setForm] = useState({
    ...ML_FORM_DEFAULTS,
    end: new Date().toISOString().slice(0, 10),
  })
  const [tickers, setTickers] = useState(PRESET_TICKERS)
  const [tickerInput, setTickerInput] = useState('')
  const [errors, setErrors] = useState({})
  const [running, setRunning] = useState(false)
  const [result, setResult] = useState(null)
  const [error, setError] = useState(null)

  function setField(name, value) {
    setForm((prev) => ({ ...prev, [name]: value }))
  }

  function addTicker(raw) {
    const t = String(raw ?? '').trim().toUpperCase()
    if (!t) return
    if (!tickers.includes(t) && tickers.length < MAX_UNIVERSE_SIZE) {
      setTickers([...tickers, t])
    }
    setTickerInput('')
  }

  function removeTicker(t) {
    setTickers(tickers.filter((x) => x !== t))
  }

  async function handleRun() {
    const check = validateMlForm({ ...form, ticker: tickers[0] ?? '' })
    setErrors(check.errors)
    if (tickers.length === 0) {
      setErrors((e) => ({ ...e, tickers: 'Add at least one ticker.' }))
      return
    }
    if (!check.valid && !check.errors.ticker) return
    if (!check.valid) {
      const { ticker: _drop, ...rest } = check.errors
      if (Object.keys(rest).length > 0) return
    }

    setRunning(true)
    setError(null)
    setResult(null)
    try {
      const data = await runMlBacktest(buildMlBacktestPayload(form, tickers))
      setResult(data)
      if (data.success && data.backtestId) {
        // Persisted to the existing backtests tables — also visible in history.
        navigate(`/results/${data.backtestId}`, { state: { mlBacktest: data } })
        return
      }
    } catch (err) {
      setError(err?.response?.data?.error ?? 'Unable to run the ML backtest right now.')
    } finally {
      setRunning(false)
    }
  }

  const metrics = result?.portfolio?.metrics

  return (
    <section className="mx-auto max-w-4xl space-y-6">
      <div>
        <h1 className="text-2xl font-semibold text-slate-900">ML Portfolio Backtest</h1>
        <p className="mt-1 text-sm text-slate-500">
          Ranks the universe by P(BUY), holds the Top-N with equal weights and
          executes through the platform&apos;s t+1-open engine. Trained on the
          chronological train window; traded out-of-sample on the test window.
        </p>
      </div>

      <div className="rounded-lg border border-slate-200 bg-white p-6 shadow-sm">
        <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
          Universe (up to {MAX_UNIVERSE_SIZE} tickers)
        </h2>
        <div className="mt-2 flex flex-wrap gap-2">
          {tickers.map((t) => (
            <span
              key={t}
              className="flex items-center gap-1 rounded-full bg-slate-100 px-3 py-1 text-xs font-medium text-slate-700"
            >
              {t}
              <button
                type="button"
                onClick={() => removeTicker(t)}
                className="text-slate-400 hover:text-red-600"
                aria-label={`Remove ${t}`}
              >
                ×
              </button>
            </span>
          ))}
        </div>
        {errors.tickers && <p className="mt-1 text-xs text-red-600">{errors.tickers}</p>}
        <div className="mt-3 flex gap-2">
          <TickerAutocomplete
            id="mlbt-ticker-input"
            value={tickerInput}
            onChange={setTickerInput}
            onSelectBehavior="submit"
            onSubmit={(symbol) => addTicker(symbol)}
            inputProps={{
              onKeyDown: (e) => {
                // Manual entry still works: Enter adds the typed symbol.
                if (e.key === 'Enter' && !e.defaultPrevented) {
                  e.preventDefault()
                  addTicker(tickerInput)
                }
              },
            }}
            placeholder="Add ticker (e.g. WIPRO.NS or search by company)"
            className="w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
          />
          <button
            type="button"
            onClick={() => addTicker(tickerInput)}
            className="shrink-0 rounded-md bg-slate-700 px-3 py-2 text-sm font-medium text-white hover:bg-slate-600"
          >
            Add
          </button>
        </div>

        <div className="mt-4 grid gap-4 sm:grid-cols-3">
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlbt-model">
              Model
            </label>
            <select
              id="mlbt-model"
              value={form.model}
              onChange={(e) => setField('model', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            >
              {[
                'logistic_regression', 'lda', 'qda', 'knn', 'naive_bayes',
                'decision_tree', 'random_forest', 'svm', 'gradient_boosting',
                'xgboost', 'mlp', 'lstm',
              ].map((m) => (
                <option key={m} value={m}>{m}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlbt-topn">
              Top-N
            </label>
            <input
              id="mlbt-topn"
              type="number"
              min="1"
              max="10"
              value={form.topN}
              onChange={(e) => setField('topN', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlbt-rebalance">
              Rebalancing
            </label>
            <select
              id="mlbt-rebalance"
              value={form.rebalance}
              onChange={(e) => setField('rebalance', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            >
              {['daily', 'weekly', 'monthly'].map((r) => (
                <option key={r} value={r}>
                  {r.charAt(0).toUpperCase() + r.slice(1)}
                </option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlbt-start">
              Start date
            </label>
            <input
              id="mlbt-start"
              type="date"
              value={form.start}
              onChange={(e) => setField('start', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlbt-end">
              End date
            </label>
            <input
              id="mlbt-end"
              type="date"
              value={form.end}
              onChange={(e) => setField('end', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlbt-capital">
              Initial capital ($)
            </label>
            <input
              id="mlbt-capital"
              type="number"
              min="1"
              step="100"
              value={form.capital}
              onChange={(e) => setField('capital', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            />
          </div>
        </div>

        <button
          type="button"
          onClick={handleRun}
          disabled={running}
          className="mt-4 w-full rounded-md bg-emerald-600 px-4 py-2 text-sm font-medium text-white hover:bg-emerald-700 disabled:opacity-60"
          data-testid="mlbt-run-button"
        >
          {running ? 'Training + backtesting the portfolio…' : 'Run ML Backtest'}
        </button>
        {running && (
          <p className="mt-2 text-sm text-slate-500" data-testid="mlbt-progress">
            One model is trained per ticker, then the Top-N portfolio is
            simulated — this can take a few minutes.
          </p>
        )}
        {error && (
          <div className="mt-3 rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-700" data-testid="mlbt-error">
            {error}
          </div>
        )}
      </div>

      {result && (
        <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm" data-testid="mlbt-result">
          <h2 className="font-medium text-slate-800">
            {result.model_name} · Top-{result.config?.top_n} ·{' '}
            {result.config?.rebalance_days ? `rebalance every ${result.config.rebalance_days}d` : ''}
          </h2>
          {metrics && (
            <div className="mt-3 grid grid-cols-2 gap-3 sm:grid-cols-4">
              {[
                ['Total Return', pct(metrics.totalReturn)],
                ['CAGR', pct(metrics.cagr)],
                ['Sharpe', num(metrics.sharpe)],
                ['Sortino', num(metrics.sortino)],
                ['Max Drawdown', pct(metrics.maximumDrawdown)],
                ['Win Rate', pct(metrics.winRate)],
                ['Volatility', pct(metrics.volatility)],
                ['Trades', metrics.totalTrades ?? '—'],
              ].map(([label, value]) => (
                <div key={label} className="rounded-md border border-slate-200 px-3 py-2">
                  <p className="text-xs font-medium uppercase tracking-wide text-slate-500">{label}</p>
                  <p className="mt-1 text-sm font-semibold tabular-nums text-slate-900">{value}</p>
                </div>
              ))}
            </div>
          )}

          {Array.isArray(result.rebalances) && result.rebalances.length > 0 && (
            <div className="mt-4">
              <h3 className="text-sm font-medium text-slate-700">
                Rebalance history (Top-N by P(BUY))
              </h3>
              <ul className="mt-2 space-y-2">
                {result.rebalances.slice(0, 8).map((rb) => (
                  <li key={rb.date} className="rounded-md border border-slate-100 px-3 py-2 text-sm">
                    <span className="font-medium text-slate-700">{rb.date}</span>
                    <span className="ml-2 text-slate-500">
                      {rb.holdings.map((h) => `${h.ticker} (${Number(h.prob_buy).toFixed(2)})`).join(' · ')}
                    </span>
                  </li>
                ))}
              </ul>
            </div>
          )}

          {!result.backtestId && (
            <p className="mt-3 text-xs text-amber-700">
              This run was not persisted (database unavailable) — the results
              above are still valid for this session.
            </p>
          )}
        </div>
      )}
    </section>
  )
}

function pct(value) {
  const n = Number(value)
  if (!Number.isFinite(n)) return '—'
  return `${n > 0 ? '+' : ''}${(n * 100).toFixed(2)}%`
}

function num(value) {
  const n = Number(value)
  return Number.isFinite(n) ? n.toFixed(2) : '—'
}
