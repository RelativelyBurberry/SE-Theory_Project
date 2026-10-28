import { useMemo, useState } from 'react'
import { useLocation, useNavigate } from 'react-router-dom'
import {
  LineChart, Line, XAxis, YAxis, Tooltip, Legend, CartesianGrid,
  ResponsiveContainer, BarChart, Bar, ReferenceLine,
} from 'recharts'
import { compareMlModels, ML_MODEL_OPTIONS } from '../services/mlService'
import TickerAutocomplete from '../components/TickerAutocomplete'
import {
  ML_FORM_DEFAULTS, validateMlForm, buildMlComparePayload,
} from '../utils/mlDefaults'
import {
  saveMlCompareSnapshot,
  loadMlCompareSnapshot,
  clearMlCompareSnapshot,
} from '../utils/mlComparePersistence'

const ALL_MODELS = ML_MODEL_OPTIONS.map((m) => m.value)

/** Flatten the backend's per-class curve arrays into chart rows. */
function curveToRows(curve, xKey, yKey) {
  const xs = Object.values(curve?.[xKey] ?? {})
  const ys = Object.values(curve?.[yKey] ?? {})
  const rows = []
  for (let i = 0; i < Math.min(xs.length, ys.length); i++) {
    const x = Number(xs[i])
    const y = Number(ys[i])
    if (Number.isFinite(x) && Number.isFinite(y)) {
      rows.push({ x, y })
    }
  }
  rows.sort((a, b) => a.x - b.x)
  return rows
}

const SERIES_COLORS = { BUY: '#059669', HOLD: '#64748b', SELL: '#dc2626' }

export default function MLComparisonPage() {
  const location = useLocation()
  const navigate = useNavigate()
  // A completed comparison persists to sessionStorage (see
  // utils/mlComparePersistence.js). On every mount — whether the user simply
  // navigated back to /ml-compare or hard-refreshed the page — the last
  // completed comparison is restored here so the table/charts/ROC curves do
  // not disappear. The form prefills from an intentionally navigated
  // location.state first (e.g. "Run comparison" from the Strategy Builder),
  // otherwise from the stored snapshot, otherwise defaults.
  const snapshot = useMemo(() => loadMlCompareSnapshot(), [])
  const initial =
    location.state?.mlForm ?? snapshot?.form ?? { ...ML_FORM_DEFAULTS }

  const [form, setForm] = useState({
    ...initial,
    end: initial.end || new Date().toISOString().slice(0, 10),
  })
  const [errors, setErrors] = useState({})
  const [running, setRunning] = useState(false)
  const [result, setResult] = useState(snapshot?.result ?? null)
  const [error, setError] = useState(null)

  function setField(name, value) {
    setForm((prev) => ({ ...prev, [name]: value }))
  }

  async function handleCompare() {
    const check = validateMlForm(form)
    setErrors(check.errors)
    if (!check.valid) return
    setRunning(true)
    setError(null)
    setResult(null)
    // A new run invalidates the previously completed comparison (the page
    // stays blank while training); only a successful completion is saved.
    clearMlCompareSnapshot()
    try {
      const data = await compareMlModels(buildMlComparePayload(form, ALL_MODELS))
      setResult(data)
      saveMlCompareSnapshot({ result: data, form })
    } catch (err) {
      setError(err?.response?.data?.error ?? 'Unable to run the model comparison right now.')
    } finally {
      setRunning(false)
    }
  }

  const okRows = useMemo(
    () => (result?.comparison ?? []).filter((r) => r.Status === 'ok'),
    [result],
  )
  const failedRows = useMemo(
    () => (result?.comparison ?? []).filter((r) => r.Status === 'failed'),
    [result],
  )

  // Aggregate ROC / PR curves across models is not meaningful; the charts show
  // the best-F1 model's curves, exactly as evaluated (no fabrication).
  // per_model is a keyed object { <model_key>: payload } from POST /ml/compare
  // (null entries = failed models); fall back to an array shape for safety.
  const bestModel = result?.best_by_f1
  const bestTrain = useMemo(() => {
    if (!bestModel) return null
    const pm = result?.per_model
    const entries = Array.isArray(pm)
      ? pm
      : Object.values(pm ?? {})
    return (
      entries.find(
        (p) => p && (p.model_name === bestModel || p.model === bestModel),
      ) ?? null
    )
  }, [result, bestModel])

  const rocRows = useMemo(() => {
    const curves = bestTrain?.validation_metrics?.curves
    if (!Array.isArray(curves)) return []
    return curves.map((c) => ({
      cls: c.class,
      auc: c.roc?.auc,
      rows: curveToRows(c.roc, 'fpr', 'tpr'),
    }))
  }, [bestTrain])

  const prRows = useMemo(() => {
    const curves = bestTrain?.validation_metrics?.curves
    if (!Array.isArray(curves)) return []
    return curves.map((c) => ({
      cls: c.class,
      auc: c.pr?.auc,
      rows: curveToRows(c.pr, 'recall', 'precision'),
    }))
  }, [bestTrain])

  const comparisonChartData = useMemo(
    () => okRows.map((r) => ({
      name: r.Model,
      Accuracy: round2(Number(r.Accuracy) * 100),
      F1: round2(Number(r.F1) * 100),
    })),
    [okRows],
  )

  return (
    <section className="mx-auto max-w-4xl space-y-6">
      <div>
        <h1 className="text-2xl font-semibold text-slate-900">ML Model Comparison</h1>
        <p className="mt-1 text-sm text-slate-500">
          Trains all 12 ML/DL models on the same chronological split and
          compares classification metrics (test window) with trading metrics
          from the existing t+1-open backtesting engine. One model failing never
          blocks the others.
        </p>
      </div>

      <div className="rounded-lg border border-slate-200 bg-white p-6 shadow-sm">
        <div className="grid gap-4 sm:grid-cols-2">
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlcmp-ticker">
              Ticker
            </label>
            <div className="mt-1">
              <TickerAutocomplete
                id="mlcmp-ticker"
                value={form.ticker}
                onChange={(v) => setField('ticker', v)}
                placeholder="e.g. AAPL or RELIANCE.NS"
                className="w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
              />
            </div>
            {errors.ticker && <p className="mt-1 text-xs text-red-600">{errors.ticker}</p>}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlcmp-horizon">
              Horizon (days)
            </label>
            <input
              id="mlcmp-horizon"
              type="number"
              min="1"
              max="60"
              value={form.horizon}
              onChange={(e) => setField('horizon', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            />
            {errors.horizon && <p className="mt-1 text-xs text-red-600">{errors.horizon}</p>}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlcmp-start">
              Start date
            </label>
            <input
              id="mlcmp-start"
              type="date"
              value={form.start}
              onChange={(e) => setField('start', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            />
            {errors.start && <p className="mt-1 text-xs text-red-600">{errors.start}</p>}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="mlcmp-end">
              End date
            </label>
            <input
              id="mlcmp-end"
              type="date"
              value={form.end}
              onChange={(e) => setField('end', e.target.value)}
              className="mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none"
            />
            {errors.end && <p className="mt-1 text-xs text-red-600">{errors.end}</p>}
          </div>
        </div>
        <button
          type="button"
          onClick={handleCompare}
          disabled={running}
          className="mt-4 w-full rounded-md bg-emerald-600 px-4 py-2 text-sm font-medium text-white hover:bg-emerald-700 disabled:opacity-60"
          data-testid="mlcmp-run-button"
        >
          {running ? 'Training all 12 models (this can take several minutes)…' : 'Compare All 12 Models'}
        </button>
        {errors.featureGroups && <p className="mt-1 text-xs text-red-600">{errors.featureGroups}</p>}
      </div>

      {running && (
        <p className="text-sm text-slate-500" data-testid="mlcmp-progress">
          Training 12 models on {form.ticker} ({form.start} → {form.end})…
        </p>
      )}
      {error && (
        <div className="rounded-lg border border-red-200 bg-red-50 p-4 text-sm text-red-700" data-testid="mlcmp-error">
          {error}
        </div>
      )}

      {result && (
        <>
          <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm" data-testid="mlcmp-table">
            <h2 className="font-medium text-slate-800">
              Comparison · {result.ticker} · {result.models_run} model(s)
              {result.models_failed > 0 && ` · ${result.models_failed} failed`}
              {result.best_by_f1 ? ` · best F1: ${result.best_by_f1}` : ''}
            </h2>
            <div className="mt-3 overflow-x-auto">
              <table className="w-full text-left text-sm">
                <thead>
                  <tr className="border-b border-slate-200 text-slate-500">
                    <th className="py-2 pr-3 font-medium">Model</th>
                    <th className="py-2 pr-3 font-medium">Accuracy</th>
                    <th className="py-2 pr-3 font-medium">Precision</th>
                    <th className="py-2 pr-3 font-medium">Recall</th>
                    <th className="py-2 pr-3 font-medium">F1</th>
                    <th className="py-2 pr-3 font-medium">ROC-AUC</th>
                    <th className="py-2 pr-3 font-medium">PR-AUC</th>
                    <th className="py-2 pr-3 font-medium">CAGR</th>
                    <th className="py-2 pr-3 font-medium">Sharpe</th>
                    <th className="py-2 pr-3 font-medium">Sortino</th>
                    <th className="py-2 pr-3 font-medium">Max DD</th>
                    <th className="py-2 pr-3 font-medium">Win Rate</th>
                    <th className="py-2 font-medium">Trades</th>
                  </tr>
                </thead>
                <tbody>
                  {(result.comparison ?? []).map((row) => (
                    <tr key={row.Model} className="border-b border-slate-100">
                      <td className="py-2 pr-3 font-medium text-slate-700">{row.Model}</td>
                      {row.Status === 'failed' ? (
                        <td colSpan={12} className="py-2 text-xs text-red-600">
                          failed: {row.Error}
                        </td>
                      ) : (
                        <>
                          <td className="py-2 pr-3 tabular-nums">{pct1(row.Accuracy)}</td>
                          <td className="py-2 pr-3 tabular-nums">{pct1(row.Precision)}</td>
                          <td className="py-2 pr-3 tabular-nums">{pct1(row.Recall)}</td>
                          <td className="py-2 pr-3 tabular-nums">{pct1(row.F1)}</td>
                          <td className="py-2 pr-3 tabular-nums">{num2(row.ROC_AUC)}</td>
                          <td className="py-2 pr-3 tabular-nums">{num2(row.PR_AUC)}</td>
                          <td className="py-2 pr-3 tabular-nums">{pct1(row.CAGR)}</td>
                          <td className="py-2 pr-3 tabular-nums">{num2(row.Sharpe)}</td>
                          <td className="py-2 pr-3 tabular-nums">{num2(row.Sortino)}</td>
                          <td className="py-2 pr-3 tabular-nums">{pct1(row.MaxDrawdown)}</td>
                          <td className="py-2 pr-3 tabular-nums">{pct1(row.WinRate)}</td>
                          <td className="py-2 tabular-nums">{row.TotalTrades ?? '—'}</td>
                        </>
                      )}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            <p className="mt-3 text-xs text-slate-400">
              Classification metrics are computed on the held-out test window;
              trading metrics come from running each model&apos;s signals
              through the existing backtesting engine (t+1 open). High accuracy
              alone does not imply a profitable strategy.
            </p>
          </div>

          {comparisonChartData.length > 0 && (
            <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm" data-testid="mlcmp-chart">
              <h2 className="font-medium text-slate-800">Accuracy vs F1 by model (%)</h2>
              <div className="mt-3 h-[340px]">
                <ResponsiveContainer width="100%" height="100%">
                  <BarChart data={comparisonChartData} margin={{ top: 8, right: 16, bottom: 40, left: 8 }}>
                    <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                    <XAxis
                      dataKey="name"
                      tick={{ fontSize: 10, fill: '#475569' }}
                      angle={-35}
                      textAnchor="end"
                      interval={0}
                    />
                    <YAxis domain={[0, 100]} tick={{ fontSize: 12, fill: '#475569' }} />
                    <Tooltip />
                    <Legend wrapperStyle={{ fontSize: 12 }} />
                    <Bar dataKey="Accuracy" fill="#059669" isAnimationActive={false} />
                    <Bar dataKey="F1" fill="#2563eb" isAnimationActive={false} />
                  </BarChart>
                </ResponsiveContainer>
              </div>
            </div>
          )}

          {rocRows.length > 0 && (
            <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm" data-testid="mlcmp-roc">
              <h2 className="font-medium text-slate-800">
                ROC curves — {bestModel} (per class)
              </h2>
              <div className="mt-3 grid gap-4 md:grid-cols-3">
                {rocRows.map((c) => (
                  <div key={c.cls}>
                    <p className="text-sm font-medium text-slate-700">
                      {c.cls} · AUC {c.auc != null ? Number(c.auc).toFixed(3) : '—'}
                    </p>
                    <div className="mt-1 h-52">
                      <ResponsiveContainer width="100%" height="100%">
                        <LineChart data={c.rows} margin={{ top: 4, right: 8, bottom: 4, left: 0 }}>
                          <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                          <XAxis dataKey="x" domain={[0, 1]} tick={{ fontSize: 10 }} />
                          <YAxis dataKey="y" domain={[0, 1]} tick={{ fontSize: 10 }} width={32} />
                          <Tooltip />
                          <ReferenceLine segment={[{ x: 0, y: 0 }, { x: 1, y: 1 }]} stroke="#cbd5e1" strokeDasharray="4 4" />
                          <Line type="monotone" dataKey="y" stroke={SERIES_COLORS[c.cls] ?? '#334155'} dot={false} isAnimationActive={false} />
                        </LineChart>
                      </ResponsiveContainer>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          )}

          {prRows.length > 0 && (
            <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm" data-testid="mlcmp-pr">
              <h2 className="font-medium text-slate-800">
                Precision-Recall curves — {bestModel} (per class)
              </h2>
              <div className="mt-3 grid gap-4 md:grid-cols-3">
                {prRows.map((c) => (
                  <div key={c.cls}>
                    <p className="text-sm font-medium text-slate-700">
                      {c.cls} · AUC {c.auc != null ? Number(c.auc).toFixed(3) : '—'}
                    </p>
                    <div className="mt-1 h-52">
                      <ResponsiveContainer width="100%" height="100%">
                        <LineChart data={c.rows} margin={{ top: 4, right: 8, bottom: 4, left: 0 }}>
                          <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                          <XAxis dataKey="x" domain={[0, 1]} tick={{ fontSize: 10 }} />
                          <YAxis dataKey="y" domain={[0, 1]} tick={{ fontSize: 10 }} width={32} />
                          <Tooltip />
                          <Line type="monotone" dataKey="y" stroke={SERIES_COLORS[c.cls] ?? '#334155'} dot={false} isAnimationActive={false} />
                        </LineChart>
                      </ResponsiveContainer>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          )}

          {failedRows.length > 0 && (
            <div className="rounded-lg border border-amber-200 bg-amber-50 p-4 text-sm text-amber-800" data-testid="mlcmp-failures">
              Models that failed during this comparison run are listed with
              their errors above; they never block the other models.
            </div>
          )}
        </>
      )}

      {!result && !running && !error && (
        <p className="text-sm text-slate-500">
          Choose a ticker and date range, then press “Compare All 12 Models”.
          Every model solves the same BUY / HOLD / SELL problem on the same
          chronological split, so the table is a fair like-for-like comparison.
        </p>
      )}

      <div className="rounded-lg border border-slate-200 bg-white p-5 text-sm text-slate-600 shadow-sm">
        <h2 className="font-medium text-slate-800">Run a full Top-N ML portfolio backtest</h2>
        <p className="mt-1">
          After picking a model, run the portfolio flow (Top-N by P(BUY),
          rebalancing, per-ticker trades) from the{' '}
          <button
            type="button"
            className="font-medium text-emerald-700 underline"
            onClick={() => navigate('/strategy-builder', { state: { mlForm: form } })}
          >
            Strategy Builder
          </button>{' '}
          ML Strategy tab. Results persist to history and open in the standard
          results dashboard.
        </p>
      </div>
    </section>
  )
}

function round2(n) {
  return Number.isFinite(n) ? Math.round(n * 100) / 100 : null
}

function pct1(value) {
  const n = Number(value)
  if (!Number.isFinite(n)) return '—'
  return `${n > 0 ? '+' : ''}${(n * 100).toFixed(1)}%`
}

function num2(value) {
  const n = Number(value)
  return Number.isFinite(n) ? n.toFixed(2) : '—'
}
