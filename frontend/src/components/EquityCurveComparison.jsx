import { useEffect, useMemo, useState } from 'react'
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  Tooltip,
  Legend,
  CartesianGrid,
  ResponsiveContainer,
} from 'recharts'
import { getBacktest, getBacktestHistory } from '../services/backtestService'
import { formatCurrency } from '../utils/format'

/**
 * Equity Curve Comparison (Figure 4 of the research paper).
 *
 * Compares up to three strategies — MA Crossover (50/200), RSI (14, 30/70)
 * and Buy & Hold — for a selected asset on a single, research-quality chart.
 *
 * Data contract (no fabrication, ever):
 *  - The currently viewed backtest (prop `backtest`) supplies one series.
 *  - GET /backtest/history lists the user's persisted runs; GET /backtest/:id
 *    fetches each full record so the chart only ever plots REAL equity curves
 *    produced by the platform engine and stored in Neon.
 *  - All series are normalised to the same starting value (INDEX_BASE) purely
 *    for visual comparison; the underlying curve values are never altered.
 *  - Strategies that have no persisted run for the selected ticker are listed
 *    as missing — nothing is invented to fill the gap.
 *
 * Notes:
 *  - When multiple persisted runs exist for the same strategy, only the most
 *    recent run (highest id) is plotted so each strategy appears once.
 *  - Null/NaN portfolio values are rendered as gaps (connectNulls), never as
 *    fabricated values.
 */

const INDEX_BASE = 10000
const MAX_COMPARED_RUNS = 8

const STRATEGY_LABELS = {
  MA_Crossover: 'MA Cross (50/200)',
  RSI: 'RSI (14, 30/70)',
  Buy_and_Hold: 'Buy & Hold',
}

const STRATEGY_STYLES = {
  MA_Crossover: { color: '#059669', dashed: false },
  RSI: { color: '#2563eb', dashed: false },
  Buy_and_Hold: { color: '#64748b', dashed: true },
}

const DEFAULT_STYLE = { color: '#334155', dashed: false }

/** Human label derived from the strategy code + parameters. */
function strategyDisplayLabel(strategy, parameters = {}) {
  if (strategy === 'MA_Crossover') {
    const fast = parameters?.fast_period
    const slow = parameters?.slow_period
    return fast && slow ? `MA Cross (${fast}/${slow})` : STRATEGY_LABELS.MA_Crossover
  }
  if (strategy === 'RSI') {
    const period = parameters?.period
    const os = parameters?.oversold
    const ob = parameters?.overbought
    return period && os != null && ob != null
      ? `RSI (${period}, ${os}/${ob})`
      : STRATEGY_LABELS.RSI
  }
  return STRATEGY_LABELS[strategy] ?? strategy
}

/**
 * Normalise one raw equity curve to INDEX_BASE using the first measurable
 * portfolio value as the base. Invalid values (null / NaN) become nulls that
 * Recharts renders as gaps; the raw values are only scaled, never altered.
 */
function normalizeSeries(equityCurve, base) {
  const dates = []
  const values = []
  let firstValue = null
  for (const row of equityCurve) {
    const v = row?.portfolioValue
    if (v == null || !Number.isFinite(Number(v))) {
      dates.push(row?.date)
      values.push(null)
      continue
    }
    const n = Number(v)
    if (firstValue === null && n > 0) firstValue = n
    dates.push(row?.date)
    values.push(firstValue && firstValue > 0 ? (n / firstValue) * base : null)
  }
  // Drop any leading nulls (the series must have a measurable starting base).
  const firstIndex = values.findIndex((v) => v != null)
  if (firstIndex > 0) {
    return {
      dates: dates.slice(firstIndex),
      values: values.slice(firstIndex),
    }
  }
  return { dates, values }
}

function ComparisonTooltip({ active, payload, label }) {
  if (!active || !payload || payload.length === 0) return null
  return (
    <div className="rounded-md border border-slate-200 bg-white px-3 py-2 text-xs shadow-md">
      <p className="mb-1 font-semibold text-slate-800">{label}</p>
      {payload.map((entry) => (
        <p
          key={entry.dataKey ?? entry.name}
          className="flex items-center justify-between gap-4"
        >
          <span className="text-slate-600" style={{ color: entry.color }}>
            {entry.name}
          </span>
          <span className="font-medium tabular-nums text-slate-900">
            {entry.value != null && Number.isFinite(Number(entry.value))
              ? formatCurrency(Number(entry.value), 2)
              : '—'}
          </span>
        </p>
      ))}
    </div>
  )
}

export default function EquityCurveComparison({ backtest }) {
  const [selectedTicker, setSelectedTicker] = useState(backtest?.ticker ?? '')
  const [tickers, setTickers] = useState(
    backtest?.ticker ? [backtest.ticker] : [],
  )
  const [series, setSeries] = useState([])
  const [status, setStatus] = useState('loading')
  const [error, setError] = useState('')

  useEffect(() => {
    if (!selectedTicker) {
      setStatus('ready')
      setSeries([])
      return
    }
    let cancelled = false

    async function load() {
      setStatus('loading')
      setError('')
      try {
        const history = await getBacktestHistory(50, 0)
        const items = (history?.items ?? []).filter(
          (item) => item && item.ticker === selectedTicker,
        )
        const persistedIds = new Set(
          items.map((item) => item.id).filter((id) => id != null),
        )

        const runs = []
        for (const item of items.slice(0, MAX_COMPARED_RUNS)) {
          try {
            runs.push(await getBacktest(item.id))
          } catch {
            // An unreadable record is skipped — it must never block the chart.
          }
        }
        // The currently-viewed backtest is always a legitimate, real series.
        if (
          backtest &&
          backtest.ticker === selectedTicker &&
          (backtest.id == null || !persistedIds.has(backtest.id))
        ) {
          runs.push(backtest)
        }

        // Keep the most recent persisted run per strategy (one line each).
        const latest = new Map()
        for (const run of runs) {
          if (
            !run ||
            !Array.isArray(run.equityCurve) ||
            run.equityCurve.length === 0
          ) {
            continue
          }
          const previous = latest.get(run.strategy)
          if (
            !previous ||
            Number(run.id ?? 0) > Number(previous.id ?? 0)
          ) {
            latest.set(run.strategy, run)
          }
        }

        const list = []
        for (const run of latest.values()) {
          const { dates, values } = normalizeSeries(run.equityCurve, INDEX_BASE)
          if (dates.length === 0) continue
          list.push({
            strategy: run.strategy,
            label: strategyDisplayLabel(run.strategy, run.parameters),
            style: STRATEGY_STYLES[run.strategy] ?? DEFAULT_STYLE,
            dates,
            values,
            finalValue: values[values.length - 1],
          })
        }

        const allTickers = new Set(
          [
            ...(history?.items ?? []).map((item) => item.ticker),
            backtest?.ticker,
          ].filter(Boolean),
        )

        if (!cancelled) {
          setSeries(list)
          setTickers([...allTickers])
          setStatus('ready')
        }
      } catch (err) {
        if (cancelled) return
        setError(
          err?.response?.data?.error ??
            'Unable to load the equity-curve comparison right now.',
        )
        setStatus('error')
      }
    }

    load()
    return () => {
      cancelled = true
    }
  }, [selectedTicker, backtest])

  const present = new Set(series.map((s) => s.strategy))
  const missingStrategies = ['MA_Crossover', 'RSI', 'Buy_and_Hold'].filter(
    (s) => !present.has(s),
  )

  const chartData = useMemo(() => {
    const byDate = new Map()
    for (const s of series) {
      s.dates.forEach((date, i) => {
        if (!date) return
        if (!byDate.has(date)) byDate.set(date, { date })
        const value = s.values[i]
        byDate.get(date)[s.strategy] =
          value != null && Number.isFinite(Number(value)) ? Number(value) : null
      })
    }
    return [...byDate.values()].sort((a, b) => a.date.localeCompare(b.date))
  }, [series])

  const firstYear = chartData.length
    ? chartData[0].date.slice(0, 4)
    : (backtest?.startDate ?? '').slice(0, 4)
  const lastYear = chartData.length
    ? chartData[chartData.length - 1].date.slice(0, 4)
    : (backtest?.endDate ?? '').slice(0, 4)
  const periodText =
    firstYear && lastYear ? `${firstYear}–${lastYear}` : ''

  if (status === 'loading') {
    return (
      <div
        className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm"
        data-testid="equity-comparison"
      >
        <div
          className="h-6 w-56 animate-pulse rounded bg-slate-200"
          data-testid="equity-comparison-loading"
        />
        <div className="mt-4 h-[420px] animate-pulse rounded bg-slate-100" />
      </div>
    )
  }

  if (status === 'error') {
    return (
      <div
        className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm"
        data-testid="equity-comparison"
      >
        <h2 className="font-medium text-slate-800">Equity Curve Comparison</h2>
        <div
          className="mt-3 rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-700"
          data-testid="equity-comparison-error"
        >
          {error}
        </div>
      </div>
    )
  }

  return (
    <div
      className="rounded-lg border border-slate-200 bg-white p-6 shadow-sm"
      data-testid="equity-comparison"
    >
      <div className="flex flex-wrap items-start justify-between gap-3">
        <div>
          <h2 className="font-medium text-slate-800">
            Equity Curve Comparison — {selectedTicker || ''}
            {periodText ? ` (${periodText})` : ''}
          </h2>
          <p className="mt-1 text-sm text-slate-500">
            Indexed to ${INDEX_BASE.toLocaleString('en-US')} initial capital ·
            plotted from persisted backtest results (GET /backtest/:id) —
            close-to-close marking, signals executed at the t+1 open.
          </p>
        </div>
        {tickers.length > 1 && (
          <div className="flex items-center gap-2">
            <label
              htmlFor="comparison-asset"
              className="text-xs font-medium uppercase tracking-wide text-slate-500"
            >
              Asset
            </label>
            <select
              id="comparison-asset"
              value={selectedTicker}
              onChange={(event) => setSelectedTicker(event.target.value)}
              className="rounded-md border border-slate-300 bg-white px-3 py-1.5 text-sm text-slate-700 focus:border-emerald-500 focus:outline-none"
            >
              {tickers.map((t) => (
                <option key={t} value={t}>
                  {t}
                </option>
              ))}
            </select>
          </div>
        )}
      </div>

      {series.length === 0 ? (
        <p className="mt-6 text-sm text-slate-500" data-testid="comparison-empty">
          No persisted equity-curve data found for {selectedTicker || 'this asset'}.
        </p>
      ) : (
        <>
          <div className="mt-4 h-[420px]" data-testid="comparison-chart">
            <ResponsiveContainer width="100%" height="100%">
              <LineChart
                data={chartData}
                margin={{ top: 8, right: 16, bottom: 8, left: 8 }}
              >
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  dataKey="date"
                  tick={{ fontSize: 12, fill: '#475569' }}
                  tickFormatter={(d) => d.slice(0, 7)}
                  minTickGap={40}
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
                <Tooltip content={<ComparisonTooltip />} />
                <Legend
                  iconType="plainline"
                  wrapperStyle={{ fontSize: 12, paddingTop: 8 }}
                />
                {series.map((s) => (
                  <Line
                    key={s.strategy}
                    type="monotone"
                    dataKey={s.strategy}
                    name={s.label}
                    stroke={s.style.color}
                    strokeWidth={2}
                    strokeDasharray={s.style.dashed ? '6 3' : undefined}
                    dot={false}
                    connectNulls
                    isAnimationActive={false}
                  />
                ))}
              </LineChart>
            </ResponsiveContainer>
          </div>

          <div className="mt-4 grid gap-2 sm:grid-cols-3">
            {series.map((s) => (
              <div
                key={s.strategy}
                className="flex items-center justify-between rounded-md border border-slate-200 px-3 py-2"
              >
                <span className="flex items-center gap-2 text-xs font-medium text-slate-600">
                  <span
                    className="inline-block h-2 w-2 rounded-full"
                    style={{ backgroundColor: s.style.color }}
                  />
                  {s.label}
                </span>
                <span className="text-sm font-semibold tabular-nums text-slate-800">
                  {s.finalValue != null && Number.isFinite(Number(s.finalValue))
                    ? formatCurrency(Number(s.finalValue), 0)
                    : '—'}
                </span>
              </div>
            ))}
          </div>

          {missingStrategies.length > 0 && (
            <div
              className="mt-3 rounded-md border border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-800"
              data-testid="comparison-missing"
            >
              Showing {series.length} of 3 strategies for{' '}
              {selectedTicker || 'this asset'}. Not present in the saved
              backtests:{' '}
              {missingStrategies
                .map((s) => STRATEGY_LABELS[s] ?? s)
                .join(', ')}
              . Only actual persisted runs are plotted — no data is fabricated.
            </div>
          )}
        </>
      )}
    </div>
  )
}