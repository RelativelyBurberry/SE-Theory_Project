import { useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { validateTicker } from '../services/dataService'
import { previewStrategy } from '../services/strategyService'
import { runBacktest } from '../services/backtestService'
import { trainMlModel, compareMlModels } from '../services/mlService'
import { ML_MODEL_OPTIONS, ML_FEATURE_GROUP_OPTIONS } from '../services/mlService'
import {
  STRATEGY_DEFAULTS,
  STRATEGY_OPTIONS,
  validateStrategyForm,
  buildStrategyPayload,
} from '../utils/strategyDefaults'
import {
  ML_FORM_DEFAULTS,
  validateMlForm,
  buildMlTrainPayload,
  buildMlComparePayload,
} from '../utils/mlDefaults'

const PREVIEW_MAX_ROWS = 25

function signalBadge(signal) {
  if (signal === 'BUY') {
    return (
      <span className="rounded bg-emerald-100 px-2 py-0.5 text-xs font-semibold text-emerald-700">
        BUY
      </span>
    )
  }
  if (signal === 'SELL') {
    return (
      <span className="rounded bg-red-100 px-2 py-0.5 text-xs font-semibold text-red-700">
        SELL
      </span>
    )
  }
  return <span className="text-xs text-slate-400">—</span>
}

function fieldError(errors, name) {
  return errors[name] ? (
    <p className="mt-1 text-xs font-medium text-red-600" data-testid={`error-${name}`}>
      {errors[name]}
    </p>
  ) : null
}

const baseInput =
  'mt-1 w-full rounded-md border border-slate-300 px-3 py-2 text-sm focus:border-emerald-500 focus:outline-none'

/**
 * Strategy Builder (Phase 6 + ML module) — strategy configuration + signal
 * preview + backtest execution.
 *
 * Strategy Type radio (ML plan section 15):
 *  - Technical Strategy: the original Phase 4/6 flow (unchanged behavior).
 *  - ML Strategy: model selection, label configuration, feature groups,
 *    train-on-validation-metrics view and Top-N portfolio backtest, all served
 *    by the new /ml/* endpoints. The ML module only produces predictions; all
 *    execution stays in the existing t+1-open R engine.
 */
export default function StrategyBuilderPage() {
  const [strategyType, setStrategyType] = useState('technical')

  const [form, setForm] = useState({
    ticker: 'AAPL',
    capital: '10000',
    start: '2024-01-01',
    end: '2024-03-31',
    strategy: 'MA_Crossover',
    fastPeriod: String(STRATEGY_DEFAULTS.MA_Crossover.fast_period),
    slowPeriod: String(STRATEGY_DEFAULTS.MA_Crossover.slow_period),
    rsiPeriod: String(STRATEGY_DEFAULTS.RSI.period),
    oversold: String(STRATEGY_DEFAULTS.RSI.oversold),
    overbought: String(STRATEGY_DEFAULTS.RSI.overbought),
  })

  const [mlForm, setMlForm] = useState({ ...ML_FORM_DEFAULTS, end: defaultEndDate() })

  const [errors, setErrors] = useState({})
  const [touched, setTouched] = useState({})

  const [tickerCheck, setTickerCheck] = useState({ state: 'idle' }) // idle|loading|valid|invalid
  const [previewing, setPreviewing] = useState(false)
  const [preview, setPreview] = useState(null) // strategyService response
  const [previewError, setPreviewError] = useState(null)

  const [running, setRunning] = useState(false)
  const [runError, setRunError] = useState(null)

  // ML state
  const [mlTraining, setMlTraining] = useState(false)
  const [mlTrainResult, setMlTrainResult] = useState(null)
  const [mlTrainError, setMlTrainError] = useState(null)
  const [mlComparing, setMlComparing] = useState(false)
  const [mlCompareResult, setMlCompareResult] = useState(null)
  const [mlCompareError, setMlCompareError] = useState(null)

  const navigate = useNavigate()

  function defaultEndDate() {
    return new Date().toISOString().slice(0, 10)
  }

  function setField(name, value) {
    setForm((prev) => ({ ...prev, [name]: value }))
    // Live re-validation of the whole form only after it has been touched.
    if (touched[name]) {
      const result = validateStrategyForm({ ...form, [name]: value })
      setErrors(result.errors)
    }
  }

  function handleBlur(name) {
    setTouched((prev) => ({ ...prev, [name]: true }))
    const result = validateStrategyForm(form)
    setErrors(result.errors)
  }

  function setMlField(name, value) {
    setMlForm((prev) => ({ ...prev, [name]: value }))
    if (touched[`ml_${name}`]) {
      const result = validateMlForm({ ...mlForm, [name]: value })
      setErrors(result.errors)
    }
  }

  function touchAllTechnical() {
    setTouched((prev) => ({
      ...prev,
      ticker: true, capital: true, start: true, end: true,
      fastPeriod: true, slowPeriod: true, rsiPeriod: true,
      oversold: true, overbought: true,
    }))
  }

  function touchAllMl() {
    setTouched((prev) => ({
      ...prev,
      ml_ticker: true, ml_capital: true, ml_start: true, ml_end: true,
      ml_horizon: true, ml_buyThreshold: true, ml_sellThreshold: true,
      ml_featureGroups: true, ml_topN: true, ml_rebalance: true,
    }))
  }

  function switchStrategyType(type) {
    setStrategyType(type)
    setPreview(null)
    setPreviewError(null)
    setRunError(null)
    setMlTrainResult(null)
    setMlTrainError(null)
    setMlCompareResult(null)
    setMlCompareError(null)
    setErrors({})
  }

  async function handleTickerValidate() {
    const ticker = ((strategyType === 'ml' ? mlForm : form).ticker || '').trim().toUpperCase()
    if (!ticker) {
      setTickerCheck({ state: 'invalid', error: 'Ticker is required.' })
      return
    }
    setTickerCheck({ state: 'loading' })
    try {
      const result = await validateTicker(ticker)
      setTickerCheck({
        state: result.valid ? 'valid' : 'invalid',
        error: result.valid ? null : result.error ?? 'No data found for this ticker.',
      })
    } catch (err) {
      setTickerCheck({
        state: 'invalid',
        error: err?.response?.data?.error ?? 'Unable to validate ticker right now.',
      })
    }
  }

  async function handlePreview() {
    const result = validateStrategyForm(form)
    touchAllTechnical()
    setErrors(result.errors)
    if (!result.valid) return

    setPreviewing(true)
    setPreview(null)
    setPreviewError(null)
    try {
      const payload = await previewStrategy(buildStrategyPayload(form))
      setPreview(payload)
    } catch (err) {
      setPreviewError(
        err?.response?.data?.error ?? 'Unable to preview the strategy right now.',
      )
    } finally {
      setPreviewing(false)
    }
  }

  async function handleRunBacktest() {
    const result = validateStrategyForm(form)
    touchAllTechnical()
    setErrors(result.errors)
    if (!result.valid) return

    setRunning(true)
    setRunError(null)
    try {
      const payload = buildStrategyPayload(form)
      const response = await runBacktest(payload)
      if (response.success && response.backtestId) {
        navigate(`/results/${response.backtestId}`)
      } else {
        setRunError('Backtest completed but no ID was returned.')
      }
    } catch (err) {
      setRunError(
        err?.response?.data?.error ?? 'Unable to run the backtest right now.',
      )
    } finally {
      setRunning(false)
    }
  }

  async function handleMlTrain() {
    const result = validateMlForm(mlForm)
    touchAllMl()
    setErrors(result.errors)
    if (!result.valid) return

    setMlTraining(true)
    setMlTrainResult(null)
    setMlTrainError(null)
    try {
      const data = await trainMlModel(buildMlTrainPayload(mlForm))
      setMlTrainResult(data)
    } catch (err) {
      setMlTrainError(
        err?.response?.data?.error ?? 'Unable to train the model right now.',
      )
    } finally {
      setMlTraining(false)
    }
  }

  async function handleMlCompare() {
    const result = validateMlForm(mlForm)
    touchAllMl()
    setErrors(result.errors)
    if (!result.valid) return

    setMlComparing(true)
    setMlCompareResult(null)
    setMlCompareError(null)
    try {
      // The two fastest models keep the in-page comparison quick; the full
      // 12-model comparison lives on the ML Comparison page.
      const data = await compareMlModels(
        buildMlComparePayload(mlForm, ['decision_tree', 'random_forest', 'xgboost']),
      )
      setMlCompareResult(data)
    } catch (err) {
      setMlCompareError(
        err?.response?.data?.error ?? 'Unable to run the model comparison right now.',
      )
    } finally {
      setMlComparing(false)
    }
  }

  const { valid } =
    strategyType === 'ml'
      ? validateMlForm(mlForm)
      : validateStrategyForm(form)

  const rows = preview?.signals ?? []
  const previewRows = rows.slice(0, PREVIEW_MAX_ROWS)
  const columns =
    form.strategy === 'MA_Crossover'
      ? ['fast_sma', 'slow_sma']
      : ['rsi']

  return (
    <section className="mx-auto max-w-3xl space-y-6">
      <div>
        <h1 className="text-2xl font-semibold text-slate-900">Strategy Builder</h1>
        <p className="mt-1 text-sm text-slate-500">
          Configure a strategy, preview the BUY / SELL signals it generates,
          then run a full backtest through the t+1-open execution engine. All
          values are produced by the platform&apos;s R analytics — nothing is
          fabricated.
        </p>
      </div>

      {/* Strategy type selector (ML plan section 15) */}
      <div className="flex gap-4 rounded-lg border border-slate-200 bg-white p-4 shadow-sm">
        {[
          { value: 'technical', label: 'Technical Strategy' },
          { value: 'ml', label: 'ML Strategy' },
        ].map((opt) => (
          <label key={opt.value} className="flex items-center gap-2 text-sm font-medium text-slate-700">
            <input
              type="radio"
              name="strategyType"
              value={opt.value}
              checked={strategyType === opt.value}
              onChange={() => switchStrategyType(opt.value)}
              className="h-4 w-4 accent-emerald-600"
              data-testid={`strategy-type-${opt.value}`}
            />
            {opt.label}
          </label>
        ))}
      </div>

      {strategyType === 'technical' ? (
      <form
        className="space-y-6 rounded-lg border border-slate-200 bg-white p-6 shadow-sm"
        onSubmit={(e) => {
          e.preventDefault()
          handlePreview()
        }}
        noValidate
      >
        <div>
          <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
            1 · Instrument &amp; Capital
          </h2>
          <div className="mt-3 grid gap-4 sm:grid-cols-2">
            <div>
              <label className="block text-sm font-medium text-slate-700" htmlFor="ticker">
                Stock ticker
              </label>
              <div className="flex gap-2">
                <input
                  id="ticker"
                  type="text"
                  name="ticker"
                  value={form.ticker}
                  onChange={(e) => setField('ticker', e.target.value.toUpperCase())}
                  onBlur={() => {
                    handleBlur('ticker')
                    handleTickerValidate()
                  }}
                  placeholder="AAPL"
                  aria-invalid={Boolean(errors.ticker)}
                  className={`${baseInput} ${errors.ticker ? 'border-red-400' : ''}`}
                />
                <button
                  type="button"
                  onClick={handleTickerValidate}
                  disabled={tickerCheck.state === 'loading' || !form.ticker}
                  className="shrink-0 rounded-md bg-slate-700 px-3 py-2 text-sm font-medium text-white hover:bg-slate-600 disabled:opacity-50"
                >
                  {tickerCheck.state === 'loading' ? 'Checking…' : 'Check'}
                </button>
              </div>
              {tickerCheck.state === 'valid' && (
                <p className="mt-1 text-xs font-medium text-emerald-600" data-testid="ticker-valid">
                  ✓ {form.ticker.trim().toUpperCase()} resolves on the data source.
                </p>
              )}
              {tickerCheck.state === 'invalid' && (
                <p className="mt-1 text-xs font-medium text-red-600" data-testid="ticker-invalid">
                  {tickerCheck.error}
                </p>
              )}
              {fieldError(errors, 'ticker')}
            </div>

            <div>
              <label className="block text-sm font-medium text-slate-700" htmlFor="capital">
                Initial capital ($)
              </label>
              <input
                id="capital"
                type="number"
                name="capital"
                min="1"
                step="100"
                value={form.capital}
                onChange={(e) => setField('capital', e.target.value)}
                onBlur={() => handleBlur('capital')}
                aria-invalid={Boolean(errors.capital)}
                className={`${baseInput} ${errors.capital ? 'border-red-400' : ''}`}
              />
              {fieldError(errors, 'capital')}
            </div>
          </div>
        </div>

        <div>
          <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
            2 · Date Range
          </h2>
          <div className="mt-3 grid gap-4 sm:grid-cols-2">
            <div>
              <label className="block text-sm font-medium text-slate-700" htmlFor="start">
                Start date
              </label>
              <input
                id="start"
                type="date"
                name="startDate"
                value={form.start}
                onChange={(e) => setField('start', e.target.value)}
                onBlur={() => handleBlur('start')}
                aria-invalid={Boolean(errors.start)}
                className={`${baseInput} ${errors.start ? 'border-red-400' : ''}`}
              />
              {fieldError(errors, 'start')}
            </div>
            <div>
              <label className="block text-sm font-medium text-slate-700" htmlFor="end">
                End date
              </label>
              <input
                id="end"
                type="date"
                name="endDate"
                value={form.end}
                onChange={(e) => setField('end', e.target.value)}
                onBlur={() => handleBlur('end')}
                aria-invalid={Boolean(errors.end)}
                className={`${baseInput} ${errors.end ? 'border-red-400' : ''}`}
              />
              {fieldError(errors, 'end')}
            </div>
          </div>
        </div>

        <div>
          <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
            3 · Strategy
          </h2>
          <div className="mt-3 space-y-4">
            <div>
              <label className="block text-sm font-medium text-slate-700" htmlFor="strategy">
                Strategy
              </label>
              <select
                id="strategy"
                name="strategy"
                value={form.strategy}
                onChange={(e) => setField('strategy', e.target.value)}
                className={baseInput}
              >
                {STRATEGY_OPTIONS.map((s) => (
                  <option key={s.value} value={s.value}>
                    {s.label}
                  </option>
                ))}
              </select>
            </div>

            {form.strategy === 'MA_Crossover' ? (
              <div className="grid gap-4 sm:grid-cols-2">
                <div>
                  <label className="block text-sm font-medium text-slate-700" htmlFor="fastPeriod">
                    Fast MA period
                  </label>
                  <input
                    id="fastPeriod"
                    type="number"
                    name="fastPeriod"
                    min="1"
                    value={form.fastPeriod}
                    onChange={(e) => setField('fastPeriod', e.target.value)}
                    onBlur={() => handleBlur('fastPeriod')}
                    aria-invalid={Boolean(errors.fastPeriod)}
                    className={`${baseInput} ${errors.fastPeriod ? 'border-red-400' : ''}`}
                  />
                  {fieldError(errors, 'fastPeriod')}
                </div>
                <div>
                  <label className="block text-sm font-medium text-slate-700" htmlFor="slowPeriod">
                    Slow MA period
                  </label>
                  <input
                    id="slowPeriod"
                    type="number"
                    name="slowPeriod"
                    min="1"
                    value={form.slowPeriod}
                    onChange={(e) => setField('slowPeriod', e.target.value)}
                    onBlur={() => handleBlur('slowPeriod')}
                    aria-invalid={Boolean(errors.slowPeriod)}
                    className={`${baseInput} ${errors.slowPeriod ? 'border-red-400' : ''}`}
                  />
                  {fieldError(errors, 'slowPeriod')}
                </div>
              </div>
            ) : (
              <div className="grid gap-4 sm:grid-cols-3">
                <div>
                  <label className="block text-sm font-medium text-slate-700" htmlFor="rsiPeriod">
                    RSI period
                  </label>
                  <input
                    id="rsiPeriod"
                    type="number"
                    name="rsiPeriod"
                    min="1"
                    value={form.rsiPeriod}
                    onChange={(e) => setField('rsiPeriod', e.target.value)}
                    onBlur={() => handleBlur('rsiPeriod')}
                    aria-invalid={Boolean(errors.rsiPeriod)}
                    className={`${baseInput} ${errors.rsiPeriod ? 'border-red-400' : ''}`}
                  />
                  {fieldError(errors, 'rsiPeriod')}
                </div>
                <div>
                  <label className="block text-sm font-medium text-slate-700" htmlFor="oversold">
                    Oversold
                  </label>
                  <input
                    id="oversold"
                    type="number"
                    name="oversold"
                    min="1"
                    max="99"
                    value={form.oversold}
                    onChange={(e) => setField('oversold', e.target.value)}
                    onBlur={() => handleBlur('oversold')}
                    aria-invalid={Boolean(errors.oversold)}
                    className={`${baseInput} ${errors.oversold ? 'border-red-400' : ''}`}
                  />
                  {fieldError(errors, 'oversold')}
                </div>
                <div>
                  <label className="block text-sm font-medium text-slate-700" htmlFor="overbought">
                    Overbought
                  </label>
                  <input
                    id="overbought"
                    type="number"
                    name="overbought"
                    min="1"
                    max="99"
                    value={form.overbought}
                    onChange={(e) => setField('overbought', e.target.value)}
                    onBlur={() => handleBlur('overbought')}
                    aria-invalid={Boolean(errors.overbought)}
                    className={`${baseInput} ${errors.overbought ? 'border-red-400' : ''}`}
                  />
                  {fieldError(errors, 'overbought')}
                </div>
              </div>
            )}
          </div>
        </div>

        <div>
          <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
            4 · Run
          </h2>
          <div className="mt-3 flex flex-col gap-3 sm:flex-row">
            <button
              type="submit"
              disabled={previewing || !valid}
              className="sm:w-1/2 rounded-md border border-slate-300 bg-white px-4 py-2 text-sm font-medium text-slate-700 hover:bg-slate-50 disabled:opacity-60"
              data-testid="preview-button"
            >
              {previewing ? 'Generating signals…' : 'Preview Strategy'}
            </button>

            <button
              type="button"
              onClick={handleRunBacktest}
              disabled={running || !valid}
              className="sm:w-1/2 rounded-md bg-emerald-600 px-4 py-2 text-sm font-medium text-white hover:bg-emerald-700 disabled:opacity-60"
              data-testid="run-backtest-button"
            >
              {running ? 'Running backtest…' : 'Run Backtest'}
            </button>
          </div>

          {runError && (
            <div
              className="mt-3 rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-700"
              data-testid="run-error"
            >
              {runError}
            </div>
          )}
        </div>
      </form>
      ) : null}

      {previewError && (
        <div
          className="rounded-lg border border-red-200 bg-red-50 p-4 text-sm text-red-700"
          data-testid="preview-error"
        >
          {previewError}
        </div>
      )}

      {preview && strategyType === 'technical' && (
        <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm">
          <h2 className="font-medium text-slate-800">
            {preview.ticker} · {preview.strategy} · {preview.signal_count} signal
            {preview.signal_count === 1 ? '' : 's'}
          </h2>
          <p className="mt-1 text-sm text-slate-500">
            {preview.start} → {preview.end} · {preview.row_count} trading rows
            {rows.length > PREVIEW_MAX_ROWS &&
              ` · showing first ${PREVIEW_MAX_ROWS}`}
          </p>
          <div className="mt-3 overflow-x-auto">
            <table className="w-full text-left text-sm">
              <thead>
                <tr className="border-b border-slate-200 text-slate-500">
                  <th className="py-2 pr-3 font-medium">Date</th>
                  <th className="py-2 pr-3 font-medium">Close</th>
                  {columns.map((col) => (
                    <th key={col} className="py-2 pr-3 font-medium">
                      {col === 'fast_sma' ? 'Fast SMA' : col === 'slow_sma' ? 'Slow SMA' : 'RSI'}
                    </th>
                  ))}
                  <th className="py-2 font-medium">Signal</th>
                </tr>
              </thead>
              <tbody>
                {previewRows.map((row) => (
                  <tr key={row.date} className="border-b border-slate-100">
                    <td className="py-2 pr-3 text-slate-700">{row.date}</td>
                    <td className="py-2 pr-3">{row.close?.toFixed(2)}</td>
                    {columns.map((col) => (
                      <td key={col} className="py-2 pr-3">
                        {row[col] != null ? Number(row[col]).toFixed(2) : '—'}
                      </td>
                    ))}
                    <td className="py-2">{signalBadge(row.signal)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <p className="mt-3 text-xs text-slate-400">
            Signals are generated at the close of day t from data available at
            or before t. A full backtest executes them at the t+1 open through
            the platform&apos;s R engine.
          </p>
        </div>
      )}

      {!preview && !previewError && strategyType === 'technical' && (
        <p className="text-sm text-slate-500">
          Fill in the form and press “Preview Strategy” to see the generated
          BUY / SELL signals (no trades are executed).
        </p>
      )}

      {strategyType === 'ml' && (
        <MlStrategyForm
          mlForm={mlForm}
          setMlField={setMlField}
          errors={errors}
          touched={touched}
          tickerCheck={tickerCheck}
          onTickerValidate={handleTickerValidate}
          baseInput={baseInput}
          fieldError={fieldError}
          valid={valid}
          mlTraining={mlTraining}
          mlTrainResult={mlTrainResult}
          mlTrainError={mlTrainError}
          onTrain={handleMlTrain}
          mlComparing={mlComparing}
          mlCompareResult={mlCompareResult}
          mlCompareError={mlCompareError}
          onCompare={handleMlCompare}
          navigate={navigate}
        />
      )}
    </section>
  )
}

/** Confusion-matrix mini table (predicted rows x actual columns). */
function ConfusionMatrixTable({ cm }) {
  if (!Array.isArray(cm) || cm.length === 0) return null
  const classes = cm.map((row) => row.predicted)
  return (
    <table className="mt-2 text-sm" data-testid="ml-confusion-matrix">
      <thead>
        <tr className="text-slate-500">
          <th className="pr-3 text-left font-medium">Predicted ↓ / Actual →</th>
          {classes.map((c) => (
            <th key={c} className="px-3 font-medium">{c}</th>
          ))}
        </tr>
      </thead>
      <tbody>
        {cm.map((row) => (
          <tr key={row.predicted}>
            <td className="pr-3 font-medium text-slate-700">{row.predicted}</td>
            {classes.map((c) => (
              <td key={c} className="px-3 text-center tabular-nums">
                {row.actual_counts?.[c] ?? 0}
              </td>
            ))}
          </tr>
        ))}
      </tbody>
    </table>
  )
}

/** The ML Strategy form + train/compare results. */
function MlStrategyForm({
  mlForm, setMlField, errors, touched, tickerCheck, onTickerValidate,
  baseInput, fieldError, valid,
  mlTraining, mlTrainResult, mlTrainError, onTrain,
  mlComparing, mlCompareResult, mlCompareError, onCompare, navigate,
}) {
  function toggleFeatureGroup(group) {
    const current = mlForm.featureGroups ?? []
    const next = current.includes(group)
      ? current.filter((g) => g !== group)
      : [...current, group]
    setMlField('featureGroups', next)
  }

  const vm = mlTrainResult?.validation_metrics
  const importance = mlTrainResult?.feature_importance
    ? Object.entries(mlTrainResult.feature_importance)
        .sort((a, b) => Number(b[1]) - Number(a[1]))
        .slice(0, 8)
    : []

  return (
    <form
      className="space-y-6 rounded-lg border border-slate-200 bg-white p-6 shadow-sm"
      onSubmit={(e) => {
        e.preventDefault()
        onTrain()
      }}
      noValidate
    >
      <div>
        <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
          1 · Instrument &amp; Capital
        </h2>
        <div className="mt-3 grid gap-4 sm:grid-cols-2">
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-ticker">
              Stock ticker
            </label>
            <div className="flex gap-2">
              <input
                id="ml-ticker"
                type="text"
                value={mlForm.ticker}
                onChange={(e) => setMlField('ticker', e.target.value.toUpperCase())}
                onBlur={() => onTickerValidate()}
                placeholder="AAPL"
                aria-invalid={Boolean(errors.ticker)}
                className={`${baseInput} ${errors.ticker ? 'border-red-400' : ''}`}
              />
              <button
                type="button"
                onClick={onTickerValidate}
                disabled={tickerCheck.state === 'loading' || !mlForm.ticker}
                className="shrink-0 rounded-md bg-slate-700 px-3 py-2 text-sm font-medium text-white hover:bg-slate-600 disabled:opacity-50"
              >
                {tickerCheck.state === 'loading' ? 'Checking…' : 'Check'}
              </button>
            </div>
            {tickerCheck.state === 'valid' && (
              <p className="mt-1 text-xs font-medium text-emerald-600">
                ✓ {mlForm.ticker.trim().toUpperCase()} resolves on the data source.
              </p>
            )}
            {tickerCheck.state === 'invalid' && (
              <p className="mt-1 text-xs font-medium text-red-600">{tickerCheck.error}</p>
            )}
            {fieldError(errors, 'ticker')}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-capital">
              Initial capital ($)
            </label>
            <input
              id="ml-capital"
              type="number"
              min="1"
              step="100"
              value={mlForm.capital}
              onChange={(e) => setMlField('capital', e.target.value)}
              aria-invalid={Boolean(errors.capital)}
              className={`${baseInput} ${errors.capital ? 'border-red-400' : ''}`}
            />
            {fieldError(errors, 'capital')}
          </div>
        </div>
      </div>

      <div>
        <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
          2 · Date Range
        </h2>
        <p className="mt-1 text-xs text-slate-500">
          Data is split chronologically (train → validation → test); the test
          window is scored out-of-sample. Needs ≥ 1 year of history.
        </p>
        <div className="mt-3 grid gap-4 sm:grid-cols-2">
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-start">
              Start date
            </label>
            <input
              id="ml-start"
              type="date"
              value={mlForm.start}
              onChange={(e) => setMlField('start', e.target.value)}
              aria-invalid={Boolean(errors.start)}
              className={`${baseInput} ${errors.start ? 'border-red-400' : ''}`}
            />
            {fieldError(errors, 'start')}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-end">
              End date
            </label>
            <input
              id="ml-end"
              type="date"
              value={mlForm.end}
              onChange={(e) => setMlField('end', e.target.value)}
              aria-invalid={Boolean(errors.end)}
              className={`${baseInput} ${errors.end ? 'border-red-400' : ''}`}
            />
            {fieldError(errors, 'end')}
          </div>
        </div>
      </div>

      <div>
        <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
          3 · Model &amp; Labels
        </h2>
        <div className="mt-3 grid gap-4 sm:grid-cols-2">
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-model">
              Model
            </label>
            <select
              id="ml-model"
              value={mlForm.model}
              onChange={(e) => setMlField('model', e.target.value)}
              className={baseInput}
            >
              {ML_MODEL_OPTIONS.map((m) => (
                <option key={m.value} value={m.value}>{m.label}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-horizon">
              Prediction horizon (trading days)
            </label>
            <input
              id="ml-horizon"
              type="number"
              min="1"
              max="60"
              value={mlForm.horizon}
              onChange={(e) => setMlField('horizon', e.target.value)}
              aria-invalid={Boolean(errors.horizon)}
              className={`${baseInput} ${errors.horizon ? 'border-red-400' : ''}`}
            />
            {fieldError(errors, 'horizon')}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-buy">
              BUY threshold (%)
            </label>
            <input
              id="ml-buy"
              type="number"
              step="0.5"
              min="0.1"
              value={mlForm.buyThreshold}
              onChange={(e) => setMlField('buyThreshold', e.target.value)}
              aria-invalid={Boolean(errors.buyThreshold)}
              className={`${baseInput} ${errors.buyThreshold ? 'border-red-400' : ''}`}
            />
            {fieldError(errors, 'buyThreshold')}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-sell">
              SELL threshold (%)
            </label>
            <input
              id="ml-sell"
              type="number"
              step="0.5"
              min="0.1"
              value={mlForm.sellThreshold}
              onChange={(e) => setMlField('sellThreshold', e.target.value)}
              aria-invalid={Boolean(errors.sellThreshold)}
              className={`${baseInput} ${errors.sellThreshold ? 'border-red-400' : ''}`}
            />
            {fieldError(errors, 'sellThreshold')}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-evaluate-on">
              Score on
            </label>
            <select
              id="ml-evaluate-on"
              value={mlForm.evaluateOn}
              onChange={(e) => setMlField('evaluateOn', e.target.value)}
              className={baseInput}
            >
              <option value="test">Test window (out-of-sample)</option>
              <option value="validation">Validation window</option>
            </select>
          </div>
          <div className="flex items-end pb-2">
            <label className="flex items-center gap-2 text-sm font-medium text-slate-700">
              <input
                type="checkbox"
                checked={Boolean(mlForm.tune)}
                onChange={(e) => setMlField('tune', e.target.checked)}
                className="h-4 w-4 accent-emerald-600"
              />
              Tune hyperparameters on validation
            </label>
          </div>
        </div>
      </div>

      <div>
        <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
          4 · Features
        </h2>
        {fieldError(errors, 'featureGroups')}
        <div className="mt-2 flex flex-wrap gap-3">
          {ML_FEATURE_GROUP_OPTIONS.map((g) => (
            <label key={g.value} className="flex items-center gap-2 text-sm text-slate-700">
              <input
                type="checkbox"
                checked={(mlForm.featureGroups ?? []).includes(g.value)}
                onChange={() => toggleFeatureGroup(g.value)}
                className="h-4 w-4 accent-emerald-600"
                data-testid={`feature-${g.value}`}
              />
              {g.label}
            </label>
          ))}
        </div>
      </div>

      <div>
        <h2 className="text-sm font-semibold uppercase tracking-wide text-slate-500">
          5 · Portfolio &amp; Run
        </h2>
        <div className="mt-3 grid gap-4 sm:grid-cols-2">
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-topn">
              Portfolio selection (Top-N)
            </label>
            <input
              id="ml-topn"
              type="number"
              min="1"
              max="10"
              value={mlForm.topN}
              onChange={(e) => setMlField('topN', e.target.value)}
              aria-invalid={Boolean(errors.topN)}
              className={`${baseInput} ${errors.topN ? 'border-red-400' : ''}`}
            />
            {fieldError(errors, 'topN')}
          </div>
          <div>
            <label className="block text-sm font-medium text-slate-700" htmlFor="ml-rebalance">
              Rebalancing
            </label>
            <select
              id="ml-rebalance"
              value={mlForm.rebalance}
              onChange={(e) => setMlField('rebalance', e.target.value)}
              className={baseInput}
            >
              {['daily', 'weekly', 'monthly'].map((r) => (
                <option key={r} value={r}>
                  {r.charAt(0).toUpperCase() + r.slice(1)}
                </option>
              ))}
            </select>
          </div>
        </div>

        <div className="mt-4 flex flex-col gap-3 sm:flex-row">
          <button
            type="submit"
            disabled={mlTraining || !valid}
            className="sm:w-1/2 rounded-md bg-emerald-600 px-4 py-2 text-sm font-medium text-white hover:bg-emerald-700 disabled:opacity-60"
            data-testid="ml-train-button"
          >
            {mlTraining ? 'Training…' : 'Train Model'}
          </button>
          <button
            type="button"
            onClick={onCompare}
            disabled={mlComparing || !valid}
            className="sm:w-1/2 rounded-md border border-slate-300 bg-white px-4 py-2 text-sm font-medium text-slate-700 hover:bg-slate-50 disabled:opacity-60"
            data-testid="ml-compare-button"
          >
            {mlComparing ? 'Comparing models…' : 'Quick Compare (3 models)'}
          </button>
        </div>

        {mlTraining && (
          <p className="mt-3 text-sm text-slate-500" data-testid="ml-training-progress">
            Building features, training on the chronological train window and
            scoring on the {mlForm.evaluateOn} window — this can take up to a
            minute.
          </p>
        )}

        {mlTrainError && (
          <div
            className="mt-3 rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-700"
            data-testid="ml-train-error"
          >
            {mlTrainError}
          </div>
        )}
        {mlCompareError && (
          <div
            className="mt-3 rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-700"
            data-testid="ml-compare-error"
          >
            {mlCompareError}
          </div>
        )}
      </div>

      {mlCompareResult && (
        <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm" data-testid="ml-compare-result">
          <h2 className="font-medium text-slate-800">
            Model comparison · {mlCompareResult.ticker}
            {mlCompareResult.best_by_f1 ? ` · best F1: ${mlCompareResult.best_by_f1}` : ''}
          </h2>
          <div className="mt-3 overflow-x-auto">
            <table className="w-full text-left text-sm">
              <thead>
                <tr className="border-b border-slate-200 text-slate-500">
                  <th className="py-2 pr-3 font-medium">Model</th>
                  <th className="py-2 pr-3 font-medium">Accuracy</th>
                  <th className="py-2 pr-3 font-medium">F1</th>
                  <th className="py-2 pr-3 font-medium">ROC-AUC</th>
                  <th className="py-2 pr-3 font-medium">CAGR</th>
                  <th className="py-2 pr-3 font-medium">Sharpe</th>
                  <th className="py-2 pr-3 font-medium">Max DD</th>
                  <th className="py-2 font-medium">Win Rate</th>
                </tr>
              </thead>
              <tbody>
                {(mlCompareResult.comparison ?? []).map((row) => (
                  <tr key={row.Model} className="border-b border-slate-100">
                    <td className="py-2 pr-3 font-medium text-slate-700">{row.Model}</td>
                    {row.Status === 'failed' ? (
                      <td colSpan={7} className="py-2 text-xs text-red-600">{row.Error}</td>
                    ) : (
                      <>
                        <td className="py-2 pr-3 tabular-nums">{pct(row.Accuracy)}</td>
                        <td className="py-2 pr-3 tabular-nums">{num(row.F1)}</td>
                        <td className="py-2 pr-3 tabular-nums">{num(row.ROC_AUC)}</td>
                        <td className="py-2 pr-3 tabular-nums">{pct(row.CAGR)}</td>
                        <td className="py-2 pr-3 tabular-nums">{num(row.Sharpe)}</td>
                        <td className="py-2 pr-3 tabular-nums">{pct(row.MaxDrawdown)}</td>
                        <td className="py-2 tabular-nums">{pct(row.WinRate)}</td>
                      </>
                    )}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <p className="mt-3 text-xs text-slate-400">
            Full 12-model comparison with visualizations lives on the{' '}
            <button
              type="button"
              className="font-medium text-emerald-700 underline"
              onClick={() => navigate('/ml-compare', { state: { mlForm } })}
            >
              ML Model Comparison page
            </button>.
          </p>
        </div>
      )}

      {mlTrainResult && (
        <div className="rounded-lg border border-slate-200 bg-white p-5 shadow-sm" data-testid="ml-train-result">
          <h2 className="font-medium text-slate-800">
            {mlTrainResult.model_name} · {mlTrainResult.ticker}
            {mlTrainResult.evaluate_on === 'validation' ? ' (validation)' : ' (test, out-of-sample)'}
          </h2>
          <p className="mt-1 text-xs text-slate-500">
            Trained on {mlTrainResult.train_info?.n_train} rows · validation{' '}
            {mlTrainResult.train_info?.n_validation} · test{' '}
            {mlTrainResult.train_info?.n_test} · features used{' '}
            {mlTrainResult.train_info?.features_used?.length ?? 0}
            {mlTrainResult.train_info?.scaled ? ' · scaled' : ''}
            {mlTrainResult.model_id ? ` · model_id ${mlTrainResult.model_id}` : ''}
          </p>

          {vm && (
            <div className="mt-4 grid grid-cols-2 gap-3 sm:grid-cols-3">
              {[
                ['Accuracy', pct(vm.accuracy)],
                ['Precision', pct(vm.precision)],
                ['Recall', pct(vm.recall)],
                ['F1', num(vm.f1)],
                ['ROC-AUC (macro)', num(vm.roc_auc_macro)],
                ['PR-AUC (macro)', num(vm.pr_auc_macro)],
              ].map(([label, value]) => (
                <div key={label} className="rounded-md border border-slate-200 px-3 py-2">
                  <p className="text-xs font-medium uppercase tracking-wide text-slate-500">{label}</p>
                  <p className="mt-1 text-lg font-semibold tabular-nums text-slate-900">{value}</p>
                </div>
              ))}
            </div>
          )}

          {vm && <ConfusionMatrixTable cm={vm.confusion_matrix} />}

          {mlTrainResult.backtest?.metrics && (
            <div className="mt-4">
              <h3 className="text-sm font-medium text-slate-700">
                Backtest of the ML signals (existing t+1-open engine)
              </h3>
              <div className="mt-2 grid grid-cols-2 gap-3 sm:grid-cols-4">
                {[
                  ['Total Return', pct(mlTrainResult.backtest.metrics.totalReturn)],
                  ['CAGR', pct(mlTrainResult.backtest.metrics.cagr)],
                  ['Sharpe', num(mlTrainResult.backtest.metrics.sharpe)],
                  ['Max Drawdown', pct(mlTrainResult.backtest.metrics.maximumDrawdown)],
                ].map(([label, value]) => (
                  <div key={label} className="rounded-md border border-slate-200 px-3 py-2">
                    <p className="text-xs font-medium uppercase tracking-wide text-slate-500">{label}</p>
                    <p className="mt-1 text-sm font-semibold tabular-nums text-slate-900">{value}</p>
                  </div>
                ))}
              </div>
            </div>
          )}

          {importance.length > 0 && (
            <div className="mt-4">
              <h3 className="text-sm font-medium text-slate-700">Top features (importance)</h3>
              <ul className="mt-2 space-y-1">
                {importance.map(([name, value]) => (
                  <li key={name} className="flex items-center gap-2 text-sm">
                    <span className="w-40 shrink-0 truncate text-slate-600" title={name}>{name}</span>
                    <span className="h-2 flex-1 rounded bg-slate-100">
                      <span
                        className="block h-2 rounded bg-emerald-500"
                        style={{ width: `${Math.max(2, Math.round(Number(value) * 100))}%` }}
                      />
                    </span>
                    <span className="w-12 text-right tabular-nums text-slate-500">
                      {Number(value).toFixed(3)}
                    </span>
                  </li>
                ))}
              </ul>
            </div>
          )}

          <p className="mt-4 text-xs text-slate-400">
            High classification accuracy alone does not imply a profitable
            strategy — always read the backtest metrics alongside.
          </p>
        </div>
      )}
    </form>
  )
}

function num(value, decimals = 3) {
  const n = Number(value)
  return Number.isFinite(n) ? n.toFixed(decimals) : '—'
}

function pct(value) {
  const n = Number(value)
  if (!Number.isFinite(n)) return '—'
  return `${n > 0 ? '+' : ''}${(n * 100).toFixed(2)}%`
}
