/**
 * ML Strategy defaults + client-side validation (ML module).
 *
 * Single frontend source for the ML Strategy Builder form. The BACKEND is the
 * authoritative source: every value is re-validated independently by the
 * Plumber handlers — frontend checks are a convenience layer only.
 */

import { isValidTickerFormat, tickerFormatError } from './ticker'

export const ML_FORM_DEFAULTS = {
  ticker: 'AAPL',
  capital: '10000',
  start: '2023-01-01',
  end: '',
  model: 'random_forest',
  horizon: '5',
  buyThreshold: '2',
  sellThreshold: '2',
  featureGroups: [
    'returns',
    'trend',
    'rsi',
    'macd',
    'atr',
    'bollinger',
    'volatility',
    'momentum',
    'volume',
  ],
  topN: '5',
  rebalance: 'weekly',
  evaluateOn: 'test',
  tune: false,
}

/**
 * Validate the ML Strategy Builder form.
 *
 * @param {object} f Form values (ML_FORM_DEFAULTS shape).
 * @param {string} today Date string "YYYY-MM-DD" (injectable for tests).
 * @return {object} { errors: {field: message}, valid: boolean }
 */
export function validateMlForm(f, today = new Date().toISOString().slice(0, 10)) {
  const errors = {}
  const ticker = (f.ticker || '').trim().toUpperCase()

  if (!ticker) {
    errors.ticker = 'Ticker is required.'
  } else if (!isValidTickerFormat(ticker)) {
    errors.ticker = tickerFormatError()
  }

  if (!f.start) {
    errors.start = 'Start date is required.'
  }
  if (!f.end) {
    errors.end = 'End date is required.'
  }
  const parse = (v) => {
    if (!v) return null
    const d = new Date(`${v}T12:00:00Z`)
    return Number.isNaN(d.getTime()) ? null : d
  }
  const start = parse(f.start)
  const end = parse(f.end)
  const todayDate = parse(today)
  if (start && end && start > end) {
    errors.start = 'Start date must be on or before the end date.'
  }
  if (end && todayDate && end > todayDate) {
    errors.end = 'End date cannot be in the future.'
  }
  // ML models need training AND evaluation windows; reject very short ranges.
  if (start && end && (end - start) / 86400000 < 365) {
    errors.start = 'ML training needs at least one year of history (features warm-up + train/validation/test splits).'
  }

  if (f.capital === '' || f.capital === null || f.capital === undefined) {
    errors.capital = 'Initial capital is required.'
  } else {
    const c = Number(f.capital)
    if (!Number.isFinite(c) || c <= 0) {
      errors.capital = 'Initial capital must be a number greater than zero.'
    }
  }

  const horizon = Number(f.horizon)
  if (!Number.isInteger(horizon) || horizon < 1 || horizon > 60) {
    errors.horizon = 'Prediction horizon must be an integer between 1 and 60 trading days.'
  }

  const buy = Number(f.buyThreshold)
  const sell = Number(f.sellThreshold)
  if (!Number.isFinite(buy) || buy <= 0 || buy >= 100) {
    errors.buyThreshold = 'BUY threshold must be a percentage between 0 and 100.'
  }
  if (!Number.isFinite(sell) || sell <= 0 || sell >= 100) {
    errors.sellThreshold = 'SELL threshold must be a percentage between 0 and 100.'
  }

  if (!Array.isArray(f.featureGroups) || f.featureGroups.length === 0) {
    errors.featureGroups = 'Select at least one feature group.'
  }

  const topN = Number(f.topN)
  if (!Number.isInteger(topN) || topN < 1 || topN > 10) {
    errors.topN = 'Top-N must be an integer between 1 and 10.'
  }

  if (!['daily', 'weekly', 'monthly'].includes(f.rebalance)) {
    errors.rebalance = 'Rebalancing must be daily, weekly or monthly.'
  }

  return { errors, valid: Object.keys(errors).length === 0 }
}

/**
 * Build the POST /ml/train payload from the form.
 * Thresholds are sent as decimals (2 -> 0.02), per the API contract.
 */
export function buildMlTrainPayload(f) {
  return {
    model: f.model,
    ticker: (f.ticker || '').trim().toUpperCase(),
    start: f.start,
    end: f.end,
    horizon: Number(f.horizon),
    buy_threshold: Number(f.buyThreshold) / 100,
    sell_threshold: Number(f.sellThreshold) / 100,
    feature_groups: f.featureGroups,
    evaluate_on: f.evaluateOn,
    tune: Boolean(f.tune),
    initialCapital: Number(f.capital),
  }
}

/** Build the POST /ml/compare payload (all 12 models unless `models` set). */
export function buildMlComparePayload(f, models = null) {
  const payload = buildMlTrainPayload(f)
  if (models && models.length > 0) {
    payload.models = models
  } else {
    delete payload.models
  }
  return payload
}

/** Build the POST /ml/backtest payload (Top-N portfolio). */
export function buildMlBacktestPayload(f, tickers) {
  return {
    ...buildMlTrainPayload(f),
    tickers: (tickers || []).map((t) => t.trim().toUpperCase()).filter(Boolean),
    top_n: Number(f.topN),
    rebalance: f.rebalance,
  }
}
