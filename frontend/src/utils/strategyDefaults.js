/**
 * Strategy defaults and client-side validation helpers (Phase 4).
 *
 * Single frontend source for the documented Review 1 strategy parameters
 * (IMPLEMENTATION_PLAN.md § I). The BACKEND is the authoritative source:
 * it exposes GET /strategies/defaults and independently validates every
 * request (frontend checks are a convenience layer, never a substitute).
 *
 * Defaults (documented):
 *   MA_Crossover: fast_period = 50, slow_period = 200
 *   RSI: period = 14, oversold = 30, overbought = 70
 */

import { isValidTickerFormat, tickerFormatError } from './ticker'

export const STRATEGY_DEFAULTS = {
  MA_Crossover: {
    fast_period: 50,
    slow_period: 200,
  },
  RSI: {
    period: 14,
    oversold: 30,
    overbought: 70,
  },
}

export const STRATEGY_OPTIONS = [
  { value: 'MA_Crossover', label: 'Moving Average Crossover' },
  { value: 'RSI', label: 'Relative Strength Index (RSI)' },
]

/** True when a period is a whole number >= 1. */
function isPositiveInt(value) {
  const n = Number(value)
  return Number.isFinite(n) && Number.isInteger(n) && n >= 1
}

/** True when value is a finite number strictly greater than 0. */
function isPositiveNumber(value) {
  const n = Number(value)
  return Number.isFinite(n) && n > 0
}

/** Normalize a date string "YYYY-MM-DD" to a comparable Date (noon UTC). */
function parseDate(value) {
  if (!value) return null
  const d = new Date(`${value}T12:00:00Z`)
  return Number.isNaN(d.getTime()) ? null : d
}

/**
 * Validate the whole Strategy Builder form.
 *
 * @param {object} f Form values: { ticker, capital, start, end,
 *   strategy, fastPeriod, slowPeriod, rsiPeriod, oversold, overbought }
 * @param {string} today Date string "YYYY-MM-DD" (injectable for tests).
 * @return {object} { errors: {field: message}, valid: boolean }
 */
export function validateStrategyForm(f, today = new Date().toISOString().slice(0, 10)) {
  const errors = {}
  const ticker = (f.ticker || '').trim().toUpperCase()

  // Ticker: non-empty + basic format (mirrors the backend TICKER_REGEX).
  // Shared rule lives in utils/ticker.js (accepts RELIANCE.NS etc.).
  if (!ticker) {
    errors.ticker = 'Ticker is required.'
  } else if (!isValidTickerFormat(ticker)) {
    errors.ticker = tickerFormatError()
  }

  // Dates
  const start = parseDate(f.start)
  const end = parseDate(f.end)
  const todayDate = parseDate(today)
  if (!start) errors.start = 'Start date is required.'
  if (!end) errors.end = 'End date is required.'
  if (start && end && start > end) {
    errors.start = 'Start date must be on or before the end date.'
  }
  if (start && todayDate && start > todayDate) {
    errors.start = 'Start date cannot be in the future.'
  }
  if (end && todayDate && end > todayDate) {
    errors.end = 'End date cannot be in the future.'
  }

  // Initial capital
  if (f.capital === '' || f.capital === null || f.capital === undefined) {
    errors.capital = 'Initial capital is required.'
  } else if (!isPositiveNumber(f.capital)) {
    errors.capital = 'Initial capital must be a number greater than zero.'
  }

  // Strategy-specific parameters
  if (f.strategy === 'MA_Crossover') {
    if (!isPositiveInt(f.fastPeriod)) {
      errors.fastPeriod = 'Fast period must be a positive integer.'
    }
    if (!isPositiveInt(f.slowPeriod)) {
      errors.slowPeriod = 'Slow period must be a positive integer.'
    }
    if (
      isPositiveInt(f.fastPeriod) &&
      isPositiveInt(f.slowPeriod) &&
      Number(f.fastPeriod) >= Number(f.slowPeriod)
    ) {
      errors.fastPeriod =
        'Fast period must be strictly less than the slow period.'
    }
  } else if (f.strategy === 'RSI') {
    if (!isPositiveInt(f.rsiPeriod)) {
      errors.rsiPeriod = 'RSI period must be a positive integer.'
    }
    const os = Number(f.oversold)
    const ob = Number(f.overbought)
    if (!isPositiveNumber(f.oversold) || os >= 100) {
      errors.oversold = 'Oversold must be a number between 0 and 100.'
    }
    if (!isPositiveNumber(f.overbought) || ob >= 100) {
      errors.overbought = 'Overbought must be a number between 0 and 100.'
    }
    if (
      isPositiveNumber(f.oversold) &&
      os < 100 &&
      isPositiveNumber(f.overbought) &&
      ob < 100 &&
      os >= ob
    ) {
      errors.oversold =
        'Oversold must be strictly less than the overbought threshold.'
    }
  }

  return { errors, valid: Object.keys(errors).length === 0 }
}

/**
 * Build the API payload for POST /strategies/preview.
 * Parameter names use the snake_case contract documented in the backend.
 */
export function buildStrategyPayload(f) {
  const parameters =
    f.strategy === 'MA_Crossover'
      ? { fast_period: Number(f.fastPeriod), slow_period: Number(f.slowPeriod) }
      : {
          period: Number(f.rsiPeriod),
          oversold: Number(f.oversold),
          overbought: Number(f.overbought),
        }

  return {
    ticker: (f.ticker || '').trim().toUpperCase(),
    start: f.start,
    end: f.end,
    initialCapital: Number(f.capital),
    strategy: f.strategy,
    parameters,
  }
}

export default STRATEGY_DEFAULTS