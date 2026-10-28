/**
 * Formatting utilities for the results dashboard (Phase 6).
 *
 * Number/percent/currency formatters for metrics cards, equity curve axis
 * labels, and the trade table.
 */

/**
 * Format a number with thousands separators and fixed decimals.
 * @param {number|null|undefined} value The number to format.
 * @param {number} decimals Decimal places (default 2).
 * @return {string} Formatted string, or "—" if value is empty/not finite.
 */
export function formatNumber(value, decimals = 2) {
  if (value === null || value === undefined) return '—'
  const n = Number(value)
  if (!Number.isFinite(n)) return '—'
  return n.toLocaleString('en-US', {
    minimumFractionDigits: decimals,
    maximumFractionDigits: decimals,
  })
}

/**
 * Format a decimal as a percentage with sign.
 * @param {number|null|undefined} value Decimal value (e.g., 0.1523 for 15.23%).
 * @param {number} decimals Decimal places (default 2).
 * @return {string} Formatted string like "+15.23%" or "—".
 */
export function formatPercent(value, decimals = 2) {
  if (value === null || value === undefined) return '—'
  const n = Number(value)
  if (!Number.isFinite(n)) return '—'
  const pct = n * 100
  const sign = pct > 0 ? '+' : ''
  return `${sign}${pct.toFixed(decimals)}%`
}

/**
 * Format a currency value with $ sign and thousands separators.
 * @param {number|null|undefined} value The dollar amount.
 * @param {number} decimals Decimal places (default 2).
 * @return {string} Formatted string like "$10,000.00" or "—".
 */
export function formatCurrency(value, decimals = 2) {
  if (value === null || value === undefined) return '—'
  const n = Number(value)
  if (!Number.isFinite(n)) return '—'
  return `$${formatNumber(n, decimals)}`
}

/**
 * Format a date string (YYYY-MM-DD) to a readable format.
 * @param {string|null|undefined} dateString ISO date string.
 * @return {string} Formatted date like "Jan 1, 2024" or "—".
 */
export function formatDate(dateString) {
  if (!dateString) return '—'
  try {
    const date = new Date(`${dateString}T12:00:00Z`)
    return date.toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      timeZone: 'UTC',
    })
  } catch {
    return dateString
  }
}

/**
 * Format a timestamp to a readable datetime.
 * @param {string|null|undefined} timestamp ISO timestamp string.
 * @return {string} Formatted datetime or "—".
 */
export function formatDateTime(timestamp) {
  if (!timestamp) return '—'
  try {
    const date = new Date(timestamp)
    return date.toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    })
  } catch {
    return timestamp
  }
}