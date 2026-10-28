/**
 * AlphaForge Insight — Live News Service (Phase H.4)
 *
 * Fetches recent financial news from Marketaux via backend API.
 * - Headlines, URLs, publishers, timestamps, ticker relevance
 * - Sentiment when available (field may be absent)
 * - Handles loading, empty, error, rate-limit states
 */

import apiClient from './apiClient'

/**
 * Fetch recent live news for specified tickers.
 *
 * @param {string[]} tickers - Array of ticker symbols (e.g., ['AAPL', 'MSFT'])
 * @param {number} limit - Maximum articles to return (default 3, max 3 on free tier)
 * @returns {Promise<Object>} News response with articles array
 * @throws {Error} On network failure or API error
 */
export async function getLiveNews(tickers, limit = 3) {
  if (!tickers || tickers.length === 0) {
    throw new Error('At least one ticker is required')
  }

  // Join tickers as comma-separated string for query parameter
  const tickersParam = tickers.join(',')

  const response = await apiClient.get('/news/live', {
    params: {
      tickers: tickersParam,
      limit: Math.max(1, Math.min(limit, 3)),
    },
  })

  return response.data
}

/**
 * Check if news data is stale (older than cache TTL).
 *
 * @param {string} retrievalTime - ISO 8601 timestamp from API
 * @param {number} maxAgeMinutes - Maximum age in minutes (default 15)
 * @returns {boolean} True if data is stale
 */
export function isNewsStale(retrievalTime, maxAgeMinutes = 15) {
  if (!retrievalTime) return true

  try {
    const retrieved = new Date(retrievalTime)
    const now = new Date()
    const ageMinutes = (now - retrieved) / (1000 * 60)
    return ageMinutes > maxAgeMinutes
  } catch {
    return true
  }
}

/**
 * Format relative time (e.g., "2 hours ago") for display.
 *
 * @param {string} timestamp - ISO 8601 timestamp
 * @returns {string} Human-readable relative time
 */
export function formatRelativeTime(timestamp) {
  if (!timestamp) return 'Unknown'

  try {
    const date = new Date(timestamp)
    const now = new Date()
    const diffMs = now - date
    const diffMins = Math.floor(diffMs / (1000 * 60))
    const diffHours = Math.floor(diffMs / (1000 * 60 * 60))
    const diffDays = Math.floor(diffMs / (1000 * 60 * 60 * 24))

    if (diffMins < 1) return 'Just now'
    if (diffMins < 60) return `${diffMins} minute${diffMins === 1 ? '' : 's'} ago`
    if (diffHours < 24) return `${diffHours} hour${diffHours === 1 ? '' : 's'} ago`
    if (diffDays < 7) return `${diffDays} day${diffDays === 1 ? '' : 's'} ago`

    return date.toLocaleDateString()
  } catch {
    return 'Unknown'
  }
}

/**
 * Get sentiment label and color for display.
 *
 * @param {number|null} sentiment - Sentiment score (-1 to 1, or null)
 * @returns {Object} {label, color} for display
 */
export function getSentimentDisplay(sentiment) {
  if (sentiment === null || sentiment === undefined) {
    return { label: null, color: null }
  }

  const score = Number(sentiment)

  if (isNaN(score)) {
    return { label: null, color: null }
  }

  if (score >= 0.3) {
    return { label: 'Positive', color: 'text-emerald-600 bg-emerald-50' }
  }

  if (score <= -0.3) {
    return { label: 'Negative', color: 'text-red-600 bg-red-50' }
  }

  return { label: 'Neutral', color: 'text-slate-600 bg-slate-100' }
}
