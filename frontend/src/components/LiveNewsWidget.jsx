import { useEffect, useState } from 'react'
import {
  getLiveNews,
  formatRelativeTime,
  getSentimentDisplay,
  isNewsStale,
} from '../services/newsService'

/**
 * AlphaForge Insight — Live News Component (Phase H.4)
 *
 * Displays recent financial news for specified tickers.
 * - Headlines, clickable article links, publisher, publication time
 * - Relevant ticker badges
 * - Sentiment labels when available
 * - Loading, empty, stale-data, rate-limit, and error states
 * - Ticker filtering with dropdown
 *
 * IMPORTANT: This displays recent news for informational purposes only.
 * Do not interpret sentiment as a price prediction. News does not
 * improve forecasting unless validated separately.
 */

const SUPPORTED_TICKERS = [
  { symbol: 'AAPL', name: 'Apple Inc.' },
  { symbol: 'MSFT', name: 'Microsoft Corporation' },
  { symbol: 'GOOGL', name: 'Alphabet Inc.' },
]

function TickerBadge({ ticker }) {
  return (
    <span className="inline-flex items-center rounded-md bg-blue-50 px-2 py-0.5 text-xs font-medium text-blue-700">
      {ticker}
    </span>
  )
}

function SentimentBadge({ sentiment }) {
  const { label, color } = getSentimentDisplay(sentiment)

  if (!label) return null

  return (
    <span className={`inline-flex items-center rounded-md px-2 py-0.5 text-xs font-medium ${color}`}>
      {label}
    </span>
  )
}


function ArticleCard({ article }) {
  const timeAgo = formatRelativeTime(article.published_at)

 const relatedTickers = Array.isArray(article.related_tickers)
  ? article.related_tickers
  : typeof article.related_tickers === "string"
    ? article.related_tickers.split(",").map((t) => t.trim()).filter(Boolean)
    : []

  return (
    <article className="rounded-lg border border-slate-200 bg-white p-4 shadow-sm transition-shadow hover:shadow-md">
      <div className="flex items-start justify-between gap-3">
        <div className="flex-1">
          <a
            href={article.url}
            target="_blank"
            rel="noopener noreferrer"
            className="text-sm font-medium text-slate-900 hover:text-blue-600 hover:underline line-clamp-2"
          >
            {article.headline}
          </a>

          <div className="mt-2 flex flex-wrap items-center gap-2">
            {relatedTickers.length > 0 && (
              <div className="flex flex-wrap gap-1">
                {relatedTickers.map((ticker) => (
                  <TickerBadge key={ticker} ticker={ticker} />
                ))}
              </div>
            )}

            {article.sentiment !== null &&
              article.sentiment !== undefined && (
                <SentimentBadge sentiment={article.sentiment} />
              )}
          </div>
        </div>
      </div>

      <div className="mt-3 flex items-center justify-between text-xs text-slate-500">
        <span className="truncate">{article.publisher}</span>
        <span className="ml-2 shrink-0">{timeAgo}</span>
      </div>

      {article.snippet && (
        <p className="mt-2 text-xs text-slate-600 line-clamp-2">
          {article.snippet}
        </p>
      )}
    </article>
  )
}


function LoadingState() {
  return (
    <div className="space-y-3" data-testid="news-loading">
      {[1, 2, 3].map((i) => (
        <div key={i} className="animate-pulse rounded-lg border border-slate-200 bg-white p-4">
          <div className="h-4 w-3/4 rounded bg-slate-200"></div>
          <div className="mt-3 h-3 w-1/2 rounded bg-slate-200"></div>
          <div className="mt-2 h-3 w-1/4 rounded bg-slate-200"></div>
        </div>
      ))}
    </div>
  )
}

function EmptyState() {
  return (
    <div className="rounded-lg border border-slate-200 bg-white p-6 text-center" data-testid="news-empty">
      <p className="text-sm text-slate-500">
        No recent news available for the selected tickers.
      </p>
      <p className="mt-1 text-xs text-slate-400">
        News may be temporarily unavailable outside market hours.
      </p>
    </div>
  )
}

function ErrorState({ error, onRetry }) {
  return (
    <div className="rounded-lg border border-red-200 bg-red-50 p-6 text-center" data-testid="news-error">
      <p className="text-sm font-medium text-red-800">
        Unable to load news
      </p>
      <p className="mt-1 text-xs text-red-600">
        {error || 'An unexpected error occurred. Please try again later.'}
      </p>
      {onRetry && (
        <button
          onClick={onRetry}
          className="mt-3 rounded-md bg-red-100 px-3 py-1.5 text-xs font-medium text-red-700 hover:bg-red-200"
        >
          Retry
        </button>
      )}
    </div>
  )
}

function StaleDataWarning({ retrievalTime }) {
  if (!isNewsStale(retrievalTime, 30)) return null

  return (
    <div className="rounded-md border border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-700" data-testid="news-stale-warning">
      ⚠️ Data may be outdated (last updated: {formatRelativeTime(retrievalTime)})
    </div>
  )
}

export default function LiveNewsWidget({ className = '' }) {
  const [selectedTickers, setSelectedTickers] = useState(['AAPL', 'MSFT', 'GOOGL'])
  const [articles, setArticles] = useState([])
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState(null)
  const [retrievalTime, setRetrievalTime] = useState(null)

  const fetchNews = async () => {
    setLoading(true)
    setError(null)

    try {
      const data = await getLiveNews(selectedTickers, 3)
      setArticles(data.articles || [])
      setRetrievalTime(data.retrieval_time)
    } catch (err) {
      const errorMessage = err.response?.data?.error || err.message || 'Failed to fetch news'
      setError(errorMessage)
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    fetchNews()
    // Refresh every 15 minutes
    const interval = setInterval(fetchNews, 15 * 60 * 1000)
    return () => clearInterval(interval)
  }, [selectedTickers.join(',')])

  const handleTickerToggle = (symbol) => {
    setSelectedTickers((prev) => {
      if (prev.includes(symbol)) {
        // Don't allow deselecting all tickers
        if (prev.length === 1) return prev
        return prev.filter((t) => t !== symbol)
      }
      return [...prev, symbol]
    })
  }

  return (
    <div className={`space-y-4 ${className}`}>
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-lg font-semibold text-slate-900">Recent News</h2>
          <p className="mt-0.5 text-xs text-slate-500">
            Live headlines from Marketaux • For informational purposes only
          </p>
        </div>
        <button
          onClick={fetchNews}
          disabled={loading}
          className="rounded-md border border-slate-200 bg-white px-3 py-1.5 text-xs font-medium text-slate-600 hover:bg-slate-50 disabled:opacity-50"
          aria-label="Refresh news"
        >
          {loading ? 'Loading...' : 'Refresh'}
        </button>
      </div>

      {/* Ticker Filter */}
      <div className="flex flex-wrap gap-2" role="group" aria-label="Filter by ticker">
        {SUPPORTED_TICKERS.map((ticker) => {
          const isSelected = selectedTickers.includes(ticker.symbol)
          return (
            <button
              key={ticker.symbol}
              onClick={() => handleTickerToggle(ticker.symbol)}
              className={`rounded-md px-3 py-1.5 text-xs font-medium transition-colors ${
                isSelected
                  ? 'bg-emerald-100 text-emerald-700 ring-1 ring-emerald-600'
                  : 'bg-slate-100 text-slate-600 hover:bg-slate-200'
              }`}
              aria-pressed={isSelected}
            >
              {ticker.symbol}
            </button>
          )
        })}
      </div>

      {/* Stale Data Warning */}
      {retrievalTime && <StaleDataWarning retrievalTime={retrievalTime} />}

      {/* Content States */}
      {loading ? (
        <LoadingState />
      ) : error ? (
        <ErrorState error={error} onRetry={fetchNews} />
      ) : articles.length === 0 ? (
        <EmptyState />
      ) : (
        <div className="space-y-3" data-testid="news-articles">
          {articles.map((article, index) => (
            <ArticleCard key={`${article.url}-${index}`} article={article} />
          ))}
        </div>
      )}

      {/* Attribution */}
      <p className="text-xs text-slate-400">
        News provided by Marketaux. Sentiment labels, when shown, are provided by the API 
        and do not constitute investment advice. This feature does not improve forecasting 
        accuracy unless validated separately.
      </p>
    </div>
  )
}
