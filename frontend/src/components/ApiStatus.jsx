import { useCallback, useEffect, useState } from 'react'
import { checkHealth } from '../services/healthService'
import apiClient from '../services/apiClient'

/**
 * Backend reachability indicator.
 *
 * Fetches GET /health on mount and renders one of three states:
 *   - checking : request in flight (loading)
 *   - online   : backend responded successfully
 *   - offline  : request failed (network error, non-2xx, timeout)
 *
 * A "Retry" button re-runs the check. This component is the Phase 1
 * frontend <-> backend connectivity integration test.
 */
export default function ApiStatus() {
  const [status, setStatus] = useState('checking') // 'checking' | 'online' | 'offline'
  const [detail, setDetail] = useState('')

  const runCheck = useCallback(async () => {
    setStatus('checking')
    setDetail('')
    try {
      const data = await checkHealth()
      setStatus('online')
      setDetail(data?.message ?? 'API is running')
    } catch (error) {
      setStatus('offline')
      const hint =
        error?.response?.data?.error ??
        error?.message ??
        'Backend unreachable'
      setDetail(hint)
    }
  }, [])

  useEffect(() => {
    runCheck()
  }, [runCheck])

  const baseUrl = apiClient.defaults.baseURL

  const styles = {
    checking: 'border-amber-300 bg-amber-50 text-amber-800',
    online: 'border-emerald-300 bg-emerald-50 text-emerald-800',
    offline: 'border-rose-300 bg-rose-50 text-rose-800',
  }

  const label = {
    checking: 'Checking backend connection…',
    online: `Backend online: ${detail}`,
    offline: `Backend offline: ${detail}`,
  }[status]

  return (
    <div
      role="status"
      aria-live="polite"
      className={`flex items-center justify-between rounded-md border px-3 py-2 text-sm ${styles[status]}`}
    >
      <span>
        {status === 'checking' && (
          <span className="mr-2 inline-block h-3 w-3 animate-spin rounded-full border-2 border-amber-500 border-t-transparent align-middle" />
        )}
        {label}
        {status === 'offline' && (
          <span className="ml-2 opacity-60">({baseUrl})</span>
        )}
      </span>
      {status === 'offline' && (
        <button
          type="button"
          onClick={runCheck}
          className="rounded border border-rose-300 bg-white px-2 py-1 text-xs font-medium text-rose-700 hover:bg-rose-100"
        >
          Retry
        </button>
      )}
    </div>
  )
}