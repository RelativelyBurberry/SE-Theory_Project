import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
// Importing apiClient creates the shared axios instance with its auth
// interceptors registered — exactly the production wiring.
import { apiClient } from '../services/apiClient'
// Real services wired to the SAME shared apiClient instance. These are the
// exact calls the Results page (GET /backtest/:id) and Dashboard (history)
// make on every load and hard refresh.
import { getBacktest, getBacktestHistory } from '../services/backtestService'

function firstHandler(manager, errorIndex = 0) {
  // Axios >= 1.x stores registered interceptors in `handlers` (a Mutex).
  const handlers = manager.handlers
  expect(handlers).toBeDefined()
  expect(handlers.length).toBeGreaterThan(0)
  const entry = handlers[0]
  // Support both entry shapes: [fulfilled, rejected] tuples (old) and
  // { fulfilled, rejected } objects (axios 1.x).
  if (typeof entry === 'function') return entry
  return errorIndex ? entry.rejected : entry.fulfilled
}

describe('apiClient auth contract (real axios instance)', () => {
  beforeEach(() => {
    localStorage.clear()
  })

  it('registers exactly one request and one response interceptor', () => {
    expect(apiClient.interceptors.request.handlers.length).toBe(1)
    expect(apiClient.interceptors.response.handlers.length).toBe(1)
  })

  it('uses the documented 10s global timeout (long ML routes override per-request)', () => {
    expect(apiClient.defaults.timeout).toBe(10000)
  })

  it('attaches Authorization: Bearer <jwt> from the shared token key when signed in', () => {
    localStorage.setItem('token', 'my.jwt.token')
    const fulfill = firstHandler(apiClient.interceptors.request)
    const out = fulfill({ headers: {} })
    expect(out.headers.Authorization).toBe('Bearer my.jwt.token')
  })

  it('sends no Authorization header when not signed in', () => {
    const fulfill = firstHandler(apiClient.interceptors.request)
    const out = fulfill({ headers: {} })
    expect(out.headers.Authorization).toBeUndefined()
  })

  it('clears the stored session and notifies listeners on 401 from a protected endpoint', async () => {
    localStorage.setItem('token', 'stale.jwt.token')
    localStorage.setItem('user', JSON.stringify({ id: 1, email: 'x@y.z' }))
    const loggedOut = vi.fn()
    window.addEventListener('auth:logged-out', loggedOut)

    const reject = firstHandler(apiClient.interceptors.response, 1)
    const error = {
      response: { status: 401, data: { error: 'Authentication required.' } },
      config: { url: '/backtest/43' },
    }
    await expect(reject(error)).rejects.toBe(error)

    expect(localStorage.getItem('token')).toBeNull()
    expect(localStorage.getItem('user')).toBeNull()
    expect(loggedOut).toHaveBeenCalled()
    window.removeEventListener('auth:logged-out', loggedOut)
  })

  it('does not treat login/register 401s as session expiry', async () => {
    localStorage.setItem('token', 'still-valid.jwt.token')
    const reject = firstHandler(apiClient.interceptors.response, 1)
    const error = {
      response: { status: 401, data: { error: 'Invalid email or password.' } },
      config: { url: '/auth/login' },
    }
    // Still rejected so the login form can render the error inline,
    // but the stored session must survive.
    await expect(reject(error)).rejects.toBe(error)
    expect(localStorage.getItem('token')).toBe('still-valid.jwt.token')
  })
})

describe('persisted-backtest fetches carry the JWT end-to-end (service -> interceptor -> wire)', () => {
  const captured = []

  beforeEach(() => {
    localStorage.clear()
    localStorage.setItem('token', 'persistence.jwt.token')
    captured.length = 0
    // Route real requests through a recording adapter instead of the network.
    apiClient.defaults.adapter = async (config) => {
      captured.push(config)
      return {
        data: {},
        status: 200,
        statusText: 'OK',
        headers: {},
        config,
      }
    }
  })

  afterEach(() => {
    // Restore axios's default (XHR) adapter: tests above this block rely on
    // the plain 10s-timeout instance, and later suites must not inherit it.
    delete apiClient.defaults.adapter
  })

  function authHeader(config) {
    const direct = config.headers?.Authorization
    if (direct) return direct
    return config.headers?.get?.('Authorization') ?? undefined
  }

  it('GET /backtest/43 (Results page) is sent with Authorization: Bearer <jwt>', async () => {
    await getBacktest(43)

    expect(captured).toHaveLength(1)
    expect(captured[0].url).toBe('/backtest/43')
    expect(captured[0].method).toMatch(/^get$/i)
    expect(authHeader(captured[0])).toBe('Bearer persistence.jwt.token')
  })

  it('GET /backtest/history (Dashboard) is sent with Authorization: Bearer <jwt>', async () => {
    await getBacktestHistory(20, 0)

    expect(captured).toHaveLength(1)
    expect(captured[0].url).toBe('/backtest/history')
    expect(captured[0].method).toMatch(/^get$/i)
    expect(authHeader(captured[0])).toBe('Bearer persistence.jwt.token')
    // Pagination params must be exactly what the service builds.
    expect(captured[0].params).toEqual({ limit: 20, offset: 0 })
  })

  it('persisted-record and history fetches succeed after a hard refresh (token survives from localStorage)', async () => {
    // Simulates F5 at /results/43 and /dashboard: the ONLY state is the
    // token that AuthContext persisted to localStorage; the page modules
    // construct these requests fresh through the shared apiClient.
    const [record, history] = await Promise.all([
      getBacktest(43),
      getBacktestHistory(10, 0),
    ])

    expect(record).toEqual({})
    expect(history).toEqual({})
    expect(captured.map((c) => c.url).sort()).toEqual([
      '/backtest/43',
      '/backtest/history',
    ])
    expect(
      captured.every((c) => authHeader(c) === 'Bearer persistence.jwt.token'),
    ).toBe(true)
  })
})
