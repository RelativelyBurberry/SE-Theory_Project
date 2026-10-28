import { createContext, useContext, useCallback, useEffect, useState } from 'react'
import * as authService from '../services/authService'

/**
 * AuthContext — global authentication state (Phase 2).
 *
 * Provides: { user, token, login, register, logout, loading, isAuthenticated }
 *
 * Token storage strategy (student project decision):
 *   The JWT and the current user snapshot are stored in localStorage.
 *   Tradeoff: localStorage survives page reloads, which keeps the user signed
 *   in across refreshes, but it is readable by any script running on the page
 *   (XSS exposure). This is acceptable for a development/student project;
 *   a production build would use httpOnly cookies and short-lived tokens.
 *   No production-grade security is claimed.
 */
const AuthContext = createContext(null)

const TOKEN_KEY = 'token'
const USER_KEY = 'user'

function readStoredUser() {
  try {
    const raw = localStorage.getItem(USER_KEY)
    return raw ? JSON.parse(raw) : null
  } catch {
    return null
  }
}

export function AuthProvider({ children }) {
  const [user, setUser] = useState(readStoredUser)
  const [token, setToken] = useState(() => localStorage.getItem(TOKEN_KEY))
  const [loading, setLoading] = useState(false)

  const persist = useCallback((nextToken, nextUser) => {
    if (nextToken && nextUser) {
      localStorage.setItem(TOKEN_KEY, nextToken)
      localStorage.setItem(USER_KEY, JSON.stringify(nextUser))
    } else {
      localStorage.removeItem(TOKEN_KEY)
      localStorage.removeItem(USER_KEY)
    }
  }, [])

  // Keep localStorage in sync with auth state. This covers every path that
  // mutates token/user (login, logout and the 401 'auth:logged-out' event).
  useEffect(() => {
    persist(token, user)
  }, [token, user, persist])

  /**
   * Login: calls POST /auth/login, stores the JWT + user, and updates state.
   * Re-throws API errors so the LoginPage can display them.
   */
  const login = useCallback(
    async (email, password) => {
      setLoading(true)
      try {
        const data = await authService.login(email, password)
        setToken(data.token)
        setUser(data.user)
        persist(data.token, data.user)
        return data
      } finally {
        setLoading(false)
      }
    },
    [persist],
  )

  /**
   * Register: calls POST /auth/register. Does NOT auto-login; the
   * RegisterPage navigates the user to /login afterwards.
   * Re-throws API errors so the RegisterPage can display them.
   */
  const register = useCallback(async (email, password) => {
    setLoading(true)
    try {
      const data = await authService.register(email, password)
      return data
    } finally {
      setLoading(false)
    }
  }, [])

  /**
   * Logout: clears auth state (localStorage tokens and user).
   */
  const logout = useCallback(() => {
    setToken(null)
    setUser(null)
    persist(null, null)
  }, [persist])

  // Keep state in sync when the 401 interceptor clears the token
  // (e.g. an expired token was rejected by a protected endpoint).
  useEffect(() => {
    const handleLoggedOut = () => {
      setToken(null)
      setUser(null)
    }
    window.addEventListener('auth:logged-out', handleLoggedOut)
    return () => window.removeEventListener('auth:logged-out', handleLoggedOut)
  }, [])

  // Boot-time session validation. A JWT persisted from a previous session can
  // be expired or revoked, yet the navbar would keep showing the user while
  // every API call is rejected with 401. Ask the backend once whether the
  // stored token is still valid: a definitive 401 clears the session so
  // PrivateRoute sends the user to /login instead of a wall of API errors.
  // A network failure (backend unreachable) must NOT log the user out, so
  // only a real HTTP 401 response is treated as "session invalid".
  useEffect(() => {
    if (!token) return undefined
    let cancelled = false
    Promise.resolve(authService.getMe?.()).catch((err) => {
        if (cancelled) return
        if (err?.response?.status === 401) {
          setToken(null)
          setUser(null)
        }
      })
    return () => {
      cancelled = true
    }
  }, [token])

  const isAuthenticated = Boolean(token && user)

  return (
    <AuthContext.Provider
      value={{ user, token, login, register, logout, loading, isAuthenticated }}
    >
      {children}
    </AuthContext.Provider>
  )
}

export function useAuth() {
  const ctx = useContext(AuthContext)
  if (!ctx) {
    throw new Error('useAuth must be used within an AuthProvider')
  }
  return ctx
}

export default AuthContext