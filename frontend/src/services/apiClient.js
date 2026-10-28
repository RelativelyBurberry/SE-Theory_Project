import axios from 'axios'

/**
 * Centralized Axios client for the AlphaForge Insight backend.
 *
 * The base URL is read from VITE_API_URL (see frontend/.env.example) and
 * falls back to the local Plumber default when the variable is unset.
 *
 * Interceptors (Phase 2):
 *  - Request: attach the JWT from localStorage as the Authorization header.
 *  - Response: on 401 (except from the auth endpoints themselves), clear the
 *    stored token and redirect to /login.
 */
const API_BASE_URL = import.meta.env.VITE_API_URL ?? 'http://localhost:8000'

export const apiClient = axios.create({
  baseURL: API_BASE_URL,
  timeout: 10000, // 10s
  headers: { 'Content-Type': 'application/json' },
})

// --- Request interceptor: attach JWT --------------------------------------
apiClient.interceptors.request.use((config) => {
  const token = localStorage.getItem('token')
  if (token) {
    config.headers.Authorization = `Bearer ${token}`
  }
  return config
})

// --- Response interceptor: handle expired/invalid tokens -------------------
apiClient.interceptors.response.use(
  (response) => response,
  (error) => {
    const status = error?.response?.status
    const url = error?.config?.url ?? ''
    // A 401 from /auth/login or /auth/register is an expected credential
    // error (rendered inline by the form), NOT a session-expiry redirect.
    const isAuthEndpoint = url.includes('/auth/login') || url.includes('/auth/register')
    if (status === 401 && !isAuthEndpoint) {
      localStorage.removeItem('token')
      localStorage.removeItem('user')
      // Notify any listeners (AuthContext keeps its state in sync).
      window.dispatchEvent(new Event('auth:logged-out'))
      if (!window.location.pathname.startsWith('/login')) {
        window.location.href = '/login'
      }
    }
    return Promise.reject(error)
  },
)

export default apiClient