import apiClient from './apiClient'

/**
 * Auth API service — wraps the backend auth endpoints using the single
 * centralized Axios client (src/services/apiClient.js).
 */

/**
 * POST /auth/register
 * Returns { success, message, user: { id, email, created_at } }
 */
export async function register(email, password) {
  const { data } = await apiClient.post('/auth/register', { email, password })
  return data
}

/**
 * POST /auth/login
 * Returns { success, message, token, expiresIn, user: { id, email, created_at } }
 */
export async function login(email, password) {
  const { data } = await apiClient.post('/auth/login', { email, password })
  return data
}

/**
 * GET /auth/me — requires a valid JWT.
 * Returns { success, user: { id, email, created_at } }
 */
export async function getMe() {
  const { data } = await apiClient.get('/auth/me')
  return data
}

export default { register, login, getMe }