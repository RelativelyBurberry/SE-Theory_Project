import apiClient from './apiClient'

/**
 * GET /health — backend liveness check.
 *
 * Returns the parsed JSON body on success and rejects with the Axios error on
 * failure so callers can surface a user-friendly message.
 */
export async function checkHealth() {
  const { data } = await apiClient.get('/health')
  return data
}

export default { checkHealth }