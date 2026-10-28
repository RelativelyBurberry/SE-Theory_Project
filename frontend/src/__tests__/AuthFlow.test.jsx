import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, act, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { AuthProvider } from '../context/AuthContext'
import App from '../App'

// Mock services so tests never hit the network.
vi.mock('../services/authService', () => ({
  login: vi.fn(),
  register: vi.fn(),
  getMe: vi.fn(),
}))
vi.mock('../services/healthService', () => ({
  checkHealth: vi.fn().mockResolvedValue({ status: 'ok', message: 'API is running' }),
}))
import * as authService from '../services/authService'

const TEST_USER = { id: 1, email: 'user@example.com', created_at: '2026-01-01T00:00:00Z' }
const TEST_TOKEN = 'test.jwt.token'

function renderApp(initialPath = '/') {
  return render(
    <MemoryRouter initialEntries={[initialPath]}>
      <AuthProvider>
        <App />
      </AuthProvider>
    </MemoryRouter>,
  )
}

describe('Authentication flow (App level)', () => {
  beforeEach(() => {
    localStorage.clear()
    vi.clearAllMocks()
  })

  it('redirects an unauthenticated user away from the protected dashboard to /login', () => {
    renderApp('/dashboard')
    // The dashboard must NOT render; the login page must be shown instead.
    expect(
      screen.queryByRole('heading', { name: /welcome to alphaforge insight/i }),
    ).not.toBeInTheDocument()
    expect(screen.getByRole('heading', { name: /sign in/i })).toBeInTheDocument()
  })

  it('lets an authenticated user reach the protected dashboard', () => {
    localStorage.setItem('token', TEST_TOKEN)
    localStorage.setItem('user', JSON.stringify(TEST_USER))
    renderApp('/dashboard')
    expect(
      screen.getByRole('heading', { name: /welcome to alphaforge insight/i }),
    ).toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: /sign in/i })).not.toBeInTheDocument()
  })

  it('updates auth state after a successful login and exposes the dashboard', async () => {
    authService.login.mockResolvedValue({
      success: true,
      message: 'Login successful.',
      token: TEST_TOKEN,
      expiresIn: '86400s',
      user: TEST_USER,
    })
    const user = userEvent.setup()
    renderApp('/login')
    await user.type(screen.getByLabelText(/email/i), TEST_USER.email)
    await user.type(screen.getByLabelText(/password/i), 'password123')
    await user.click(screen.getByRole('button', { name: /sign in/i }))

    // After login the app navigates to /dashboard.
    expect(
      await screen.findByRole('heading', { name: /welcome to alphaforge insight/i }),
    ).toBeInTheDocument()
    // JWT is persisted and the header shows the signed-in user + Log out.
    expect(localStorage.getItem('token')).toBe(TEST_TOKEN)
    expect(JSON.parse(localStorage.getItem('user')).email).toBe(TEST_USER.email)
    expect(screen.getByText(TEST_USER.email)).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /log out/i })).toBeInTheDocument()
  })

  it('logout clears auth state and returns to /login', async () => {
    localStorage.setItem('token', TEST_TOKEN)
    localStorage.setItem('user', JSON.stringify(TEST_USER))
    const user = userEvent.setup()
    renderApp('/dashboard')
    expect(
      await screen.findByRole('heading', { name: /welcome to alphaforge insight/i }),
    ).toBeInTheDocument()

    await user.click(screen.getByRole('button', { name: /log out/i }))

    // Back on the login page with auth state cleared.
    expect(await screen.findByRole('heading', { name: /sign in/i })).toBeInTheDocument()
    expect(localStorage.getItem('token')).toBeNull()
    expect(localStorage.getItem('user')).toBeNull()
  })

  it('clears stored auth when a protected request returns 401', async () => {
    // Pre-populate a stale token; simulate a 401 from a protected endpoint.
    localStorage.setItem('token', TEST_TOKEN)
    localStorage.setItem('user', JSON.stringify(TEST_USER))
    renderApp('/dashboard')
    expect(
      await screen.findByRole('heading', { name: /welcome to alphaforge insight/i }),
    ).toBeInTheDocument()

    // Fire the same event the axios 401 interceptor dispatches.
    act(() => {
      window.dispatchEvent(new Event('auth:logged-out'))
    })

    // Auth state is cleared (in a real browser the interceptor then
    // redirects to /login).
    await waitFor(() => expect(localStorage.getItem('token')).toBeNull())
    await waitFor(() => expect(localStorage.getItem('user')).toBeNull())
  })

  it('validates the persisted token at boot via GET /auth/me', async () => {
    localStorage.setItem('token', TEST_TOKEN)
    localStorage.setItem('user', JSON.stringify(TEST_USER))
    authService.getMe.mockResolvedValue({ success: true, user: TEST_USER })
    renderApp('/dashboard')
    expect(
      await screen.findByRole('heading', { name: /welcome to alphaforge insight/i }),
    ).toBeInTheDocument()
    expect(authService.getMe).toHaveBeenCalled()
    // A still-valid token keeps the user signed in.
    expect(localStorage.getItem('token')).toBe(TEST_TOKEN)
  })

  it('clears a stale/expired token at boot so the user lands on /login', async () => {
    // The exact logged-in-but-401-everywhere state: navbar user snapshot in
    // localStorage, JWT expired. Boot validation must detect it once via
    // /auth/me instead of letting every API call fail with 401.
    localStorage.setItem('token', TEST_TOKEN)
    localStorage.setItem('user', JSON.stringify(TEST_USER))
    authService.getMe.mockRejectedValue({
      response: { status: 401, data: { success: false, error: 'Invalid or expired token.' } },
    })
    renderApp('/dashboard')
    // PrivateRoute now redirects to the login page.
    expect(
      await screen.findByRole('heading', { name: /sign in/i }),
    ).toBeInTheDocument()
    await waitFor(() => expect(localStorage.getItem('token')).toBeNull())
    await waitFor(() => expect(localStorage.getItem('user')).toBeNull())
  })

  it('does not log the user out when boot validation only hits a network failure', async () => {
    // Backend unreachable ≠ session invalid: without a real HTTP 401 the
    // stored session must survive.
    localStorage.setItem('token', TEST_TOKEN)
    localStorage.setItem('user', JSON.stringify(TEST_USER))
    authService.getMe.mockRejectedValue(new TypeError('Network Error'))
    renderApp('/dashboard')
    expect(
      await screen.findByRole('heading', { name: /welcome to alphaforge insight/i }),
    ).toBeInTheDocument()
    expect(localStorage.getItem('token')).toBe(TEST_TOKEN)
    expect(localStorage.getItem('user')).not.toBeNull()
  })
})