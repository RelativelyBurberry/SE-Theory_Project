import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { AuthProvider } from '../context/AuthContext'
import RegisterPage from '../pages/RegisterPage'

// Mock the auth service so tests never hit the network.
vi.mock('../services/authService', () => ({
  login: vi.fn(),
  register: vi.fn(),
  getMe: vi.fn(),
}))
import * as authService from '../services/authService'

function renderRegister() {
  return render(
    <MemoryRouter initialEntries={['/register']}>
      <AuthProvider>
        <RegisterPage />
      </AuthProvider>
    </MemoryRouter>,
  )
}

describe('RegisterPage', () => {
  beforeEach(() => {
    localStorage.clear()
    vi.clearAllMocks()
  })

  it('renders the registration form with email, password, confirm and login link', () => {
    renderRegister()
    expect(screen.getByRole('heading', { name: /create account/i })).toBeInTheDocument()
    expect(screen.getByLabelText(/email/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/^password/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/confirm password/i)).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /create account/i })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: /sign in/i })).toBeInTheDocument()
  })

  it('shows validation errors for short and mismatched passwords', async () => {
    const user = userEvent.setup()
    renderRegister()
    await user.type(screen.getByLabelText(/email/i), 'user@example.com')
    await user.type(screen.getByLabelText(/^password/i), 'short')
    await user.type(screen.getByLabelText(/confirm password/i), 'different')
    await user.click(screen.getByRole('button', { name: /create account/i }))
    expect(await screen.findByText(/password must be at least 8 characters/i)).toBeInTheDocument()
    expect(screen.getByText(/passwords do not match/i)).toBeInTheDocument()
    expect(authService.register).not.toHaveBeenCalled()
  })

  it('sends only email and password (never the confirmation) to the backend', async () => {
    authService.register.mockResolvedValue({
      success: true,
      message: 'Account created successfully.',
      user: { id: 7, email: 'user@example.com', created_at: '2026-01-01T00:00:00Z' },
    })
    const user = userEvent.setup()
    renderRegister()
    await user.type(screen.getByLabelText(/email/i), 'user@example.com')
    await user.type(screen.getByLabelText(/^password/i), 'password123')
    await user.type(screen.getByLabelText(/confirm password/i), 'password123')
    await user.click(screen.getByRole('button', { name: /create account/i }))
    // The backend receives exactly { email, password } — the confirmation
    // field never leaves the browser.
    expect(authService.register).toHaveBeenCalledTimes(1)
    expect(authService.register).toHaveBeenCalledWith('user@example.com', 'password123')
    expect(authService.register.mock.calls[0]).toHaveLength(2)
  })

  it('displays a duplicate-account error for a 409 response', async () => {
    authService.register.mockRejectedValue({
      response: {
        status: 409,
        data: { success: false, error: 'An account with this email already exists.' },
      },
    })
    const user = userEvent.setup()
    renderRegister()
    await user.type(screen.getByLabelText(/email/i), 'taken@example.com')
    await user.type(screen.getByLabelText(/^password/i), 'password123')
    await user.type(screen.getByLabelText(/confirm password/i), 'password123')
    await user.click(screen.getByRole('button', { name: /create account/i }))
    expect(await screen.findByText(/already exists/i)).toBeInTheDocument()
  })
})