import { useState } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { useAuth } from '../context/AuthContext'
import { Navigate } from 'react-router-dom'

/**
 * Register — Phase 2.
 *
 * Email/password/confirm form with inline validation, a loading state,
 * duplicate-account handling (409 from the API), general API error display,
 * and a link to the Login page.
 *
 * The password confirmation field is validated client-side only and is NEVER
 * sent to the backend — only { email, password } is posted to /auth/register.
 */
export default function RegisterPage() {
  const { register, isAuthenticated } = useAuth()
  const navigate = useNavigate()

  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [confirmPassword, setConfirmPassword] = useState('')
  const [errors, setErrors] = useState({})
  const [apiError, setApiError] = useState('')
  const [submitting, setSubmitting] = useState(false)

  // Already signed in -> straight to the dashboard.
  if (isAuthenticated) {
    return <Navigate to="/dashboard" replace />
  }

  function validate() {
    const next = {}
    if (!email.trim()) {
      next.email = 'Email is required.'
    } else if (!/^[^@\s]+@[^@\s]+\.[^@\s]+$/.test(email.trim())) {
      next.email = 'Please enter a valid email address.'
    }
    if (!password) {
      next.password = 'Password is required.'
    } else if (password.length < 8) {
      next.password = 'Password must be at least 8 characters.'
    }
    if (!confirmPassword) {
      next.confirmPassword = 'Please confirm your password.'
    } else if (password !== confirmPassword) {
      next.confirmPassword = 'Passwords do not match.'
    }
    return next
  }

  async function handleSubmit(e) {
    e.preventDefault()
    setApiError('')
    const next = validate()
    setErrors(next)
    if (Object.keys(next).length > 0) return

    setSubmitting(true)
    try {
      await register(email.trim(), password)
      // Registration succeeded -> send the user to log in with the new
      // account (credentials are not echoed or reused on the login route).
      navigate('/login', { replace: true, state: { registered: email.trim() } })
    } catch (error) {
      const status = error?.response?.status
      if (status === 409) {
        setApiError('An account with this email already exists. Try signing in instead.')
      } else if (status === 400) {
        setApiError(error?.response?.data?.error ?? 'Please check your input and try again.')
      } else {
        setApiError(
          error?.response?.data?.error ??
            'Unable to reach the server. Please try again later.',
        )
      }
    } finally {
      setSubmitting(false)
    }
  }

  return (
    <div className="mx-auto mt-10 max-w-md rounded-lg border border-slate-200 bg-white p-6 shadow-sm">
      <h1 className="text-2xl font-semibold text-slate-900">Create account</h1>
      <p className="mb-6 text-sm text-slate-500">
        Start backtesting your trading strategies
      </p>

      <form className="space-y-4" onSubmit={handleSubmit} noValidate>
        <label className="block text-sm font-medium text-slate-700">
          Email
          <input
            type="email"
            name="email"
            autoComplete="email"
            placeholder="you@example.com"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            aria-invalid={errors.email ? 'true' : undefined}
            className={`mt-1 w-full rounded-md border px-3 py-2 text-sm ${
              errors.email ? 'border-rose-400' : 'border-slate-300'
            }`}
          />
          {errors.email && <span className="mt-1 block text-xs text-rose-600">{errors.email}</span>}
        </label>

        <label className="block text-sm font-medium text-slate-700">
          Password
          <input
            type="password"
            name="password"
            autoComplete="new-password"
            placeholder="At least 8 characters"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            aria-invalid={errors.password ? 'true' : undefined}
            className={`mt-1 w-full rounded-md border px-3 py-2 text-sm ${
              errors.password ? 'border-rose-400' : 'border-slate-300'
            }`}
          />
          {errors.password && (
            <span className="mt-1 block text-xs text-rose-600">{errors.password}</span>
          )}
        </label>

        <label className="block text-sm font-medium text-slate-700">
          Confirm password
          <input
            type="password"
            name="confirmPassword"
            autoComplete="new-password"
            placeholder="Repeat password"
            value={confirmPassword}
            onChange={(e) => setConfirmPassword(e.target.value)}
            aria-invalid={errors.confirmPassword ? 'true' : undefined}
            className={`mt-1 w-full rounded-md border px-3 py-2 text-sm ${
              errors.confirmPassword ? 'border-rose-400' : 'border-slate-300'
            }`}
          />
          {errors.confirmPassword && (
            <span className="mt-1 block text-xs text-rose-600">{errors.confirmPassword}</span>
          )}
        </label>

        {apiError && (
          <div
            role="alert"
            className="rounded-md border border-rose-200 bg-rose-50 px-3 py-2 text-sm text-rose-700"
          >
            {apiError}
          </div>
        )}

        <button
          type="submit"
          disabled={submitting}
          className="w-full rounded-md bg-emerald-500 px-4 py-2 text-sm font-medium text-white hover:bg-emerald-600 disabled:opacity-60"
        >
          {submitting ? 'Creating account…' : 'Create account'}
        </button>
      </form>

      <p className="mt-4 text-center text-sm text-slate-600">
        Already have an account?{' '}
        <Link to="/login" className="text-emerald-600 hover:underline">
          Sign in
        </Link>
      </p>
    </div>
  )
}