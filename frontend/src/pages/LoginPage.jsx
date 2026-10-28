import { useState } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { useAuth } from '../context/AuthContext'
import { Navigate } from 'react-router-dom'

/**
 * Login — Phase 2.
 *
 * Email/password form with inline validation, a loading state on submit,
 * backend error display (401 invalid credentials, network failures) and a
 * link to the Register page.
 */
export default function LoginPage() {
  const { login, isAuthenticated } = useAuth()
  const navigate = useNavigate()

  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
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
      await login(email.trim(), password)
      navigate('/dashboard', { replace: true })
    } catch (error) {
      const status = error?.response?.status
      if (status === 401) {
        setApiError('Invalid email or password.')
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
      <h1 className="text-2xl font-semibold text-slate-900">Sign in</h1>
      <p className="mb-6 text-sm text-slate-500">
        AlphaForge Insight — Quant Strategy Backtester
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
            autoComplete="current-password"
            placeholder="••••••••"
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
          {submitting ? 'Signing in…' : 'Sign in'}
        </button>
      </form>

      <p className="mt-4 text-center text-sm text-slate-600">
        No account?{' '}
        <Link to="/register" className="text-emerald-600 hover:underline">
          Register
        </Link>
      </p>
    </div>
  )
}