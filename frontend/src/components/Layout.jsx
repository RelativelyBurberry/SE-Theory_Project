import { NavLink, Outlet, useNavigate, useLocation } from 'react-router-dom'
import ApiStatus from './ApiStatus'
import { useAuth } from '../context/AuthContext'

const navItems = [
  { to: '/dashboard', label: 'Dashboard', end: true },
  { to: '/market-data', label: 'Market Data', end: false },
  { to: '/live-market', label: 'Live Monitor', end: false },
  { to: '/strategy-builder', label: 'Strategy Builder', end: false },
  { to: '/ml-compare', label: 'ML Compare', end: false },
  { to: '/ml-backtest', label: 'ML Portfolio', end: false },
  { to: '/results', label: 'Results', end: false },
]

/**
 * Application shell: header navigation, API status indicator and footer.
 * Outlet renders the active child route.
 *
 * Phase 2: when authenticated, the header shows the user's email and a
 * Logout button instead of the "Sign in" link.
 * Phase 3: adds the Market Data nav item.
 * Phase 4: adds the Strategy Builder nav item (signal preview, no trading).
 *
 * Presentation mode (?presentation=1): a screenshot-friendly layout for the
 * research paper — hides the health strip and footer and widens the content
 * column. It only affects chrome visibility; no data is changed.
 */
export default function Layout() {
  const { user, isAuthenticated, logout } = useAuth()
  const navigate = useNavigate()
  const location = useLocation()
  const presentation =
    new URLSearchParams(location.search).get('presentation') === '1'

  function togglePresentation() {
    const params = new URLSearchParams(location.search)
    if (presentation) params.delete('presentation')
    else params.set('presentation', '1')
    const query = params.toString()
    navigate(`${location.pathname}${query ? `?${query}` : ''}`, { replace: true })
  }

  function handleLogout() {
    logout()
    navigate('/login', { replace: true })
  }

  return (
    <div className="flex min-h-screen flex-col bg-slate-50">
      <header className="bg-slate-900 text-white">
        <div className="mx-auto flex max-w-6xl items-center justify-between px-4 py-3">
          <NavLink to="/dashboard" className="text-lg font-semibold">
            AlphaForge Insight
          </NavLink>
          <nav className="flex items-center gap-6">
            {isAuthenticated &&
              navItems.map((item) => (
                <NavLink
                  key={item.to}
                  to={item.to}
                  end={item.end}
                  className={({ isActive }) =>
                    isActive
                      ? 'font-medium text-emerald-400'
                      : 'text-slate-300 hover:text-white'
                  }
                >
                  {item.label}
                </NavLink>
              ))}
            {isAuthenticated ? (
              <div className="flex items-center gap-3">
                <span className="hidden text-sm text-slate-300 md:inline">
                  {user?.email}
                </span>
                <button
                  type="button"
                  onClick={togglePresentation}
                  className="rounded bg-slate-700 px-3 py-1 text-sm font-medium text-white hover:bg-slate-600"
                >
                  {presentation ? 'Standard view' : 'Presentation view'}
                </button>
                <button
                  type="button"
                  onClick={handleLogout}
                  className="rounded bg-slate-700 px-3 py-1 text-sm font-medium text-white hover:bg-slate-600"
                >
                  Log out
                </button>
              </div>
            ) : (
              <NavLink
                to="/login"
                className="rounded bg-emerald-500 px-3 py-1 text-sm font-medium text-white hover:bg-emerald-600"
              >
                Sign in
              </NavLink>
            )}
          </nav>
        </div>
      </header>

      {!presentation && (
        <div className="mx-auto w-full max-w-6xl px-4 py-3">
          <ApiStatus />
        </div>
      )}

      <main
        className={`mx-auto w-full flex-1 px-4 py-6 ${
          presentation ? 'max-w-7xl' : 'max-w-6xl'
        }`}
      >
        <Outlet />
      </main>

      {!presentation && (
        <footer className="mx-auto w-full max-w-6xl px-4 py-6 text-sm text-slate-500">
          AlphaForge Insight — Quant Strategy Backtester. Backtests run through
          the R engine (t+1-open execution) and persist to Neon.
        </footer>
      )}
    </div>
  )
}