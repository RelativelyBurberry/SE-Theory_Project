import { Navigate } from 'react-router-dom'
import { useAuth } from '../context/AuthContext'

/**
 * PrivateRoute — authentication guard for protected pages.
 *
 * Unauthenticated users are redirected to /login. Authenticated users render
 * the wrapped children. Uses <Navigate replace> so the protected page never
 * enters browser history (Back does not return to it).
 */
export default function PrivateRoute({ children }) {
  const { isAuthenticated } = useAuth()

  if (!isAuthenticated) {
    return <Navigate to="/login" replace />
  }

  return children
}