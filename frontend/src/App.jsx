import { Routes, Route, Navigate } from 'react-router-dom'
import Layout from './components/Layout'
import PrivateRoute from './components/PrivateRoute'
import LoginPage from './pages/LoginPage'
import RegisterPage from './pages/RegisterPage'
import DashboardPage from './pages/DashboardPage'
import MarketDataPage from './pages/MarketDataPage'
import LiveMarketPage from './pages/LiveMarketPage'
import StrategyBuilderPage from './pages/StrategyBuilderPage'
import BacktestResultsPage from './pages/BacktestResultsPage'
import MLComparisonPage from './pages/MLComparisonPage'
import MLBacktestPage from './pages/MLBacktestPage'

/**
 * AlphaForge Insight — application routes (Phase 4).
 *
 * Public routes: /login, /register.
 * Protected routes (require a valid JWT / auth state): /dashboard,
 * /market-data, /strategy-builder, /results and /results/:backtestId.
 * Unauthenticated visitors are redirected to /login by PrivateRoute.
 */
export default function App() {
  return (
    <Routes>
      <Route element={<Layout />}>
        <Route path="/" element={<Navigate to="/dashboard" replace />} />
        <Route
          path="/dashboard"
          element={
            <PrivateRoute>
              <DashboardPage />
            </PrivateRoute>
          }
        />
        <Route
          path="/market-data"
          element={
            <PrivateRoute>
              <MarketDataPage />
            </PrivateRoute>
          }
        />
        <Route
          path="/live-market"
          element={
            <PrivateRoute>
              <LiveMarketPage />
            </PrivateRoute>
          }
        />
        <Route
          path="/strategy-builder"
          element={
            <PrivateRoute>
              <StrategyBuilderPage />
            </PrivateRoute>
          }
        />
        <Route
          path="/results"
          element={
            <PrivateRoute>
              <BacktestResultsPage />
            </PrivateRoute>
          }
        />
        <Route
          path="/results/:backtestId"
          element={
            <PrivateRoute>
              <BacktestResultsPage />
            </PrivateRoute>
          }
        />
        <Route
          path="/ml-compare"
          element={
            <PrivateRoute>
              <MLComparisonPage />
            </PrivateRoute>
          }
        />
        <Route
          path="/ml-backtest"
          element={
            <PrivateRoute>
              <MLBacktestPage />
            </PrivateRoute>
          }
        />
      </Route>
      <Route path="/login" element={<LoginPage />} />
      <Route path="/register" element={<RegisterPage />} />
      <Route path="*" element={<Navigate to="/dashboard" replace />} />
    </Routes>
  )
}