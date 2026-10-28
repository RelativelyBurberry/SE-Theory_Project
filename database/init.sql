-- =============================================================================
-- AlphaForge Insight — database schema (APPLIED to Neon PostgreSQL, Phase 2)
-- =============================================================================
--
-- STATUS: APPLIED 2026-09-12 to the project's Neon PostgreSQL database via
-- `Rscript database/apply_schema.R` (non-destructive; CREATE TABLE IF NOT
-- EXISTS, safe to re-run). Verified by tests/backend/test_db_connection.R.
--
-- Application: Phase 2 (see IMPLEMENTATION_PLAN.md sections G and Q).
--
-- Review-1 tables (per IMPLEMENTATION_PLAN.md section G):
--   users, backtests, results
--
-- Future normalisation (later phases, per the project proposal):
--   strategies, trades, backtest_metrics
--
-- =============================================================================

-- ----------------------------------------------------------------------------
-- users — one row per registered account
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS users (
    id            SERIAL PRIMARY KEY,
    email         VARCHAR(255) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,          -- bcrypt hash (Phase 2)
    created_at    TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- ----------------------------------------------------------------------------
-- backtests — one row per executed backtest (configuration + metadata)
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS backtests (
    id              SERIAL PRIMARY KEY,
    user_id         INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    -- VARCHAR(20): Yahoo Finance symbols with exchange suffixes exceed 10
    -- characters (RELIANCE.NS = 11); matches TICKER_MAX_LENGTH in
    -- backend/analytics/data_retrieval.R. Existing databases were widened by
    -- database/migration_ticker_length.sql.
    ticker          VARCHAR(20) NOT NULL,
    strategy        VARCHAR(20) NOT NULL,          -- 'MA_Crossover' | 'RSI'
    start_date      DATE NOT NULL,
    end_date        DATE NOT NULL,
    initial_capital NUMERIC(12,2) NOT NULL,
    parameters      JSONB NOT NULL,                 -- e.g. {"fast_period":50,"slow_period":200}
    created_at      TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- ----------------------------------------------------------------------------
-- results — one row per backtest (quantitative outputs; 1:1 with backtests)
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS results (
    id            SERIAL PRIMARY KEY,
    backtest_id   INTEGER NOT NULL REFERENCES backtests(id) ON DELETE CASCADE,
    initial_capital NUMERIC(12,2),       -- Phase 6 additive column
    final_value     NUMERIC(12,2),       -- Phase 6 additive column
    total_return  NUMERIC(10,4),
    cagr          NUMERIC(10,4),
    sharpe_ratio  NUMERIC(10,4),
    sortino_ratio NUMERIC(10,4),
    max_drawdown  NUMERIC(10,4),
    win_rate      NUMERIC(10,4),
    total_trades  INTEGER,
    volatility    NUMERIC(10,4),
    equity_curve  JSONB,      -- array of [date_string, portfolio_value]
    trade_history JSONB       -- array of {date, action, price, shares, portfolio_value}
);

-- ----------------------------------------------------------------------------
-- Indexes (performance)
-- ----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_backtests_user_id    ON backtests(user_id);
CREATE INDEX IF NOT EXISTS idx_backtests_created_at ON backtests(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_results_backtest_id  ON results(backtest_id);

-- =============================================================================
-- End of schema. Applied to Neon PostgreSQL 2026-09-12 (see apply_schema.R).
-- =============================================================================