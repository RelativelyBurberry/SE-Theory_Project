-- =============================================================================
-- AlphaForge Insight — ML extension schema (additive, non-destructive)
-- =============================================================================
-- New tables for the ML/DL module (ML plan sections 12/13/16):
--   ml_models — one row per trained model (hyperparameters, split sizes,
--               classification metrics, trading metrics, importance).
--   ml_runs   — one row per model-comparison run (the unified comparison
--               table for the DA2 comparative-analysis requirement).
--
-- The EXISTING users / backtests / results tables are untouched. This file
-- uses CREATE TABLE IF NOT EXISTS and is safe to re-run (apply via
-- Rscript database/apply_migration_ml.R).
-- =============================================================================

CREATE TABLE IF NOT EXISTS ml_models (
    id              SERIAL PRIMARY KEY,
    user_id         INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    model_key       VARCHAR(40) NOT NULL,           -- e.g. 'random_forest', 'lstm'
    model_name      VARCHAR(80) NOT NULL,           -- human label
    ticker          VARCHAR(10) NOT NULL,
    start_date      DATE NOT NULL,
    end_date        DATE NOT NULL,
    horizon         INTEGER NOT NULL DEFAULT 5,
    buy_threshold   NUMERIC(10,4) NOT NULL DEFAULT 0.02,
    sell_threshold  NUMERIC(10,4) NOT NULL DEFAULT 0.02,
    hyperparameters JSONB,
    n_train         INTEGER,
    n_validation    INTEGER,
    n_test          INTEGER,
    features_used   JSONB,
    feature_importance JSONB,
    -- classification (macro averages)
    accuracy        NUMERIC(10,4),
    precision_macro NUMERIC(10,4),
    recall_macro    NUMERIC(10,4),
    f1_macro        NUMERIC(10,4),
    roc_auc_macro   NUMERIC(10,4),
    -- trading (existing engine metrics)
    total_return    NUMERIC(12,4),
    cagr            NUMERIC(10,4),
    sharpe          NUMERIC(10,4),
    sortino         NUMERIC(10,4),
    max_drawdown    NUMERIC(10,4),
    win_rate        NUMERIC(10,4),
    volatility      NUMERIC(10,4),
    total_trades    INTEGER,
    created_at      TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS ml_runs (
    id          SERIAL PRIMARY KEY,
    user_id     INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    tickers     VARCHAR(255),
    model_keys  JSONB,
    comparison  JSONB,          -- the full comparison table (one row per model)
    created_at  TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_ml_models_user_id ON ml_models(user_id);
CREATE INDEX IF NOT EXISTS idx_ml_runs_user_id   ON ml_runs(user_id);
