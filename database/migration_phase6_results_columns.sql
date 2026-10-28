-- =============================================================================
-- AlphaForge Insight — Phase 6 additive migration (results table)
-- =============================================================================
-- STATUS: APPLIED 2026-09-12 to the project's Neon PostgreSQL database.
--
-- Adds two additive, nullable columns to results so the persisted record can
-- serve the frontend dashboard's "Initial Capital" / "Final Value" cards
-- after a page refresh (GET /backtest/:id).
--
-- Proven necessary by tests/backend/test_backtest_api.R: the run response
-- includes metrics.initialCapital / metrics.finalValue, but the persisted
-- record had no column for them, so a fresh GET could not reproduce them.
--
-- Idempotent (ADD COLUMN IF NOT EXISTS) — safe to re-run; preserves data.
-- =============================================================================

ALTER TABLE results
    ADD COLUMN IF NOT EXISTS initial_capital NUMERIC(12,2),
    ADD COLUMN IF NOT EXISTS final_value    NUMERIC(12,2);

-- =============================================================================
-- End of migration.
-- =============================================================================