-- =============================================================================
-- AlphaForge Insight — ticker column length migration (additive, non-destructive)
-- =============================================================================
-- The ML/portfolio pages accept legitimate Yahoo Finance symbols with exchange
-- suffixes (RELIANCE.NS = 11 chars, HDFCBANK.NS = 10, MOTHERSON.NS = 12), but
-- two persistence columns were created as VARCHAR(10). Inserts of NSE-style
-- symbols failed with:
--     ERROR: value too long for type character varying(10)
--
-- Fix: widen every column that stores a Yahoo Finance ticker/symbol to
-- VARCHAR(20), matching the shared ticker rule (TICKER_MAX_LENGTH = 20) in
-- backend/analytics/data_retrieval.R and frontend/src/utils/ticker.js.
--
-- Columns widened (verified against the live database via
-- information_schema.columns before this migration):
--   backtests.ticker   VARCHAR(10) -> VARCHAR(20)
--   ml_models.ticker   VARCHAR(10) -> VARCHAR(20)
--   ml_runs.tickers    stays VARCHAR(255) (comma-separated list; already wide)
--
-- NOT touched: every other VARCHAR(10)/short column stores non-ticker data.
--
-- Idempotent (ALTER TYPE ... USING is a no-op when the type already matches)
-- and non-destructive: no tables are recreated, no rows are modified or
-- dropped. Apply with:
--   Rscript database/apply_migration_ticker_length.R
-- =============================================================================

ALTER TABLE backtests
    ALTER COLUMN ticker TYPE VARCHAR(20) USING ticker::VARCHAR(20);

ALTER TABLE ml_models
    ALTER COLUMN ticker TYPE VARCHAR(20) USING ticker::VARCHAR(20);
