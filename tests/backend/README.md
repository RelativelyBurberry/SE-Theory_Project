# Backend tests

Automated test suite for the R/Plumber backend.

## Running (from the repository root)

| Test | Covers | DB required | Count |
|---|---|---|---|
| `Rscript tests/backend/test_health.R` | `GET /health` over HTTP | No | 1 |
| `Rscript tests/backend/test_auth_unit.R` | bcrypt, JWT, credential validation, bearer extraction, DATABASE_URL parsing | No | 37 |
| `Rscript tests/backend/test_db_connection.R` | Neon connection + schema (users/backtests/results, unique email, NOT NULL) | Yes (`DATABASE_URL`) | 7 |
| `Rscript tests/backend/test_auth_api.R` | Full HTTP auth flow register→login→me + DB persistence + bcrypt hash + cleanup | Yes (`DATABASE_URL`) | 26 |
| `Rscript tests/backend/test_data_api.R` | `GET /stocks/validate` + `GET /market-data` on live Yahoo data; JWT enforcement | Yes (`DATABASE_URL`) | 24 |
| `Rscript tests/backend/test_strategy_api.R` | `GET /strategies/defaults` + `POST /strategies/preview` (MA + RSI) on live data; 400/401 paths | Yes (`DATABASE_URL`) | 22 |
| `Rscript tests/backend/test_backtest_api.R` | `/backtest/run`, `/history`, `/:id`, `/:id/metrics`, `/:id/trades` on live data; auth, validation, persistence, isolation, security | Yes (`DATABASE_URL`) | 54 |
| `Rscript tests/backend/test_e2e_smoke.R` | Full-stack smoke: register → login → real AAPL 2015→2025 MA 50/200 backtest → results API → Neon → cleanup | Yes (`DATABASE_URL`) | 41 |

Exit code 0 = pass, non-zero = fail. The DB tests use the real Neon database
configured via `DATABASE_URL` (`backend/.Renviron` or root `.env`).

Test hygiene: the API/E2E suites create throwaway users with unique
`*@test.local` emails and **delete them before `quit()`** — note that R's
`quit()` skips `on.exit` handlers, so cleanup is invoked explicitly before the
summary `quit()` calls (an earlier version of this pattern leaked test users in
Neon; this is fixed and the DB was purged). Backtests/results are removed by
`ON DELETE CASCADE`.

## Requires

R packages: `plumber`, `jsonlite`, `DBI`, `RPostgres`, `bcrypt`, `jose`, `curl`,
`openssl` (plus `quantmod`, `TTR`, `xts` for the live-data suites).
The analytics suites (`tests/analytics/`) share the same record-based runner
style (no `testthat` dependency).

## Future

Phase 7 consolidates the record-based suites under a single runner and adds
any remaining edge cases.