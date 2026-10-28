# AlphaForge Insight — Backend (R + Plumber)

Phase 2 backend: database (Neon PostgreSQL) + authentication. Phase 1
implemented `GET /health`; Phase 2 adds `POST /auth/register`,
`POST /auth/login`, `GET /auth/me` with bcrypt password hashing and JWT.

## Run

From the repository root:

```bash
Rscript backend/plumber.R
```

or from inside `backend/`:

```bash
Rscript plumber.R
```

The API starts on `http://127.0.0.1:8000` by default
(`PLUMBER_HOST` / `PLUMBER_PORT` env vars override; see `.Renviron.example`).

Requirements: `DATABASE_URL` (Neon connection string) and `JWT_SECRET` (>= 16
chars) must be set in `backend/.Renviron` (copy `.Renviron.example`) or a root
`.env` file. Both files are git-ignored — never commit real secrets.

Apply the schema once (non-destructive, safe to re-run):

```bash
Rscript database/apply_schema.R    # from the repository root
```

## Health check

```bash
curl http://127.0.0.1:8000/health
```

Expected response:

```json
{
  "status": "ok",
  "service": "alphaforge-insight-api",
  "version": "0.2.0",
  "phase": "2-auth",
  "timestamp": "...",
  "message": "AlphaForge Insight API is running"
}
```

## Endpoints (Phase 2)

| Method | Path | Description |
|---|---|---|
| GET | `/health` | Liveness check |
| POST | `/auth/register` | `{email, password}` → 201 / 400 / 409 |
| POST | `/auth/login` | `{email, password}` → 200 + JWT / 400 / 401 |
| GET | `/auth/me` | Current user (requires `Authorization: Bearer <jwt>`) |

## Required R packages (Phase 2)

- `plumber`, `jsonlite` (Phase 1)
- `DBI`, `RPostgres` (PostgreSQL connection + parameterised queries)
- `bcrypt` (password hashing, cost 12)
- `jose` (HMAC-SHA256 JWTs)

Future phase packages (do NOT install yet): `quantmod`, `TTR`,
`PerformanceAnalytics`, `xts`, `tidyquant`, `testthat` — see `DESCRIPTION`.

## Structure

| Path | Purpose | Status |
|---|---|---|
| `plumber.R` | API entry point, filters, mounts, `/health` | Implemented |
| `api/health.R` | Health handler (unit-testable) | Implemented |
| `api/auth.R` | Register / login / me routes | Implemented |
| `api/auth_helpers.R` | JWT + bearer/auth-guard helpers | Implemented |
| `api/data.R`, `strategies.R`, `backtest.R` | Future route files | Placeholders |
| `analytics/*.R` | Indicators, signals, execution, metrics, data, utils | Placeholders |
| `database/connection.R` | DATABASE_URL → connection (keyword-arg parsing) | Implemented |
| `database/queries.R` | Parameterised user SQL | Implemented |
| `database/schema.sql` | Schema pointer (authoritative file: `database/init.sql`) | Applied |
| `logs/` | Server log output (git-ignored) | Empty |

## Tests

```bash
Rscript tests/backend/test_health.R          # health endpoint (no DB)
Rscript tests/backend/test_auth_unit.R       # 37 tests, no DB needed
Rscript tests/backend/test_db_connection.R   # schema on Neon (7 tests)
Rscript tests/backend/test_auth_api.R        # full auth flow on Neon (26 tests)
```

(run from the repository root; see `tests/backend/README.md`).