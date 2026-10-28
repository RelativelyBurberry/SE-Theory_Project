# Shared market data: R + Python

The Python Live Forecast endpoint now retrieves historical OHLCV through the R/Plumber backend's provider chain and cache instead of calling Yahoo Finance directly by default.

## Configure both Render services

Generate one long random secret (for example with `python -c "import secrets; print(secrets.token_urlsafe(48))"`) and set the same value in both services:

- R backend: `ALPHAFORGE_INTERNAL_MARKET_DATA_TOKEN`
- Python forecasting service: `ALPHAFORGE_INTERNAL_MARKET_DATA_TOKEN`

On the Python service also set:

- `ALPHAFORGE_R_MARKET_DATA_URL=https://YOUR-R-BACKEND.onrender.com` (base URL only, no trailing path)
- Optional emergency fallback: `PYTHON_MARKET_DATA_ALLOW_YFINANCE_FALLBACK=false` (recommended default; set true only if you explicitly want direct Yahoo access as a last resort)

The R backend exposes `GET /internal/market-data?ticker=AAPL&start=2021-10-24&end=2026-10-24`, protected by `X-AlphaForge-Internal-Token`. It uses the same `fetch_ohlcv()` function, provider chain, in-memory cache, and disk cache as the ordinary market-data route. Do not expose the token to the Vite frontend or any `VITE_*` variable.

## Deployment

1. Deploy the R changes first.
2. Set the shared token in the R service environment and redeploy.
3. Deploy the Python service with this updated `service.py`.
4. Set the R base URL and the same token in the Python service environment, then redeploy.
5. Test `/health`, then POST `/live-forecast` with `{"ticker":"AAPL","horizons":[1,5,10],"history_period":"5y"}`.

The R service must have its market-data provider keys and `MARKET_DATA_PROVIDER=auto` configured. The Python service no longer calls yfinance unless the explicit fallback flag is enabled.
