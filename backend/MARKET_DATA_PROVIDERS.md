# Market data providers

The R market-data layer supports an automatic fallback chain:

1. **Twelve Data** (`TWELVEDATA_API_KEY`) — `/time_series`, daily candles.
2. **Financial Modeling Prep** (`FMP_API_KEY`) — stable daily EOD history endpoint. Access to historical endpoints depends on the account plan.
3. **Alpha Vantage** (`ALPHAVANTAGE_API_KEY`) — daily history; free-tier quotas may apply.
4. **Stooq** (`STOOQ_API_KEY`) — daily CSV history. Stooq currently requires an API key for this endpoint.
5. **Yahoo Finance via quantmod** — no key, but unofficial access can be rate-limited.

## Setup

Copy `.Renviron.example` to `.Renviron` for local development, then add any provider keys you have. Keep `.Renviron` out of Git. On Render, set the same variables in the service Environment settings. Leave `MARKET_DATA_PROVIDER=auto` to use the fallback chain. To test a single provider, set it to `twelvedata`, `fmp`, `alphavantage`, `stooq`, or `yahoo`.

Providers without keys are skipped. If a provider is configured but rejects the request (quota, invalid key, unavailable endpoint), the service logs the failure and attempts the next provider. The client receives a combined error only if all providers fail.

## Caching

Successful OHLCV responses are cached in memory and written as RDS files under `MARKET_DATA_CACHE_DIR`. Set this to a persistent mounted disk path in Render to retain cached ticker/range results across process restarts. The default uses a temporary directory and is not durable. The full-history responses from Twelve Data, FMP and Alpha Vantage are also cached in memory for the lifetime of the R process.

## Notes

- Free plans and historical-data access vary by provider and can change. Create your own API keys and verify that daily historical prices are included before relying on a provider.
- Do not put provider API keys in frontend variables (`VITE_*`) or commit them to Git.
- A fallback chain cannot guarantee success if all providers rate-limit the account, block the host, or restrict the requested ticker/history.
