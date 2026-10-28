# Health endpoint handler for GET /health.
#
# Kept separate from plumber.R so it can be unit-tested directly
# (see tests/backend/test_health.R). Returns a flat JSON object.

health_handler <- function() {
  list(
    status = "ok",
    service = "alphaforge-insight-api",
    version = "0.6.0",
    phase = "6-backtest-api",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"),
    message = "AlphaForge Insight API is running"
  )
}