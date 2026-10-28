# UTILITIES — PLACEHOLDER (NOT IMPLEMENTED)
#
# Planned helpers (usable across analytics and API layers):
#   validate_backtest_input(...)   -> strategy parameter validation
#      (positive integers, slow_period > fast_period, 0 < oversold <
#       overbought < 100, start < end, capital > 0)
#   http_error(status, message)    -> structured { success: false, error: ... }
#   format_number / format_percent -> metric formatting for JSON
#   read_env(key, default)         -> safe environment access
#
# Error-handling convention (backend):
#   - Every endpoint returns { success: bool, ... } with an `error` string on
#     failure; status codes 400 / 401 / 500 (IMPLEMENTATION_PLAN.md section F).
#   - Plumber handlers wrap risky work in tryCatch and log server-side.
#
# No functionality is implemented in this phase.