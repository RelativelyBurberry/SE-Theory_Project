# AlphaForge Insight — Database queries
#
# All queries use parameterized SQL via DBI to prevent SQL injection.
# Passwords (hashes) are never returned from query functions unless
# explicitly requested for authentication comparison.

# --- Auth queries ----------------------------------------------------------

#' Insert a new user into the users table.
#'
#' @param con A DBI connection.
#' @param email Character. The user's email (must be unique).
#' @param password_hash Character. The bcrypt-hashed password.
#' @return A list with id, email, created_at on success.
#' @stop If the insert fails (e.g. duplicate email).
insert_user <- function(con, email, password_hash) {
  query <- "
    INSERT INTO users (email, password_hash)
    VALUES ($1, $2)
    RETURNING id, email, created_at
  "
  tryCatch(
    {
      result <- dbGetQuery(con, query, params = list(email, password_hash))
      as.list(result[1, ])
    },
    error = function(e) {
      msg <- conditionMessage(e)
      # PostgreSQL unique violation error code is 23505
      if (grepl("duplicate key|23505|unique constraint", msg, ignore.case = TRUE)) {
        return(NULL)  # Caller checks for NULL to detect duplicate
      }
      stop("Database error during user registration: ", msg)
    }
  )
}

#' Fetch a user by email address.
#'
#' @param con A DBI connection.
#' @param email Character. The email to look up.
#' @return A list with id, email, password_hash, created_at, or NULL if not found.
get_user_by_email <- function(con, email) {
  query <- "
    SELECT id, email, password_hash, created_at
    FROM users
    WHERE email = $1
  "
  result <- dbGetQuery(con, query, params = list(email))
  if (nrow(result) == 0) {
    return(NULL)
  }
  as.list(result[1, ])
}

#' Fetch a user by ID.
#'
#' @param con A DBI connection.
#' @param user_id Integer. The user's ID.
#' @return A list with id, email, created_at, or NULL if not found.
get_user_by_id <- function(con, user_id) {
  query <- "
    SELECT id, email, created_at
    FROM users
    WHERE id = $1
  "
  result <- dbGetQuery(con, query, params = list(as.integer(user_id)))
  if (nrow(result) == 0) {
    return(NULL)
  }
  as.list(result[1, ])
}

# --- Backtest queries -------------------------------------------------------

#' Insert a new backtest record.
#'
#' @param con DBI connection.
#' @param user_id Integer user ID.
#' @param ticker Character ticker symbol.
#' @param strategy Character strategy name.
#' @param start_date Date or character start date.
#' @param end_date Date or character end date.
#' @param initial_capital Numeric initial capital.
#' @param parameters List of strategy parameters (stored as JSONB).
#' @return A list with the new backtest row (id, user_id, ticker, etc.).
insert_backtest <- function(con, user_id, ticker, strategy, start_date,
                            end_date, initial_capital, parameters) {
  params_json <- jsonlite::toJSON(parameters, auto_unbox = TRUE)
  query <- "
    INSERT INTO backtests (user_id, ticker, strategy, start_date, end_date,
                           initial_capital, parameters)
    VALUES ($1, $2, $3, $4, $5, $6, $7)
    RETURNING id, user_id, ticker, strategy, start_date, end_date,
              initial_capital, parameters, created_at
  "
  result <- dbGetQuery(con, query, params = list(
    as.integer(user_id), ticker, strategy,
    as.character(start_date), as.character(end_date),
    as.numeric(initial_capital), params_json
  ))
  as.list(result[1, ])
}

#' Insert a results record for a backtest.
#'
#' @param con DBI connection.
#' @param backtest_id Integer backtest ID.
#' @param metrics Named list from compute_performance_metrics().
#' @param equity_curve_json Character JSON string of the equity curve.
#' @param trade_history_json Character JSON string of the trade history.
#' @return A list with the new results row.
insert_result <- function(con, backtest_id, metrics,
                          equity_curve_json, trade_history_json) {
  safe <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_real_ else as.numeric(x)
  safe_int <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_integer_ else as.integer(x)

  query <- "
    INSERT INTO results (backtest_id, initial_capital, final_value,
                         total_return, cagr, sharpe_ratio,
                         sortino_ratio, max_drawdown, win_rate, total_trades,
                         volatility, equity_curve, trade_history)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
    RETURNING id, backtest_id
  "
  result <- dbGetQuery(con, query, params = list(
    as.integer(backtest_id),
    safe(metrics$initial_capital),
    safe(metrics$final_value),
    safe(metrics$total_return),
    safe(metrics$cagr),
    safe(metrics$sharpe),
    safe(metrics$sortino),
    safe(metrics$maximum_drawdown),
    safe(metrics$win_rate),
    safe_int(metrics$total_trades),
    safe(metrics$volatility),
    equity_curve_json,
    trade_history_json
  ))
  as.list(result[1, ])
}

#' Get a backtest by ID (with user isolation).
#'
#' @param con DBI connection.
#' @param backtest_id Integer backtest ID.
#' @param user_id Integer user ID (for isolation).
#' @return A list with the backtest row, or NULL if not found.
get_backtest_by_id <- function(con, backtest_id, user_id) {
  query <- "
    SELECT b.id, b.user_id, b.ticker, b.strategy, b.start_date, b.end_date,
           b.initial_capital, b.parameters, b.created_at,
           r.initial_capital AS result_initial_capital,
           r.final_value AS result_final_value,
           r.total_return, r.cagr, r.sharpe_ratio, r.sortino_ratio,
           r.max_drawdown, r.win_rate, r.total_trades, r.volatility,
           r.equity_curve, r.trade_history
    FROM backtests b
    LEFT JOIN results r ON r.backtest_id = b.id
    WHERE b.id = $1 AND b.user_id = $2
  "
  result <- dbGetQuery(con, query, params = list(
    as.integer(backtest_id), as.integer(user_id)
  ))
  if (nrow(result) == 0) return(NULL)
  as.list(result[1, ])
}

#' Get paginated backtest history for a user.
#'
#' @param con DBI connection.
#' @param user_id Integer user ID.
#' @param limit Integer max rows (default 20).
#' @param offset Integer offset (default 0).
#' @return A list with items (list of backtest summaries) and total count.
get_backtest_history <- function(con, user_id, limit = 20L, offset = 0L) {
  count_query <- "SELECT COUNT(*) AS n FROM backtests WHERE user_id = $1"
  count_result <- dbGetQuery(con, count_query, params = list(as.integer(user_id)))
  total <- as.integer(count_result$n[1])

  query <- "
    SELECT b.id, b.ticker, b.strategy, b.start_date, b.end_date,
           b.initial_capital, b.created_at,
           r.total_return, r.sharpe_ratio, r.total_trades
    FROM backtests b
    LEFT JOIN results r ON r.backtest_id = b.id
    WHERE b.user_id = $1
    ORDER BY b.created_at DESC
    LIMIT $2 OFFSET $3
  "
  rows <- dbGetQuery(con, query, params = list(
    as.integer(user_id), as.integer(limit), as.integer(offset)
  ))

  items <- lapply(seq_len(nrow(rows)), function(i) as.list(rows[i, ]))

  list(items = items, total = total)
}
