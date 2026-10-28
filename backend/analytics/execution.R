# AlphaForge Insight — backtesting execution engine (Phase 5)
#
# Responsibilities (IMPLEMENTATION_PLAN.md section H):
#   - Consume generated signals and OHLCV data ONLY: the engine never
#     reinterprets strategy rules. The strategy layer (Phase 4) produces
#     signals; this layer executes them.
#   - Execute at the t+1 OPEN (no look-ahead bias):
#         signal generated at the CLOSE of day t  ->  trade executed at the
#         OPEN of day t+1.  A signal on the final available candle is NEVER
#         executed (there is no t+1; no price is fabricated).
#   - Track cash / shares / position (long-only, single asset).
#   - Build the trade log and the daily equity curve (close-to-close marking).
#
# Execution conventions (documented for reproducibility):
#   Position model:  FLAT (0 shares) <-> LONG (>0 shares).
#     BUY is only valid from FLAT; SELL only from LONG.
#     Repeated BUY while LONG is ignored (no duplicate position).
#     SELL while FLAT is ignored (no invalid trade / no short).
#   Order sizing:    shares = floor(cash / execution_price) (all-in, per
#     plan section H). If fewer than 1 full share is affordable the BUY is
#     skipped; cash can never become negative.
#   Valuation:       portfolio_value[t] = cash[t] + shares[t] * Close[t].
#     The equity curve records one observation per market-data date.
#   Transaction costs: Review 1 default is ZERO (documented limitation —
#     "No pain, no gain", Economics Letters 216 (2022) 110584). The engine
#     accepts an extensible cost model so non-zero costs can be injected
#     without redesign:
#        - numeric rate r in [0, 1)   -> cost = notional * r
#        - list(rate = r, fixed = f)  -> cost = f + notional * r
#     A cost is applied to BOTH sides of a trade (buy adds to the cash
#     outflow; sell reduces the proceeds). The Review 1 default (0) is
#     applied silently only because it is the documented assumption.
#   Determinism:     no randomness; trade IDs are assigned in execution order.

library(xts)      # xts/zoo time series (index, cbind, [,)
library(zoo)      # index() and xts attribute helpers

# ---------------------------------------------------------------------------
# Transaction-cost model
# ---------------------------------------------------------------------------

#' Normalize the transaction-cost specification into list(rate, fixed).
#'
#' Review 1 default: 0 (zero cost, documented limitation). Non-zero costs are
#' supported for future experiments:
#'   - a single numeric rate r in [0, 1)  ->  proportional cost r * notional
#'   - a list with optional `rate` (>= 0) and `fixed` (>= 0) elements
#' @param transaction_cost NULL, 0, a numeric rate in [0, 1), or a list.
#' @return list(rate = numeric, fixed = numeric), both >= 0 and finite.
#' @throws on any invalid specification (never silently coerced).
normalize_cost_model <- function(transaction_cost) {
  if (is.null(transaction_cost)) {
    return(list(rate = 0, fixed = 0))
  }
  if (is.numeric(transaction_cost) && length(transaction_cost) == 1L &&
      !is.na(transaction_cost) && is.finite(transaction_cost)) {
    if (transaction_cost < 0) {
      stop("transaction_cost rate must be >= 0.")
    }
    if (transaction_cost >= 1) {
      stop("transaction_cost rate must be < 1 (a proportional rate in [0, 1)).")
    }
    return(list(rate = as.numeric(transaction_cost), fixed = 0))
  }
  if (is.list(transaction_cost)) {
    rate <- if (is.null(transaction_cost$rate)) 0 else transaction_cost$rate
    fixed <- if (is.null(transaction_cost$fixed)) 0 else transaction_cost$fixed
    if (!is.numeric(rate) || length(rate) != 1L || is.na(rate) || !is.finite(rate) ||
        rate < 0) {
      stop("transaction_cost$rate must be a single finite number >= 0.")
    }
    if (!is.numeric(fixed) || length(fixed) != 1L || is.na(fixed) || !is.finite(fixed) ||
        fixed < 0) {
      stop("transaction_cost$fixed must be a single finite number >= 0.")
    }
    return(list(rate = as.numeric(rate), fixed = as.numeric(fixed)))
  }
  stop("transaction_cost must be 0, a numeric rate in [0, 1), or list(rate=, fixed=).")
}

#' Cost of a single trade for a given notional trade value.
#' @param notional shares * execution_price.
#' @param model Normalized cost model (list(rate, fixed)).
#' @return The cost to deduct (buy) / forfeit (sell).
trade_cost <- function(notional, model) {
  model$fixed + notional * model$rate
}

#' Largest whole share count affordable from `cash`, including costs.
#'
#' With cost model (rate, fixed) the buyer must cover shares*price*(1+rate)
#' plus the fixed fee:  q = floor((cash - fixed) / (price * (1 + rate))).
#' With the Review 1 zero-cost model this reduces to floor(cash / price).
#' @param cash Available cash (>= 0).
#' @param price Execution price (open of the execution day).
#' @param model Normalized cost model.
#' @return Integer share count (0 if not even one share is affordable).
max_affordable_shares <- function(cash, price, model) {
  avail <- cash - model$fixed
  per_share <- price * (1 + model$rate)
  q <- floor(avail / per_share)
  if (length(q) != 1L || is.na(q) || !is.finite(q) || q < 0) {
    return(0)
  }
  as.double(q)
}

# ---------------------------------------------------------------------------
# Execution engine
# ---------------------------------------------------------------------------

#' Execute strategy signals into a portfolio over the historical period.
#'
#' Pure, deterministic engine. Inputs are the validated OHLCV series and the
#' generated signal series; the engine only executes, it never reinterprets
#' strategy rules.
#'
#' @param ohlcv xts with at least Open and Close columns, strictly positive
#'   finite prices, sorted ascending by date.
#' @param signals xts (index-identical to `ohlcv`) or character vector of
#'   length nrow(ohlcv) with values "BUY", "SELL" or NA (HOLD). A signal at
#'   row t is executed at the OPEN of row t+1; the signal on the final row
#'   is never executed.
#' @param initial_capital Single finite number > 0.
#' @param transaction_cost Cost specification (default 0; see
#'   normalize_cost_model).
#' @return list(
#'   equity_curve     = data.frame(date, cash, shares, close,
#'                                 portfolio_value, daily_return),
#'   trade_history    = data.frame(trade_id, signal_date, execution_date, side,
#'                                 execution_price, shares, cost, cash_after,
#'                                 position_after),
#'   completed_trades = data.frame(round_trip_id, entry_trade_id, exit_trade_id,
#'                                 entry_date, entry_price, exit_date,
#'                                 exit_price, shares, entry_cost, exit_cost,
#'                                 profit_loss, return),
#'   open_position    = data.frame(entry_date, entry_price, shares, mark_price,
#'                                 current_value, entry_cost, unrealized_pnl)
#'                     or NULL when flat,
#'   final_cash, final_shares, final_value)
#' @throws on invalid inputs or degenerate OHLCV (documented errors; no
#'   Inf/NaN/negative-cash can be produced).
execute_backtest <- function(ohlcv, signals, initial_capital, transaction_cost = 0) {
  # --- input validation ---------------------------------------------------
  if (!inherits(ohlcv, "xts")) {
    stop("ohlcv must be an xts object.")
  }
  n <- NROW(ohlcv)
  if (n < 1L) {
    stop("ohlcv contains no rows: cannot backtest empty price data.")
  }
  cn <- colnames(ohlcv)
  if (!("Open" %in% cn) || !("Close" %in% cn)) {
    stop("ohlcv must contain Open and Close columns.")
  }
  open <- as.numeric(ohlcv[, "Open"])
  close <- as.numeric(ohlcv[, "Close"])
  if (anyNA(open) || anyNA(close)) {
    stop("ohlcv contains NA/NaN prices; sanitize the data before execution.")
  }
  if (any(!is.finite(open)) || any(!is.finite(close))) {
    stop("ohlcv contains non-finite prices.")
  }
  if (any(open <= 0) || any(close <= 0)) {
    stop("ohlcv prices must be strictly positive for the long-only engine.")
  }

  if (!is.numeric(initial_capital) || length(initial_capital) != 1L ||
      is.na(initial_capital) || !is.finite(initial_capital)) {
    stop("initial_capital must be a single finite number.")
  }
  if (initial_capital <= 0) {
    stop("initial_capital must be greater than zero.")
  }

  cost_model <- normalize_cost_model(transaction_cost)

  # --- signal vector: xts (must align) or plain character vector -----------
  if (inherits(signals, "xts")) {
    if (!identical(index(signals), index(ohlcv))) {
      stop("signals must share the exact time index of ohlcv.")
    }
    sig <- as.character(signals[, 1])
  } else if (is.character(signals) || is.factor(signals)) {
    sig <- as.character(signals)
  } else {
    stop("signals must be an xts or a character vector.")
  }
  if (length(sig) != n) {
    stop(sprintf("signals must have one value per OHLCV row (%d found, %d rows).",
                 length(sig), n))
  }
  invalid_sig <- unique(sig[!is.na(sig) & !(sig %in% c("BUY", "SELL"))])
  if (length(invalid_sig) > 0) {
    stop(sprintf("signals contains invalid value(s): %s (expected BUY, SELL or NA).",
                 paste(invalid_sig, collapse = ", ")))
  }

  idx <- index(ohlcv)

  # --- state ---------------------------------------------------------------
  cash <- initial_capital
  shares <- 0  # long-only: 0 (FLAT) or > 0 (LONG)

  # Pending entry for an open LONG position (needed for round-trip P/L).
  pending_entry_date <- NA
  pending_entry_price <- NA_real_
  pending_entry_cost <- NA_real_
  pending_entry_id <- NA_integer_

  # Accumulators (vector building handles the empty case naturally).
  th_id <- integer(0); th_sig_date <- as.Date(character(0))
  th_ex_date <- as.Date(character(0)); th_side <- character(0)
  th_price <- numeric(0); th_shares <- numeric(0); th_cost <- numeric(0)
  th_cash <- numeric(0); th_pos <- character(0)

  ct_rid <- integer(0); ct_entry_tid <- integer(0); ct_exit_tid <- integer(0)
  ct_entry_date <- as.Date(character(0)); ct_entry_price <- numeric(0)
  ct_exit_date <- as.Date(character(0)); ct_exit_price <- numeric(0)
  ct_shares <- numeric(0); ct_entry_cost <- numeric(0); ct_exit_cost <- numeric(0)
  ct_pnl <- numeric(0); ct_ret <- numeric(0)

  eq_date <- as.Date(character(0)); eq_cash <- numeric(0); eq_shares <- numeric(0)
  eq_close <- numeric(0); eq_pv <- numeric(0)

  trade_id <- 0L
  round_trip_id <- 0L

  # --- day-by-day walk -----------------------------------------------------
  for (i in seq_len(n)) {
    # 1) Execute the signal generated at the CLOSE of day i-1 at the OPEN of
    #    day i. Day 1 has no prior candle; the signal on the final day (n) is
    #    never consumed (no t+1 candle exists).
    if (i >= 2L) {
      prior <- sig[i - 1L]
      if (!is.na(prior)) {
        if (prior == "BUY" && shares == 0) {
          q <- max_affordable_shares(cash, open[i], cost_model)
          if (q >= 1) {
            notional <- q * open[i]
            cost <- trade_cost(notional, cost_model)
            trade_id <- trade_id + 1L
            cash <- cash - notional - cost
            shares <- q
            pending_entry_date <- idx[i]
            pending_entry_price <- open[i]
            pending_entry_cost <- notional + cost
            pending_entry_id <- trade_id
            th_id <- c(th_id, trade_id)
            th_sig_date <- c(th_sig_date, idx[i - 1L])
            th_ex_date <- c(th_ex_date, idx[i])
            th_side <- c(th_side, "BUY")
            th_price <- c(th_price, open[i])
            th_shares <- c(th_shares, q)
            th_cost <- c(th_cost, cost)
            th_cash <- c(th_cash, cash)
            th_pos <- c(th_pos, "LONG")
          }
          # q == 0: not even one share affordable -> no trade, cash untouched.
        } else if (prior == "SELL" && shares > 0) {
          notional <- shares * open[i]
          cost <- trade_cost(notional, cost_model)
          trade_id <- trade_id + 1L
          cash <- cash + notional - cost
          th_id <- c(th_id, trade_id)
          th_sig_date <- c(th_sig_date, idx[i - 1L])
          th_ex_date <- c(th_ex_date, idx[i])
          th_side <- c(th_side, "SELL")
          th_price <- c(th_price, open[i])
          th_shares <- c(th_shares, shares)
          th_cost <- c(th_cost, cost)
          th_cash <- c(th_cash, cash)
          th_pos <- c(th_pos, "FLAT")

          # Completed round trip: BUY -> SELL.
          exit_proceeds <- notional - cost
          profit_loss <- exit_proceeds - pending_entry_cost
          round_trip_id <- round_trip_id + 1L
          ct_rid <- c(ct_rid, round_trip_id)
          ct_entry_tid <- c(ct_entry_tid, pending_entry_id)
          ct_exit_tid <- c(ct_exit_tid, trade_id)
          ct_entry_date <- c(ct_entry_date, pending_entry_date)
          ct_entry_price <- c(ct_entry_price, pending_entry_price)
          ct_exit_date <- c(ct_exit_date, idx[i])
          ct_exit_price <- c(ct_exit_price, open[i])
          ct_shares <- c(ct_shares, shares)
          ct_entry_cost <- c(ct_entry_cost, pending_entry_cost)
          ct_exit_cost <- c(ct_exit_cost, cost)
          ct_pnl <- c(ct_pnl, profit_loss)
          ct_ret <- c(ct_ret, profit_loss / pending_entry_cost)

          shares <- 0
          pending_entry_date <- NA
          pending_entry_price <- NA_real_
          pending_entry_cost <- NA_real_
          pending_entry_id <- NA_integer_
        }
        # BUY while already LONG / SELL while FLAT: ignored by the
        # long-only state machine (no duplicate position, no short).
      }
    }

    # 2) Mark to market at the CLOSE of day i.
    pv <- cash + shares * close[i]
    eq_date <- c(eq_date, idx[i])
    eq_cash <- c(eq_cash, cash)
    eq_shares <- c(eq_shares, shares)
    eq_close <- c(eq_close, close[i])
    eq_pv <- c(eq_pv, pv)
  }

  # --- daily returns from the equity curve --------------------------------
  # daily_return[t] = pv[t] / pv[t-1] - 1; the first observation is NA
  # (no previous value). A zero/undefined previous value yields NA, never Inf.
  if (n >= 2L) {
    daily_ret <- eq_pv[-1] / eq_pv[-n] - 1
    daily_ret <- c(NA_real_, daily_ret)
  } else {
    daily_ret <- NA_real_
  }
  daily_ret[!is.finite(daily_ret)] <- NA_real_

  equity_curve <- data.frame(
    date = as.Date(eq_date),
    cash = eq_cash,
    shares = eq_shares,
    close = eq_close,
    portfolio_value = eq_pv,
    daily_return = daily_ret,
    stringsAsFactors = FALSE
  )

  trade_history <- data.frame(
    trade_id = th_id,
    signal_date = as.Date(th_sig_date),
    execution_date = as.Date(th_ex_date),
    side = th_side,
    execution_price = th_price,
    shares = th_shares,
    cost = th_cost,
    cash_after = th_cash,
    position_after = th_pos,
    stringsAsFactors = FALSE
  )

  completed_trades <- data.frame(
    round_trip_id = ct_rid,
    entry_trade_id = ct_entry_tid,
    exit_trade_id = ct_exit_tid,
    entry_date = as.Date(ct_entry_date),
    entry_price = ct_entry_price,
    exit_date = as.Date(ct_exit_date),
    exit_price = ct_exit_price,
    shares = ct_shares,
    entry_cost = ct_entry_cost,
    exit_cost = ct_exit_cost,
    profit_loss = ct_pnl,
    return = ct_ret,
    stringsAsFactors = FALSE
  )

  # --- open position (mark to market; NOT a completed trade) --------------
  open_position <- NULL
  if (shares > 0) {
    mark <- close[n]
    current_value <- shares * mark
    open_position <- data.frame(
      entry_date = as.Date(pending_entry_date),
      entry_price = pending_entry_price,
      shares = shares,
      mark_price = mark,
      current_value = current_value,
      entry_cost = pending_entry_cost,
      unrealized_pnl = current_value - pending_entry_cost,
      stringsAsFactors = FALSE
    )
  }

  list(
    equity_curve = equity_curve,
    trade_history = trade_history,
    completed_trades = completed_trades,
    open_position = open_position,
    final_cash = cash,
    final_shares = shares,
    final_value = eq_pv[n]
  )
}

# ---------------------------------------------------------------------------
# Orchestration entry point
# ---------------------------------------------------------------------------

#' Run a complete deterministic backtest: execute then measure.
#'
#' Wraps `execute_backtest()` (execution) and `compute_performance_metrics()`
#' + `build_backtest_result()` (metrics, defined in metrics.R) into the clean
#' internal result object defined in the Phase 5 specification.
#'
#' @param ohlcv xts OHLCV (see execute_backtest).
#' @param signals xts/character signals (see execute_backtest).
#' @param initial_capital Single finite number > 0.
#' @param start_date,end_date Requested backtest date range (Date or
#'   YYYY-MM-DD strings). Used for CAGR year calculation (plan section J:
#'   years = (end - start) / 365.25).
#' @param ticker,strategy,parameters Metadata for the result object.
#' @param transaction_cost Cost model (default 0, documented Review 1 value).
#' @param risk_free_rate Annual risk-free rate for Sharpe/Sortino
#'   (documented Review 1 default 0.02 = 2%).
#' @return list(summary, equity_curve, trades, metadata).
#' @seealso execute_backtest, compute_performance_metrics, build_backtest_result
run_backtest <- function(ohlcv, signals, initial_capital,
                         start_date, end_date,
                         ticker = NA_character_,
                         strategy = NA_character_,
                         parameters = list(),
                         transaction_cost = 0,
                         risk_free_rate = 0.02) {
  execution <- execute_backtest(ohlcv, signals, initial_capital, transaction_cost)
  summary <- compute_performance_metrics(
    execution$equity_curve, execution$completed_trades,
    initial_capital, start_date, end_date, risk_free_rate
  )
  metadata <- list(
    ticker = ticker,
    strategy = strategy,
    parameters = parameters,
    start_date = as.Date(start_date),
    end_date = as.Date(end_date),
    transaction_cost = transaction_cost,
    risk_free_rate = risk_free_rate,
    execution_convention =
      "signal generated at the close of t is executed at the OPEN of t+1 (no look-ahead bias)"
  )
  build_backtest_result(execution, summary, metadata)
}