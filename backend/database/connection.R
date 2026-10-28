# AlphaForge Insight — Database connection layer
#
# Provides get_db_connection() using DBI/RPostgres with the DATABASE_URL
# environment variable (Neon PostgreSQL). The env var must be loaded before
# this file is sourced; plumber.R calls readRenviron("backend/.Renviron")
# at startup to ensure DATABASE_URL is available.
#
# Never hard-code credentials. The connection string comes from DATABASE_URL.
# Never log the connection string or password.
#
# NOTE on connection style: RPostgres in this environment silently falls back
# to localhost:5432 when a full connection URI is passed via the dbname
# parameter, so DATABASE_URL is parsed here into explicit keyword arguments
# (host, port, dbname, user, password) plus query parameters (sslmode etc.).

library(DBI)
library(RPostgres)

#' Normalize a database URL to the plain libpq/postgresql scheme.
#'
#' Some tools (e.g. Python SQLAlchemy configuration) supply URLs with a
#' dialect suffix such as postgresql+psycopg:// or postgres+driver://.
#' libpq / RPostgres require a plain postgresql:// or postgres:// scheme.
#' This function only canonicalizes the scheme; the host, credentials,
#' database name and query parameters are passed through untouched.
#'
#' @param url Character. The connection string from DATABASE_URL.
#' @return The same URL with any "+driver" suffix removed from the scheme.
normalize_db_url <- function(url) {
  sub("^postgres(ql)?\\+[^/]+://", "postgres\\1://", url)
}

#' Parse a PostgreSQL connection URL into explicit connection arguments.
#'
#' RPostgres cannot be given a full `postgresql://` URI as the dbname
#' parameter reliably (it silently falls back to localhost:5432). Instead we
#' split the URL into keyword arguments that libpq understands directly.
#'
#' @param url Character. A normalized postgresql:// connection string.
#' @return A named list with elements: host, port (integer), dbname, user,
#'   password, plus any query parameters (sslmode, channel_binding, ...).
parse_db_url <- function(url) {
  if (!nzchar(url)) stop("DATABASE_URL is empty.")

  # Strip scheme (postgresql:// or postgres://).
  rest <- sub("^postgresql://", "", url, ignore.case = TRUE)
  rest <- sub("^postgres://", "", rest, ignore.case = TRUE)
  if (!grepl("@", rest, fixed = TRUE)) {
    stop("DATABASE_URL does not look like a postgresql:// URL (missing userinfo@).")
  }

  # Split off query string.
  qmark <- regexpr("\\?", rest, fixed = FALSE)
  query <- character(0)
  if (qmark > 0) {
    query_part <- substr(rest, qmark + 1, nchar(rest))
    rest <- substr(rest, 1, qmark - 1)
    kv <- strsplit(query_part, "&", fixed = TRUE)[[1]]
    for (pair in kv) {
      kv2 <- strsplit(pair, "=", fixed = TRUE)[[1]]
      if (length(kv2) == 2) {
        query[kv2[1]] <- utils::URLdecode(kv2[2])
      }
    }
  }

  # Split off database name.
  slash <- regexpr("/", rest, fixed = TRUE)
  if (slash < 1) stop("DATABASE_URL is missing the database name (no '/' before dbname).")
  hostport <- substr(rest, 1, slash - 1)
  dbname <- substr(rest, slash + 1, nchar(rest))
  if (!nzchar(dbname)) stop("DATABASE_URL is missing the database name.")

  # Split userinfo (user:password@host[:port]).
  at <- regexpr("@", hostport, fixed = TRUE)
  if (at < 1) stop("DATABASE_URL is missing the userinfo (no '@' before host).")
  userinfo <- substr(hostport, 1, at - 1)
  hostpart <- substr(hostport, at + 1, nchar(hostport))
  if (!nzchar(hostpart)) stop("DATABASE_URL is missing the host.")

  colon <- regexpr(":", userinfo, fixed = TRUE)
  if (colon > 0) {
    user <- substr(userinfo, 1, colon - 1)
    password <- substr(userinfo, colon + 1, nchar(userinfo))
  } else {
    user <- userinfo
    password <- ""
  }
  if (!nzchar(user)) stop("DATABASE_URL is missing the user name.")

  hostcolon <- regexpr(":", hostpart, fixed = TRUE)
  if (hostcolon > 0) {
    host <- substr(hostpart, 1, hostcolon - 1)
    port_raw <- substr(hostpart, hostcolon + 1, nchar(hostpart))
    port <- suppressWarnings(as.integer(port_raw))
    if (is.na(port)) stop("DATABASE_URL has an invalid port: ", port_raw)
  } else {
    host <- hostpart
    port <- 5432L
  }

  args <- list(
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password
  )
  for (nm in names(query)) args[[nm]] <- query[[nm]]
  args
}

#' Open a PostgreSQL connection with bounded retry.
#'
#' Connection establishment (DNS, TCP, TLS, auth handshake) can fail
#' transiently — e.g. RPostgres' bundled libpq on Windows occasionally fails to
#' translate a host name on a slow/edge network even though normal Windows
#' tooling resolves the same host. Retrying establishment masks those transient
#' failures. Each attempt uses a PostgreSQL connect_timeout of 4 seconds so a
#' single attempt is bounded even when a returned address (for example an
#' unreachable IPv6 address listed ahead of usable IPv4 addresses) stalls.
#'
#' Only connection establishment is retried: once dbConnect returns a
#' connection the caller takes over, so SQL errors that occur after a
#' connection has already been established are never retried here.
#'
#' @param args Named connection arguments produced by parse_db_url() (host,
#'   port, dbname, user, password, plus libpq query parameters). connect_timeout
#'   is forced to 4 seconds regardless of what the URL contains.
#' @param max_attempts Integer. Maximum number of connection attempts.
#' @param sleep_seconds Numeric vector. Delay in seconds before attempt 2, 3,
#'   ... (1 second and 2 seconds by default). The last value is reused for any
#'   further attempts.
#' @param connect_fun Function used to establish the connection (defaults to
#'   DBI::dbConnect). Injectable for unit tests; the app always uses dbConnect.
#' @return A DBI connection object.
#' @stop The original connection error when every attempt fails.
db_connect_with_retry <- function(args,
                                  max_attempts = 3L,
                                  sleep_seconds = c(1, 2),
                                  connect_fun = dbConnect) {
  max_attempts <- as.integer(max_attempts)
  stopifnot(max_attempts >= 1L, is.function(connect_fun))

  # Bound every attempt so one stalled address cannot hang a request for a
  # long time (observed here: 1-107s per connection without a timeout).
  args$connect_timeout <- 4L

  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    if (attempt > 1L) {
      index <- min(attempt - 1L, length(sleep_seconds))
      delay <- sleep_seconds[index]
      if (is.finite(delay) && delay > 0) Sys.sleep(delay)
    }

    r <- tryCatch(
      {
        con <- do.call(connect_fun, c(list(drv = RPostgres::Postgres()), args))
        return(con)
      },
      error = function(e) e
    )
    last_error <- r

    if (attempt < max_attempts) {
      message(
        "Database connection attempt ", attempt, " of ", max_attempts,
        " failed: ", conditionMessage(r)
      )
    }
  }

  stop(last_error)
}

#' Get a database connection using DATABASE_URL.
#'
#' Reads DATABASE_URL from the environment and opens a PostgreSQL connection
#' via RPostgres with explicit keyword arguments (see parse_db_url). Connection
#' establishment is retried up to 3 times with a 4 second connect_timeout per
#' attempt (see db_connect_with_retry) to ride out transient DNS/network
#' failures. SQL errors on an established connection are never retried.
#'
#' @return A DBI connection object.
#' @stop If DATABASE_URL is not set or connection fails.
get_db_connection <- function() {
  db_url <- Sys.getenv("DATABASE_URL", unset = "")
  if (!nzchar(db_url)) {
    stop(
      "DATABASE_URL environment variable is not set.\n",
      "Set it in backend/.Renviron (copy from .Renviron.example) or in the\n",
      "root .env file, or export it in your shell before starting the API."
    )
  }
  tryCatch(
    {
      args <- parse_db_url(normalize_db_url(db_url))
      con <- db_connect_with_retry(args, max_attempts = 3L)
      con
    },
    error = function(e) {
      stop(
        "Failed to connect to the database. ",
        "Check that DATABASE_URL is correct and the database is reachable.\n",
        "Error: ", conditionMessage(e)
      )
    }
  )
}

#' Close a database connection safely.
#'
#' @param con A DBI connection object (or NULL).
close_db_connection <- function(con) {
  if (!is.null(con) && dbIsValid(con)) {
    tryCatch(dbDisconnect(con), error = function(e) {
      warning("Failed to close database connection: ", conditionMessage(e))
    })
  }
}