# AlphaForge Insight — JWT authentication helpers
#
# Reusable helpers for issuing and validating JWTs. Any protected endpoint
# (e.g. /auth/me today, /backtest/* in later phases) uses:
#
#   auth <- require_auth(req, res, con = con)
#   if (!auth$ok) { res$status <- auth$status; return(auth$body) }
#
# Token validation uses jose (HMAC-SHA256) with the JWT_SECRET environment
# variable. The secret is never logged and never returned to clients.

# Minimum acceptable JWT secret length (development guard). Production should
# use a much longer random secret (>= 32 chars).
JWT_SECRET_MIN_LENGTH <- 16L

# Token lifetime in seconds (24 hours).
JWT_EXPIRES_IN_SECONDS <- 24 * 60 * 60

#' Read the JWT secret from the environment.
#'
#' @return Raw bytes of the JWT secret.
#' @stop If JWT_SECRET is unset or too short.
read_jwt_secret <- function() {
  secret <- Sys.getenv("JWT_SECRET", unset = "")
  if (!nzchar(secret)) {
    stop("JWT_SECRET environment variable is not set. Configure it in backend/.Renviron.")
  }
  if (nchar(secret) < JWT_SECRET_MIN_LENGTH) {
    stop("JWT_SECRET is too short. Use a long random string (>= 16 chars).")
  }
  charToRaw(secret)
}

#' Create a signed JWT for a user.
#'
#' @param user_id Integer or character. The user's database ID.
#' @param email Character. The user's email.
#' @param expires_in_seconds Numeric. Lifetime of the token.
#' @return A JWT string.
create_jwt <- function(user_id, email, expires_in_seconds = JWT_EXPIRES_IN_SECONDS) {
  secret <- read_jwt_secret()
  now <- as.numeric(Sys.time())
  claim <- jose::jwt_claim(
    sub = as.character(user_id),
    email = email,
    iat = now,
    exp = now + expires_in_seconds
  )
  jose::jwt_encode_hmac(claim, secret)
}

#' Extract the bearer token from a Plumber request.
#'
#' @param req The Plumber request object.
#' @return The token string, or NULL if missing/malformed.
extract_bearer_token <- function(req) {
  auth <- req$HTTP_AUTHORIZATION
  if (is.null(auth) || length(auth) == 0 || !nzchar(auth)) {
    return(NULL)
  }
  parts <- strsplit(trimws(auth), "\\s+")[[1]]
  if (length(parts) != 2 || !identical(toupper(parts[1]), "BEARER")) {
    return(NULL)
  }
  parts[2]
}

#' Validate a JWT and return its claims.
#'
#' @param token Character. The JWT to validate.
#' @return The decoded claims list (contains sub, email, iat, exp).
#' @stop If the token is invalid, expired, or signed with a different secret.
validate_jwt <- function(token) {
  if (is.null(token) || !nzchar(token)) {
    stop("missing token")
  }
  secret <- read_jwt_secret()
  # jwt_decode_hmac verifies the HMAC signature and rejects expired tokens.
  jose::jwt_decode_hmac(token, secret)
}

#' Auth guard helper for protected endpoints.
#'
#' Extracts and validates the JWT from the request. On success returns
#' list(ok = TRUE, claims = <claims>, user = <user row or NULL>). On failure
#' returns list(ok = FALSE, status = 401L, body = list(...)) so the caller
#' can set the response status and return the JSON body directly. This avoids
#' relying on stop() semantics inside Plumber handlers.
#'
#' @param req The Plumber request object.
#' @param res The Plumber response object (reserved for future use).
#' @param con A DBI connection to look up the user (optional).
#' @return list(ok = TRUE, ...) on success or list(ok = FALSE, status, body).
require_auth <- function(req, res = NULL, con = NULL) {
  token <- extract_bearer_token(req)
  if (is.null(token)) {
    return(list(
      ok = FALSE,
      status = 401L,
      body = list(success = FALSE, error = "Authentication required. Provide a valid Bearer token in the Authorization header.")
    ))
  }

  claims <- tryCatch(validate_jwt(token), error = function(e) NULL)
  if (is.null(claims)) {
    return(list(
      ok = FALSE,
      status = 401L,
      body = list(success = FALSE, error = "Invalid or expired token.")
    ))
  }

  user <- NULL
  if (!is.null(con)) {
    user <- get_user_by_id(con, claims$sub)
    if (is.null(user)) {
      return(list(
        ok = FALSE,
        status = 401L,
        body = list(success = FALSE, error = "User no longer exists.")
      ))
    }
  }

  list(ok = TRUE, claims = claims, user = user)
}