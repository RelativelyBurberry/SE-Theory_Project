# AlphaForge Insight — Auth routes (Phase 2)
#
#   POST /auth/register   Register a new user (bcrypt-hashed password)
#   POST /auth/login      Validate credentials, issue a JWT
#   GET  /auth/me         Return the currently authenticated user (JWT)
#
# Passwords are hashed with bcrypt and NEVER stored or returned in plaintext.
# Password hashes are NEVER returned in API responses.
#
# Uses parameterized SQL only (see backend/database/queries.R).
# JWT helpers, DB connection and query functions are sourced in plumber.R
# before this router is mounted (see backend/plumber.R).

# Minimum password length enforced at the API layer.
MIN_PASSWORD_LENGTH <- 8L
# bcrypt cost factor (>= 10 recommended).
BCRYPT_COST <- 12L

# Simple email pattern (sufficient for validation purposes). Uses POSIX
# [:space:] because R's default TRE regex engine does not support \s.
EMAIL_REGEX <- "^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$"

#' Validate registration/login input.
#'
#' @param email Character. Raw email from the request.
#' @param password Character. Raw password from the request.
#' @param require_password_format Logical. Enforce minimum password length
#'   on registration (login only requires a non-empty password so existing
#'   accounts with legacy short passwords can still sign in).
#' @return A character vector of error messages (empty when valid).
validate_credentials <- function(email, password,
                                 require_password_format = FALSE) {
  errors <- character(0)
  if (is.null(email) || !nzchar(email)) {
    errors <- c(errors, "Email is required.")
  } else if (!grepl(EMAIL_REGEX, trimws(email))) {
    errors <- c(errors, "Please provide a valid email address.")
  }
  if (is.null(password) || !nzchar(password)) {
    errors <- c(errors, "Password is required.")
  } else if (require_password_format && nchar(password) < MIN_PASSWORD_LENGTH) {
    errors <- c(errors, paste0("Password must be at least ", MIN_PASSWORD_LENGTH, " characters."))
  }
  errors
}

# ---------------------------------------------------------------------------
# POST /auth/register
# ---------------------------------------------------------------------------
#* Register a new user
#* @param email User's email address
#* @param password User's password (min 8 characters)
#* @serializer unboxedJSON
#* @post /auth/register
function(req, res) {
  body <- req$body
  if (is.null(body) || !is.list(body)) {
    res$status <- 400L
    return(list(success = FALSE, error = "Request body must be JSON: {\"email\": ..., \"password\": ...}"))
  }

  email <- if (!is.null(body$email)) trimws(as.character(body$email)) else ""
  password <- if (!is.null(body$password)) as.character(body$password) else ""

  errors <- validate_credentials(email, password, require_password_format = TRUE)
  if (length(errors) > 0) {
    res$status <- 400L
    return(list(success = FALSE, error = paste(errors, collapse = " "), errors = errors))
  }

  con <- NULL
  tryCatch(
    {
      con <- get_db_connection()
      hashed <- bcrypt::hashpw(password, bcrypt::gensalt(log_rounds = BCRYPT_COST))
      user <- insert_user(con, email, hashed)

      if (is.null(user)) {
        res$status <- 409L
        return(list(
          success = FALSE,
          error = "An account with this email already exists."
        ))
      }

      res$status <- 201L
      list(
        success = TRUE,
        message = "Account created successfully.",
        user = list(id = user$id, email = user$email, created_at = user$created_at)
      )
    },
    error = function(e) {
      # Never expose internal database errors to the client.
      cat("[auth/register] ERROR:", conditionMessage(e), "\n")
      res$status <- 500L
      list(success = FALSE, error = "An unexpected server error occurred. Please try again later.")
    },
    finally = {
      close_db_connection(con)
    }
  )
}

# ---------------------------------------------------------------------------
# POST /auth/login
# ---------------------------------------------------------------------------
#* Login and receive a JWT
#* @param email User's email address
#* @param password User's password
#* @serializer unboxedJSON
#* @post /auth/login
function(req, res) {
  body <- req$body
  if (is.null(body) || !is.list(body)) {
    res$status <- 400L
    return(list(success = FALSE, error = "Request body must be JSON: {\"email\": ..., \"password\": ...}"))
  }

  email <- if (!is.null(body$email)) trimws(as.character(body$email)) else ""
  password <- if (!is.null(body$password)) as.character(body$password) else ""

  errors <- validate_credentials(email, password)
  if (length(errors) > 0) {
    res$status <- 400L
    return(list(success = FALSE, error = paste(errors, collapse = " "), errors = errors))
  }

  con <- NULL
  tryCatch(
    {
      con <- get_db_connection()
      user <- get_user_by_email(con, email)

      # Identical response for unknown email and wrong password (no account
      # enumeration). Both produce HTTP 401.
      valid <- !is.null(user) && bcrypt::checkpw(password, user$password_hash)
      if (!valid) {
        res$status <- 401L
        return(list(success = FALSE, error = "Invalid email or password."))
      }

      token <- create_jwt(user$id, user$email)

      list(
        success = TRUE,
        message = "Login successful.",
        token = token,
        expiresIn = paste0(JWT_EXPIRES_IN_SECONDS, "s"),
        user = list(id = user$id, email = user$email, created_at = user$created_at)
      )
    },
    error = function(e) {
      cat("[auth/login] ERROR:", conditionMessage(e), "\n")
      res$status <- 500L
      list(success = FALSE, error = "An unexpected server error occurred. Please try again later.")
    },
    finally = {
      close_db_connection(con)
    }
  )
}

# ---------------------------------------------------------------------------
# GET /auth/me
# ---------------------------------------------------------------------------
#* Return the currently authenticated user
#* @get /auth/me
#* @serializer unboxedJSON
function(req, res) {
  # Token check first — missing/invalid tokens are rejected without touching
  # the database, so these cases always produce HTTP 401.
  token <- extract_bearer_token(req)
  if (is.null(token)) {
    res$status <- 401L
    return(list(success = FALSE, error = "Authentication required. Provide a valid Bearer token in the Authorization header."))
  }
  claims <- tryCatch(validate_jwt(token), error = function(e) NULL)
  if (is.null(claims)) {
    res$status <- 401L
    return(list(success = FALSE, error = "Invalid or expired token."))
  }

  # Token is valid -> load the current user from the database.
  con <- NULL
  tryCatch(
    {
      con <- get_db_connection()
      user <- get_user_by_id(con, claims$sub)
      if (is.null(user)) {
        res$status <- 401L
        return(list(success = FALSE, error = "User no longer exists."))
      }
      list(
        success = TRUE,
        user = list(id = user$id, email = user$email, created_at = user$created_at)
      )
    },
    error = function(e) {
      cat("[auth/me] ERROR:", conditionMessage(e), "\n")
      if (res$status < 400L) res$status <- 500L
      list(success = FALSE, error = "An unexpected server error occurred. Please try again later.")
    },
    finally = {
      close_db_connection(con)
    }
  )
}