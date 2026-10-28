# AlphaForge Insight — Phase 2 auth unit tests (no database required)
#
# Covers the bcrypt password hashing and jose JWT logic plus the credential
# and bearer-token helpers. Run from the repository root:
#   Rscript tests/backend/test_auth_unit.R
#
# Exit code 0 = pass, 1 = fail.

TEST_JWT_SECRET <- "unit-test-secret-0123456789abcdef"

source("backend/api/auth_helpers.R")

# Make the helpers use the fixed test secret.
Sys.setenv(JWT_SECRET = TEST_JWT_SECRET)

results <- list()

record <- function(name, passed, detail = "") {
  results[[length(results) + 1L]] <<- list(name = name, passed = isTRUE(passed), detail = detail)
  cat(sprintf("%s %s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

expect_true_with_msg <- function(cond, msg = "") {
  !!cond
}

# --- bcrypt password hashing -----------------------------------------------
plaintext <- "SuperSecret123!"
hash <- bcrypt::hashpw(plaintext, bcrypt::gensalt(12L))

record("bcrypt: hash is not plaintext", !identical(hash, plaintext) && !grepl("SuperSecret", hash))
record("bcrypt: hash has bcrypt prefix", grepl("^\\$2", hash))
record("bcrypt: correct password verifies", bcrypt::checkpw(plaintext, hash))
record("bcrypt: wrong password rejected", !bcrypt::checkpw("WrongPassword!", hash))
record("bcrypt: hash is unique per password via salt",
       !identical(bcrypt::hashpw(plaintext, bcrypt::gensalt(12L)), bcrypt::hashpw(plaintext, bcrypt::gensalt(12L))))

# --- JWT encode / decode ---------------------------------------------------
token <- create_jwt(user_id = 42, email = "user@example.com", expires_in_seconds = 3600)
record("jwt: token is a non-empty string", is.character(token) && nzchar(token) && nchar(token) > 40)

claims <- validate_jwt(token)
record("jwt: roundtrip preserves sub", identical(claims$sub, "42"))
record("jwt: roundtrip preserves email", identical(claims$email, "user@example.com"))
record("jwt: exp is in the future", claims$exp > as.numeric(Sys.time()))
record("jwt: token has three dot-separated parts", length(strsplit(token, "\\.")[[1]]) == 3)

# Expired token (use -3600 to clear jose's internal 60-second grace period)
expired <- create_jwt(1, "user@example.com", expires_in_seconds = -3600)
r <- tryCatch({ validate_jwt(expired); "accepted" }, error = function(e) "rejected")
record("jwt: expired token rejected", identical(r, "rejected"))

# Token signed with a different secret
Sys.setenv(JWT_SECRET = paste0("other-secret-", TEST_JWT_SECRET))
other_token <- tryCatch(
  { token_from_other_secret <- create_jwt(1, "a@b.c", 3600); "created" },
  error = function(e) "failed"
)
Sys.setenv(JWT_SECRET = TEST_JWT_SECRET)
if (identical(other_token, "created")) {
  r2 <- tryCatch({ validate_jwt(token_from_other_secret); "accepted" }, error = function(e) "rejected")
  record("jwt: token signed with different secret rejected", identical(r2, "rejected"))
} else {
  record("jwt: token signed with different secret rejected", FALSE, "could not create cross-secret token")
}

# Tampered token
tampered <- paste0(substr(token, 1, nchar(token) - 2), "xx")
r3 <- tryCatch({ validate_jwt(tampered); "accepted" }, error = function(e) "rejected")
record("jwt: tampered token rejected", identical(r3, "rejected"))

# Missing / short secret
Sys.setenv(JWT_SECRET = "")
r4 <- tryCatch({ read_jwt_secret(); "accepted" }, error = function(e) "rejected")
record("jwt: missing secret rejected", identical(r4, "rejected"))
Sys.setenv(JWT_SECRET = "short")
r5 <- tryCatch({ read_jwt_secret(); "accepted" }, error = function(e) "rejected")
record("jwt: short secret rejected", identical(r5, "rejected"))
Sys.setenv(JWT_SECRET = TEST_JWT_SECRET)

# --- credential validation -------------------------------------------------
source("backend/api/auth.R") # only for validate_credentials + constants

record("validation: valid inputs pass",
       length(validate_credentials("user@example.com", "password123", TRUE)) == 0)
record("validation: empty email rejected",
       length(validate_credentials("", "password123", TRUE)) > 0)
record("validation: malformed email rejected",
       length(validate_credentials("not-an-email", "password123", TRUE)) > 0)
record("validation: missing password rejected",
       length(validate_credentials("user@example.com", "", TRUE)) > 0)
record("validation: short password rejected (registration)",
       length(validate_credentials("user@example.com", "short", TRUE)) > 0)
record("validation: short password allowed (login)",
       length(validate_credentials("user@example.com", "short", FALSE)) == 0)

# --- bearer token extraction ----------------------------------------------
fake_req <- function(auth) {
  list(HTTP_AUTHORIZATION = auth)
}
record("bearer: valid header extracted",
       identical(extract_bearer_token(fake_req("Bearer abc.def.ghi")), "abc.def.ghi"))
record("bearer: missing header -> NULL", is.null(extract_bearer_token(fake_req(NULL))))
record("bearer: empty header -> NULL", is.null(extract_bearer_token(fake_req(""))))
record("bearer: non-bearer scheme -> NULL",
       is.null(extract_bearer_token(fake_req("Basic dXNlcjpwYXNz"))))
record("bearer: lowercase bearer accepted",
       identical(extract_bearer_token(fake_req("bearer tok123")), "tok123"))

# --- DATABASE_URL parsing (synthetic URLs only, no real credentials) -------
source("backend/database/connection.R")

p1 <- parse_db_url(normalize_db_url("postgresql://user:pass@host.example:5433/mydb"))
record("parse: user extracted", identical(p1$user, "user"))
record("parse: password extracted", identical(p1$password, "pass"))
record("parse: host extracted", identical(p1$host, "host.example"))
record("parse: port extracted", identical(p1$port, 5433L))
record("parse: dbname extracted", identical(p1$dbname, "mydb"))

p2 <- parse_db_url(normalize_db_url("postgresql://u:p@h.example/db?sslmode=require&channel_binding=require"))
record("parse: default port 5432", identical(p2$port, 5432L))
record("parse: sslmode query param kept", identical(p2$sslmode, "require"))
record("parse: channel_binding query param kept", identical(p2$channel_binding, "require"))

p3 <- parse_db_url(normalize_db_url("postgresql+psycopg://alice:s3cr3t@db.internal:5555/neondb"))
record("parse: +driver scheme normalized", identical(p3$user, "alice") && identical(p3$dbname, "neondb"))

record("parse: malformed URL (no dbname) rejected",
       length(tryCatch(parse_db_url("postgresql://u:p@h"), error = function(e) "err")) == 1)
record("parse: malformed URL (no userinfo) rejected",
       length(tryCatch(parse_db_url("postgresql://h.example/db"), error = function(e) "err")) == 1)

library(DBI)
library(RPostgres)

# --- summary ---------------------------------------------------------------
passed <- vapply(results, function(r) r$passed, logical(1))
cat(sprintf("\n%d/%d unit tests passed\n", sum(passed), length(passed)))
if (!all(passed)) {
  failed <- results[!passed]
  for (f in failed) cat("  FAILED:", f$name, "-", f$detail, "\n")
  quit(status = 1)
}
cat("ALL UNIT TESTS PASSED\n")
quit(status = 0)