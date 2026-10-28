# Automated integration test for GET /health (Phase 1).
#
# Starts the real Plumber server on a dedicated test port, polls GET /health,
# and asserts the JSON response body. This exercises the full HTTP pipeline
# (CORS filter + route + serializer + server).
#
# Run from the repository root:
#   Rscript tests/backend/test_health.R
#
# Exit code 0 = pass, 1 = fail.
# Requires the `plumber` and `jsonlite` R packages.

PORT <- 8123L # test port; avoids the default 8000

test_health <- function() {
  port <- getOption("alphaforge.test.port", PORT)
  pidfile <- tempfile(fileext = ".pid")
  outlog <- tempfile(fileext = ".log")
  errlog <- tempfile(fileext = ".log")

  # Configure the child through inherited environment variables (the system2
  # `env=` argument is avoided on Windows, where it can break child startup).
  Sys.setenv(PLUMBER_PORT = as.character(port), PLUMBER_HOST = "127.0.0.1",
             PLUMBER_PIDFILE = pidfile)
  on.exit(Sys.unsetenv(c("PLUMBER_PORT", "PLUMBER_HOST", "PLUMBER_PIDFILE")))

  # Start the API in a background R process.
  rscript <- file.path(R.home("bin"), "Rscript")
  invisible(
    system2(
      rscript,
      c(file.path("backend", "plumber.R")),
      wait = FALSE,
      stdout = outlog,
      stderr = errlog
    )
  )

  # The PID file appears once the server process has started.
  deadline <- Sys.time() + 45
  while (!file.exists(pidfile) && Sys.time() < deadline) {
    Sys.sleep(0.3)
  }
  if (!file.exists(pidfile)) {
    stop("server did not start in time\nstdout: ",
         paste(tryCatch(readLines(outlog, warn = FALSE),
                        error = function(e) "not readable"), collapse = "\n"),
         "\nstderr: ",
         paste(tryCatch(readLines(errlog, warn = FALSE),
                        error = function(e) "not readable"), collapse = "\n"))
  }

  pid <- as.integer(readLines(pidfile, warn = FALSE))
  on.exit(tools::pskill(pid), add = TRUE) # stop the server when the test exits

  # Poll GET /health until a non-empty response arrives.
  health_url <- paste0("http://127.0.0.1:", port, "/health")
  body <- NULL
  while (Sys.time() < deadline) {
    body <- tryCatch(
      {
        con <- url(health_url, "rb")
        on.exit(close(con), add = TRUE)
        paste(readLines(con, warn = FALSE), collapse = "\n")
      },
      error = function(e) NULL
    )
    if (!is.null(body) && nzchar(body)) break
    Sys.sleep(0.3)
  }
  if (is.null(body) || !nzchar(body)) {
    stop("no response received from ", health_url)
  }

  cat("Response from GET /health:\n", body, "\n\n")

  parsed <- jsonlite::fromJSON(body)
  stopifnot(
    "status field must be 'ok'" = identical(parsed$status, "ok"),
    "service field mismatch" = identical(parsed$service, "alphaforge-insight-api"),
    "message missing" = nzchar(parsed$message)
  )

  cat("PASS: GET /health returned 200 with status='ok'\n")
  invisible(TRUE)
}

ok <- tryCatch(
  {
    test_health()
    TRUE
  },
  error = function(e) {
    cat("FAIL:", conditionMessage(e), "\n")
    FALSE
  }
)

if (!isTRUE(ok)) quit(status = 1)