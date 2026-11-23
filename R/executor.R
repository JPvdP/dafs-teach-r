# Runs student code in a separate R process (Rscript), so a crash or an
# infinite loop can never take down the app itself.

# Available to every 'assert' check: .check(condition, "message shown on failure")
check_helper <- ".check <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)"

# Generous, because loading the tidyverse on a fresh (Windows) laptop can be slow
TIMEOUT_SECONDS <- 30

rscript_path <- function() {
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

# The environment for student code, without RStudio's session variables. When
# the app runs in RStudio, the student's R process is a child of the RStudio
# session, so rlang would think it runs in RStudio too, and print messages
# (like the tidyverse startup banner) to stdout, where they break the checks.
child_env <- function() {
  env <- Sys.getenv()
  env <- env[!names(env) %in% c("RSTUDIO", "RSTUDIO_SESSION_PID")]
  stats::setNames(as.character(env), names(env))
}

# Returns list(stdout, stderr, crashed, timed_out). `crashed` is based on the
# exit status, not on stderr, so warnings and package startup messages
# (which also go to stderr) don't count as errors.
run_file <- function(path, timeout = TIMEOUT_SECONDS) {
  res <- tryCatch(
    processx::run(
      rscript_path(), c("--no-init-file", path),
      error_on_status = FALSE, timeout = timeout, encoding = "UTF-8", env = child_env()
    ),
    system_command_timeout_error = function(e) NULL
  )
  if (is.null(res) || isTRUE(res$timeout)) {
    return(list(stdout = "", stderr = "", crashed = FALSE, timed_out = TRUE))
  }
  clean <- function(x) {
    x <- gsub("\r\n", "\n", x %||% "", fixed = TRUE)
    # help() pages use typewriter-style bold/underline ("_\bR", "R\bR"),
    # which is unreadable outside a terminal
    gsub(".\b", "", x, perl = TRUE)
  }
  list(stdout = clean(res$stdout), stderr = clean(res$stderr),
       crashed = res$status != 0, timed_out = FALSE)
}

# Runs before the student's code:
# - Text help: with HTML help (R's default on Windows), help() starts a help
#   server and opens a browser tab, which shows a connection error because the
#   process has already ended, and nothing is printed in the app.
# - Installing from student code would fail anyway (Rscript has no CRAN mirror
#   set), could be killed halfway by the timeout and leave a broken package, and
#   would reinstall on every Run. So explain where to install instead. Attached
#   rather than assigned, so it doesn't show up in the student's ls().
child_setup <- c(
  'options(help_type = "text")',
  paste(
    'attach(list(install.packages = function(pkgs, ...) stop(paste0(',
    '"Install packages in the RStudio Console, not in the app. The Console is busy while ",',
    '"the app runs, so: stop the app (red Stop sign), run install.packages(",',
    'paste(deparse(pkgs), collapse = ""), ") in the Console, and start the app again ",',
    '"with dafsteach::run_app()."), call. = FALSE)), name = "dafsteach:app", warn.conflicts = FALSE)'
  )
)

run_source <- function(code, timeout = TIMEOUT_SECONDS) {
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path))
  con <- file(path, open = "w", encoding = "UTF-8")
  writeLines(c(child_setup, code), con)
  close(con)
  run_file(path, timeout)
}

# Only the error itself (e.g. "Error: x should be 5"): the rest of stderr can
# quote the check's source, which would leak the answer.
last_error <- function(stderr) {
  lines <- strsplit(trimws(stderr), "\n", fixed = TRUE)[[1]]
  lines <- lines[!grepl("^(Execution halted|Calls:)", lines)]
  start <- utils::tail(grep("^Error", lines), 1)
  if (length(start)) paste(lines[start:length(lines)], collapse = "\n") else utils::tail(lines, 1)
}

run_result <- function(stdout, stderr, timed_out, passed, feedback, crashed = FALSE) {
  list(stdout = stdout, stderr = stderr, timed_out = timed_out, passed = passed,
       feedback = feedback, crashed = crashed)
}

run_code <- function(student_code, tests, timeout = TIMEOUT_SECONDS) {
  run <- run_source(student_code, timeout)
  stdout <- run$stdout
  stderr <- run$stderr
  # `crashed` lets the app tell real errors apart from messages and warnings
  result <- function(...) run_result(stdout, stderr, ..., crashed = run$crashed)

  if (run$timed_out) {
    return(run_result("", "", TRUE, FALSE,
                      paste("Your code timed out after", timeout, "seconds. Check for infinite loops.")))
  }
  if (run$crashed && !nzchar(trimws(stdout))) {
    return(result(FALSE, FALSE,
                      paste0("Your code raised an error:\n\n```\n", trimws(stderr), "\n```")))
  }

  shown <- function(x) if (nzchar(trimws(x))) trimws(x) else "(empty)"

  for (test in tests) {
    type <- test$type
    if (type %in% c("output_contains", "output_equals") && !nzchar(trimws(stdout))) {
      return(result(FALSE, FALSE, paste(
        "Your code ran, but didn't show anything. To see a value, put `print( )`",
        "around it, or write the variable's name on a line of its own."
      )))
    }
    if (type == "output_contains") {
      if (!grepl(test$value, stdout, fixed = TRUE)) {
        return(result(FALSE, FALSE, paste0(
          "Expected the output to contain **`", test$value, "`**, but it didn't.\n\n",
          "Your output:\n```\n", shown(stdout), "\n```"
        )))
      }
    } else if (type == "output_equals") {
      if (trimws(stdout) != trimws(test$value)) {
        return(result(FALSE, FALSE, paste0(
          "Expected output:\n```\n", trimws(test$value), "\n```\n",
          "Your output:\n```\n", shown(stdout), "\n```"
        )))
      }
    } else if (type == "assert") {
      check <- run_source(paste(student_code, check_helper, test$code, sep = "\n"), timeout)
      if (check$timed_out) {
        return(result(TRUE, FALSE, "Timed out while running checks."))
      }
      if (check$crashed) {
        desc <- test$description %||% test$code
        return(result(FALSE, FALSE, paste0(
          "Check failed: **", desc, "**\n\n```\n", last_error(check$stderr), "\n```"
        )))
      }
    }
  }
  result(FALSE, TRUE, "All checks passed!")
}
