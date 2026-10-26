# Completion codes use exactly the same scheme as the Python app, so the
# Python `dafs-teach verify` command can check R codes too (and vice versa).

# Instructors should override this via the DAFS_TEACH_SECRET environment
# variable. This repo is public, so the default gives no real protection.
default_secret <- "dafs-teach-default-secret-change-me"

get_secret <- function() {
  secret <- Sys.getenv("DAFS_TEACH_SECRET")
  if (nzchar(secret)) secret else default_secret
}

using_default_secret <- function() !nzchar(Sys.getenv("DAFS_TEACH_SECRET"))

normalize_name <- function(student_name) {
  paste(strsplit(trimws(tolower(student_name)), "\\s+")[[1]], collapse = " ")
}

#' Completion code for a student and lesson
#'
#' @param student_name Name exactly as the student entered it in the app.
#' @param lesson_id Lesson id, e.g. "01_r_basics".
#' @param secret Shared secret; defaults to the DAFS_TEACH_SECRET environment variable.
#' @export
generate_code <- function(student_name, lesson_id, secret = get_secret()) {
  message <- paste0(normalize_name(student_name), "|", lesson_id)
  hex <- toupper(digest::hmac(enc2utf8(secret), enc2utf8(message), algo = "sha256", serialize = FALSE))
  paste0(substr(hex, 1, 5), "-", substr(hex, 6, 10))
}

#' Check a completion code a student sent you
#'
#' @inheritParams generate_code
#' @param code The code the student sent.
#' @return TRUE if the code is valid for this student and lesson.
#' @export
verify_code <- function(student_name, lesson_id, code, secret = get_secret()) {
  if (using_default_secret() && missing(secret)) {
    warning("DAFS_TEACH_SECRET is not set, so the built-in default secret is used. ",
            "Since this repo is public, codes checked this way give no real protection.",
            call. = FALSE)
  }
  identical(generate_code(student_name, lesson_id, secret), toupper(trimws(code)))
}
