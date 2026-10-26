# Anonymous, deadline-based progress reporting, compatible with the Python
# app's Google Apps Script endpoint (DAFS_TEACH_SYNC_URL).

load_deadlines <- function(path) {
  if (!file.exists(path)) return(list())
  items <- yaml::read_yaml(path) %||% list()
  lapply(items, function(item) list(
    id       = item$id,
    deadline = as.POSIXct(item$deadline, format = "%Y-%m-%dT%H:%M:%S"),
    lessons  = as.character(unlist(item$lessons))
  ))
}

sync_url <- function() {
  url <- Sys.getenv("DAFS_TEACH_SYNC_URL")
  if (nzchar(url)) url else NULL
}

# No student name or other identifier is included: only per-exercise
# completed/attempts/solution_views counts, tagged with deadline and lesson.
send_anonymous_report <- function(deadline_id, rows, lesson_titles, timeout = 8) {
  url <- sync_url()
  if (is.null(url)) return(list(ok = FALSE, message = "No sync URL configured (DAFS_TEACH_SYNC_URL not set)."))

  payload <- list(
    deadline_id = deadline_id,
    rows = lapply(seq_len(nrow(rows)), function(i) list(
      lesson_id      = rows$lesson_id[i],
      exercise_id    = rows$exercise_id[i],
      completed      = rows$completed[i] == 1,
      attempts       = rows$attempts[i],
      solution_views = rows$solution_views[i],
      lesson_title   = lesson_titles[[rows$lesson_id[i]]] %||% rows$lesson_id[i]
    ))
  )
  tryCatch({
    httr2::request(url) |>
      httr2::req_body_json(payload, auto_unbox = TRUE) |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform()
    list(ok = TRUE, message = sprintf(
      "Sent an anonymous progress summary for '%s' to your instructor (%d exercise records, no names included).",
      deadline_id, nrow(rows)))
  }, error = function(e) list(ok = FALSE, message = paste("Report failed:", conditionMessage(e))))
}

# Returns a list of list(level, message) notices to show. Safe to call on
# every launch: sent deadlines are skipped, failed sends are retried next time.
check_and_send_due_deadlines <- function(db, deadlines, lesson_titles) {
  if (is.null(sync_url()) || !length(deadlines)) return(list())
  notices <- list()
  for (dl in deadlines) {
    if (is.na(dl$deadline) || Sys.time() < dl$deadline) next
    if (has_sent_deadline(db, dl$id)) next

    rows <- get_all_rows_any_student(db)
    rows <- rows[rows$lesson_id %in% dl$lessons, , drop = FALSE]
    if (!nrow(rows)) {
      # Nothing done for these lessons: nothing to report, no point retrying
      mark_deadline_sent(db, dl$id)
      next
    }
    res <- send_anonymous_report(dl$id, rows, lesson_titles)
    if (res$ok) {
      mark_deadline_sent(db, dl$id)
      notices[[length(notices) + 1]] <- list(level = "message", message = res$message)
    } else {
      notices[[length(notices) + 1]] <- list(level = "error", message = sprintf(
        "Deadline '%s' passed but the report couldn't be sent yet: %s", dl$id, res$message))
    }
  }
  notices
}
