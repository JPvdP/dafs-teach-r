# Same SQLite layout as the Python app's progress tracker.

with_db <- function(db, f) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))
  f(con)
}

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

init_db <- function(db) {
  with_db(db, function(con) {
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS progress (
        student_name   TEXT,
        lesson_id      TEXT,
        exercise_id    TEXT,
        completed      INTEGER DEFAULT 0,
        attempts       INTEGER DEFAULT 0,
        solution_views INTEGER DEFAULT 0,
        last_code      TEXT,
        timestamp      TEXT,
        PRIMARY KEY (student_name, lesson_id, exercise_id)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS sent_deadlines (
        deadline_id TEXT PRIMARY KEY,
        sent_at     TEXT
      )")
  })
  invisible(db)
}

record_attempt <- function(db, student, lesson_id, exercise_id, code, passed) {
  with_db(db, function(con) DBI::dbExecute(con, "
    INSERT INTO progress (student_name, lesson_id, exercise_id, completed, attempts, last_code, timestamp)
    VALUES (?, ?, ?, ?, 1, ?, ?)
    ON CONFLICT(student_name, lesson_id, exercise_id) DO UPDATE SET
      completed = MAX(completed, excluded.completed),
      attempts  = attempts + 1,
      last_code = excluded.last_code,
      timestamp = excluded.timestamp",
    params = list(student, lesson_id, exercise_id, as.integer(passed), code, now_iso())))
}

record_solution_view <- function(db, student, lesson_id, exercise_id) {
  with_db(db, function(con) DBI::dbExecute(con, "
    INSERT INTO progress (student_name, lesson_id, exercise_id, completed, attempts, solution_views, last_code, timestamp)
    VALUES (?, ?, ?, 0, 0, 1, '', ?)
    ON CONFLICT(student_name, lesson_id, exercise_id) DO UPDATE SET
      solution_views = solution_views + 1,
      timestamp      = excluded.timestamp",
    params = list(student, lesson_id, exercise_id, now_iso())))
}

get_completed <- function(db, student, lesson_id) {
  with_db(db, function(con) DBI::dbGetQuery(con, "
    SELECT exercise_id FROM progress
    WHERE student_name = ? AND lesson_id = ? AND completed = 1",
    params = list(student, lesson_id))$exercise_id)
}

get_last_code <- function(db, student, lesson_id, exercise_id) {
  rows <- with_db(db, function(con) DBI::dbGetQuery(con, "
    SELECT last_code FROM progress
    WHERE student_name = ? AND lesson_id = ? AND exercise_id = ?",
    params = list(student, lesson_id, exercise_id)))
  if (nrow(rows) && !is.na(rows$last_code[1]) && nzchar(rows$last_code[1])) rows$last_code[1] else NULL
}

# Data frame with one row per exercise this student has touched
get_all_progress <- function(db, student) {
  with_db(db, function(con) DBI::dbGetQuery(con, "
    SELECT lesson_id, exercise_id, completed, attempts, solution_views
    FROM progress WHERE student_name = ?", params = list(student)))
}

# All rows regardless of student, without names: used for anonymous reporting
get_all_rows_any_student <- function(db) {
  with_db(db, function(con) DBI::dbGetQuery(con, "
    SELECT lesson_id, exercise_id, completed, attempts, solution_views FROM progress"))
}

has_sent_deadline <- function(db, deadline_id) {
  with_db(db, function(con) nrow(DBI::dbGetQuery(con,
    "SELECT 1 FROM sent_deadlines WHERE deadline_id = ?", params = list(deadline_id))) > 0)
}

mark_deadline_sent <- function(db, deadline_id) {
  with_db(db, function(con) DBI::dbExecute(con,
    "INSERT OR REPLACE INTO sent_deadlines (deadline_id, sent_at) VALUES (?, ?)",
    params = list(deadline_id, now_iso())))
}
