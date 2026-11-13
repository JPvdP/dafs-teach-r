# Lessons are YAML files in inst/content/lessons/, in the same format as the
# Python app. The filename (without .yaml) is the lesson id, and controls the order.
load_lessons <- function(dir = content_dir()) {
  files <- sort(list.files(file.path(dir, "lessons"), pattern = "\\.ya?ml$", full.names = TRUE))
  lapply(files, function(path) {
    data <- yaml::read_yaml(path)
    exercises <- lapply(data$exercises, function(ex) {
      list(
        id           = ex$id,
        title        = ex$title,
        instructions = ex$instructions,
        starter_code = ex$starter_code %||% "",        # 'explain': template for the explanation
        tests        = ex$tests %||% list(),
        hints        = as.character(unlist(ex$hints)),
        solution     = ex$solution %||% "",             # 'explain': model explanation (markdown)
        type         = ex$type %||% "code",             # 'code' | 'explain'
        script       = ex$script %||% "",               # 'explain': the script to read
        min_words    = ex$min_words %||% 30             # 'explain': words required beyond the template
      )
    })
    list(
      id          = tools::file_path_sans_ext(basename(path)),
      title       = data$title,
      description = data$description,
      exercises   = exercises
    )
  })
}

# Lesson ids start with their number ("05_print_and_functions"), so the number
# students see always matches the order in the lessons folder.
lesson_label <- function(lesson) {
  sprintf("Lesson %d: %s", as.integer(sub("_.*", "", lesson$id)), lesson$title)
}

# English date like "Wednesday 25 November 2026", whatever the student's locale.
format_deadline <- function(time) {
  weekdays_en <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  lt <- as.POSIXlt(time)
  sprintf("%s %d %s %d", weekdays_en[lt$wday + 1], lt$mday, month.name[lt$mon + 1], lt$year + 1900)
}

# Groups lessons under their weekly deadline, in deadline order, as a list of
# list(heading, lessons). Lessons that belong to no deadline come last.
lessons_by_week <- function(lessons, deadlines) {
  ids <- vapply(lessons, `[[`, "", "id")
  by_id <- stats::setNames(lessons, ids)
  deadlines <- deadlines[order(vapply(deadlines, function(d) as.numeric(d$deadline), numeric(1)))]
  groups <- lapply(seq_along(deadlines), function(week) {
    dl <- deadlines[[week]]
    list(heading = sprintf("Week %d · deadline %s", week, format_deadline(dl$deadline)),
         lessons = unname(by_id[intersect(dl$lessons, ids)]))
  })
  placed <- unlist(lapply(deadlines, `[[`, "lessons"))
  rest <- lessons[!ids %in% placed]
  if (length(rest)) groups <- c(groups, list(list(heading = "More lessons", lessons = rest)))
  groups
}
