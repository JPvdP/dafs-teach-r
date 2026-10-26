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
