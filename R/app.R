COURSE_BANNER <- "Data Analytics For Sustainability || R Track || 2026"

#' Start the DAFS teaching app
#'
#' Opens the app in your browser (or in the RStudio Viewer). Your progress is
#' saved on your own computer. Stop the app with Esc in the R console, or the
#' red Stop button in RStudio.
#'
#' @param launch.browser Open the app automatically.
#' @export
run_app <- function(launch.browser = getOption("shiny.launch.browser", interactive())) {
  shiny::runApp(dafs_app(), launch.browser = launch.browser)
}

dafs_app <- function() {
  lessons <- load_lessons()
  db <- file.path(data_dir(), "progress.db")
  init_db(db)
  deadlines <- load_deadlines(file.path(content_dir(), "deadlines.yaml"))
  shiny::addResourcePath("dafs-assets", assets_dir())
  shiny::shinyApp(app_ui(), app_server(lessons, db, deadlines))
}

# ── Small HTML helpers ────────────────────────────────────────────────────────

md <- function(text) shiny::HTML(shiny::markdown(text))

# Button that sends `value` to input$<input_id> (as an event), so we don't
# need one observer per dynamically created button.
event_button <- function(label, input_id, value, class = "btn-outline-secondary", disabled = FALSE, title = NULL) {
  htmltools::tags$button(
    type = "button", class = paste("btn w-100", class), title = title,
    disabled = if (disabled) NA else NULL,
    onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})",
                      input_id, jsonlite::toJSON(value, auto_unbox = TRUE)),
    label
  )
}

numbered_script <- function(code) {
  lines <- strsplit(sub("\n$", "", code), "\n", fixed = TRUE)[[1]]
  rows <- sprintf('<span class="ln">%d</span>%s', seq_along(lines), htmltools::htmlEscape(lines))
  shiny::HTML(paste0('<pre class="script"><code>', paste(rows, collapse = "\n"), "</code></pre>"))
}

progress_bar <- function(done, total) {
  pct <- if (total > 0) round(100 * done / total) else 0
  htmltools::tagList(
    htmltools::div(class = "progress", style = "height: 8px;",
      htmltools::div(class = "progress-bar", role = "progressbar", style = sprintf("width: %d%%;", pct))),
    htmltools::div(class = "small text-muted mt-1", sprintf("%d/%d exercises completed", done, total))
  )
}

app_css <- "
#code { font-family: 'Courier New', Courier, monospace; font-size: 14px; }
pre.script { counter-reset: none; background: var(--bs-tertiary-bg); padding: .75rem; border-radius: .375rem; }
pre.script .ln { display: inline-block; width: 2em; margin-right: .75em; text-align: right;
                 color: var(--bs-secondary-color); user-select: none; }
.instructions pre, .feedback pre, .solution pre { background: var(--bs-tertiary-bg); padding: .75rem; border-radius: .375rem; }
.instructions table { margin-bottom: 1rem; }
.instructions table td, .instructions table th { padding: .25rem .6rem; border: 1px solid var(--bs-border-color); }
.course-banner { color: var(--bs-secondary-color); font-size: .85rem; margin-bottom: .25rem; }
.nav-exercise { text-align: left; margin-bottom: .25rem; }
"

# ── UI ────────────────────────────────────────────────────────────────────────

app_ui <- function() {
  bslib::page_sidebar(
    title = htmltools::tags$span(
      htmltools::tags$img(src = "dafs-assets/logo_uu_en.png", height = "34px", class = "me-3"),
      "DAFS Teaching App"
    ),
    window_title = "DAFS Teaching App",
    theme = bslib::bs_theme(version = 5, primary = "#C00A35"),
    sidebar = bslib::sidebar(width = 300, shiny::uiOutput("sidebar")),
    htmltools::tags$head(
      htmltools::tags$link(rel = "icon", type = "image/png", href = "dafs-assets/logo_uu_seal.png"),
      htmltools::tags$style(app_css)
    ),
    shiny::useBusyIndicators(),
    shiny::uiOutput("main")
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

app_server <- function(lessons, db, deadlines) {
  lesson_ids <- vapply(lessons, `[[`, "", "id")
  lessons_by_id <- stats::setNames(lessons, lesson_ids)
  lesson_titles <- stats::setNames(lapply(lessons, `[[`, "title"), lesson_ids)

  function(input, output, session) {
    rv <- shiny::reactiveValues(
      student = NULL, lesson_id = NULL, ex_idx = 1L, last_result = NULL,
      hints_shown = 0L, show_solution = FALSE, progress_version = 0L
    )
    code_cache <- new.env()   # unsent editor contents per exercise (not reactive on purpose)

    # Anonymous deadline reporting, once per app start
    for (notice in check_and_send_due_deadlines(db, deadlines, lesson_titles)) {
      shiny::showNotification(notice$message, type = notice$level, duration = 15)
    }

    current_lesson <- shiny::reactive(lessons_by_id[[rv$lesson_id]])
    current_exercise <- shiny::reactive(current_lesson()$exercises[[rv$ex_idx]])
    code_key <- function(lesson_id, ex_id) paste(lesson_id, ex_id, sep = "__")
    completed_ids <- shiny::reactive({
      rv$progress_version
      get_completed(db, rv$student, rv$lesson_id)
    })

    reset_exercise_state <- function() {
      rv$last_result <- NULL
      rv$hints_shown <- 0L
      rv$show_solution <- FALSE
    }
    go_to_exercise <- function(idx) {
      rv$ex_idx <- as.integer(idx)
      reset_exercise_state()
    }
    go_home <- function() {
      rv$lesson_id <- NULL
      reset_exercise_state()
    }

    # Remember what's in the editor, so switching exercises doesn't lose it
    shiny::observeEvent(input$code, {
      if (!is.null(rv$lesson_id)) {
        ex <- shiny::isolate(current_exercise())
        assign(code_key(rv$lesson_id, ex$id), input$code, envir = code_cache)
      }
    })

    get_code <- function(lesson_id, ex) {
      key <- code_key(lesson_id, ex$id)
      if (exists(key, envir = code_cache, inherits = FALSE)) return(get(key, envir = code_cache))
      get_last_code(db, rv$student, lesson_id, ex$id) %||% ex$starter_code
    }

    # ── Name entry
    shiny::observeEvent(input$start, {
      name <- trimws(input$name_input %||% "")
      if (nzchar(name)) rv$student <- name
    })

    # ── Navigation events
    shiny::observeEvent(input$open_lesson, {
      lesson <- lessons_by_id[[input$open_lesson]]
      done <- get_completed(db, rv$student, lesson$id)
      ex_ids <- vapply(lesson$exercises, `[[`, "", "id")
      first_open <- which(!ex_ids %in% done)
      rv$lesson_id <- lesson$id
      go_to_exercise(if (length(first_open)) first_open[1] else 1L)
    })
    shiny::observeEvent(input$go_home, go_home())
    shiny::observeEvent(input$go_exercise, go_to_exercise(input$go_exercise))

    shiny::observeEvent(input$show_hint, rv$hints_shown <- rv$hints_shown + 1L)

    shiny::observeEvent(input$toggle_solution, {
      ex <- current_exercise()
      if (!rv$show_solution) record_solution_view(db, rv$student, rv$lesson_id, ex$id)
      rv$show_solution <- !rv$show_solution
      rv$progress_version <- rv$progress_version + 1L
    })

    # ── Run code / submit explanation
    shiny::observeEvent(input$run, {
      ex <- current_exercise()
      code <- input$code %||% ""
      if (identical(ex$type, "explain")) {
        n_words <- length(strsplit(trimws(code), "\\s+")[[1]]) -
          length(strsplit(trimws(ex$starter_code), "\\s+")[[1]])
        passed <- n_words >= ex$min_words
        feedback <- if (passed) {
          # Revealing it here is part of the exercise, so it isn't counted as a solution view
          rv$show_solution <- TRUE
          paste("Explanation saved! Now compare it with the model answer: did you describe",
                "every step, and understand each new function correctly?")
        } else {
          sprintf(paste("Your explanation is still quite short (%d words). Write at least %d words:",
                        "describe what each part of the script does, and what it prints at the end."),
                  max(n_words, 0L), ex$min_words)
        }
        result <- run_result("", "", FALSE, passed, feedback)
      } else {
        result <- run_code(code, ex$tests)
      }
      record_attempt(db, rv$student, rv$lesson_id, ex$id, code, result$passed)
      rv$last_result <- result
      rv$progress_version <- rv$progress_version + 1L
    })

    # ── Sidebar
    output$sidebar <- shiny::renderUI({
      rv$progress_version
      if (is.null(rv$student)) {
        return(htmltools::p(class = "text-muted",
          "Learn R for data analysis, one exercise at a time. Enter your name to get started."))
      }
      header <- htmltools::tagList(
        htmltools::p(htmltools::strong("Student: "), rv$student),
        event_button("← Home", "go_home", TRUE),
        htmltools::hr()
      )
      if (!is.null(rv$lesson_id)) {
        lesson <- current_lesson()
        done <- completed_ids()
        items <- lapply(seq_along(lesson$exercises), function(i) {
          ex <- lesson$exercises[[i]]
          icon <- if (ex$id %in% done) "✅" else if (i == rv$ex_idx) "▶️" else "⭕"
          event_button(paste(icon, ex$title), "go_exercise", i, class = "btn-light nav-exercise")
        })
        htmltools::tagList(header, htmltools::strong(lesson$title), htmltools::div(class = "mt-2", items))
      } else {
        progress <- get_all_progress(db, rv$student)
        items <- lapply(lessons, function(lesson) {
          ex_ids <- vapply(lesson$exercises, `[[`, "", "id")
          n_done <- sum(ex_ids %in% progress$exercise_id[progress$lesson_id == lesson$id & progress$completed == 1])
          icon <- if (n_done == length(ex_ids)) "✅" else "⭕"
          htmltools::div(sprintf("%s %s (%d/%d)", icon, lesson$title, n_done, length(ex_ids)))
        })
        htmltools::tagList(header, htmltools::strong("Lessons"), htmltools::div(class = "mt-2", items))
      }
    })

    # ── Main area
    output$main <- shiny::renderUI({
      if (is.null(rv$student)) return(render_name_entry())
      if (is.null(rv$lesson_id)) return(render_lesson_browser())
      render_exercise()
    })

    render_name_entry <- function() {
      htmltools::div(class = "container", style = "max-width: 640px; margin-top: 3rem;",
        htmltools::h1("Welcome to DAFS Teaching App"),
        htmltools::p(class = "lead", "Learn R for data analysis, one exercise at a time."),
        shiny::textInput("name_input", "Enter your name to get started:", placeholder = "e.g. Alice", width = "100%"),
        shiny::actionButton("start", "Start learning", class = "btn-primary")
      )
    }

    render_lesson_browser <- function() {
      rv$progress_version
      progress <- get_all_progress(db, rv$student)
      cards <- lapply(lessons, function(lesson) {
        rows <- progress[progress$lesson_id == lesson$id, , drop = FALSE]
        ex_ids <- vapply(lesson$exercises, `[[`, "", "id")
        n_done <- sum(ex_ids %in% rows$exercise_id[rows$completed == 1])
        n_total <- length(ex_ids)
        extra <- NULL
        if (n_total > 0 && n_done == n_total) {
          n_viewed <- sum(ex_ids %in% rows$exercise_id[rows$solution_views > 0])
          extra <- htmltools::tagList(
            if (n_viewed > 0) htmltools::p(class = "small text-muted mt-2", sprintf(paste(
              "\U0001F4A1 You viewed the solution on %d/%d exercises in this lesson — worth",
              "revisiting those on your own to make sure it sticks."), n_viewed, n_total)),
            htmltools::tags$details(class = "mt-2",
              htmltools::tags$summary("\U0001F393 Get completion code"),
              htmltools::p(class = "mt-2", "Upload this code to Brightspace as proof you finished this module:"),
              htmltools::tags$pre(generate_code(rv$student, lesson$id)))
          )
        }
        bslib::card(
          bslib::card_body(
            bslib::layout_columns(col_widths = c(9, 3),
              htmltools::div(
                htmltools::h4(lesson$title),
                htmltools::p(class = "text-muted", lesson$description),
                progress_bar(n_done, n_total), extra),
              htmltools::div(class = "d-flex align-items-center",
                event_button("Open", "open_lesson", lesson$id, class = "btn-primary"))
            )
          )
        )
      })
      htmltools::tagList(htmltools::h2("Choose a Lesson"), cards)
    }

    render_exercise <- function() {
      lesson <- current_lesson()
      idx <- rv$ex_idx
      ex <- current_exercise()
      is_explain <- identical(ex$type, "explain")
      already_passed <- ex$id %in% completed_ids()
      n <- length(lesson$exercises)

      hints <- NULL
      if (length(ex$hints)) {
        hints <- htmltools::tagList(
          if (rv$hints_shown < length(ex$hints))
            shiny::actionButton("show_hint", "\U0001F4A1 Show hint", class = "btn-outline-secondary btn-sm"),
          lapply(seq_len(rv$hints_shown), function(i)
            htmltools::div(class = "alert alert-info mt-2 mb-0",
              md(sprintf("**Hint %d:** %s", i, ex$hints[[i]]))))
        )
      }

      solution <- NULL
      if (rv$show_solution && nzchar(ex$solution)) {
        solution <- if (is_explain) {
          bslib::card(class = "mt-3 solution", bslib::card_body(md(ex$solution)))
        } else {
          htmltools::tags$pre(class = "mt-3", htmltools::tags$code(ex$solution))
        }
      }

      left <- htmltools::div(class = "instructions",
        if (is_explain) htmltools::tagList(htmltools::strong("The script"), numbered_script(ex$script)),
        htmltools::strong("Instructions"),
        md(ex$instructions),
        hints
      )
      right <- htmltools::div(
        htmltools::strong(if (is_explain) "Your explanation" else "Your code"),
        shiny::textAreaInput("code", NULL, value = shiny::isolate(get_code(lesson$id, ex)),
                             rows = 16, width = "100%", resize = "vertical"),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::actionButton("run", if (is_explain) "\U0001F4DD Submit explanation" else "▶ Run",
                              class = "btn-primary w-100"),
          shiny::actionButton("toggle_solution", if (is_explain) "\U0001F441 Model answer" else "\U0001F441 Solution",
                              class = "btn-outline-secondary w-100")
        ),
        solution
      )

      result_ui <- NULL
      res <- rv$last_result
      if (!is.null(res)) {
        feedback <- if (res$timed_out) {
          htmltools::div(class = "alert alert-danger", "⏱ Time limit exceeded")
        } else if (res$passed) {
          htmltools::div(class = "alert alert-success feedback", md(paste("✅", res$feedback)))
        } else {
          htmltools::div(class = "alert alert-danger feedback", md(paste("❌", res$feedback)))
        }
        result_ui <- if (is_explain) {
          htmltools::tagList(htmltools::hr(), feedback)
        } else {
          output_block <- htmltools::tagList(
            if (nzchar(res$stdout)) htmltools::tags$pre(res$stdout)
            else if (!nzchar(res$stderr)) htmltools::p(class = "text-muted fst-italic", "(no output)"),
            if (nzchar(res$stderr)) htmltools::tags$pre(class = "text-danger-emphasis", res$stderr)
          )
          htmltools::tagList(htmltools::hr(), bslib::layout_columns(col_widths = c(6, 6),
            htmltools::div(htmltools::strong("Output"), output_block),
            htmltools::div(htmltools::strong("Feedback"), feedback)))
        }
      }

      nav <- bslib::layout_columns(col_widths = c(3, 6, 3),
        if (idx > 1) event_button("← Previous", "go_exercise", idx - 1L) else htmltools::div(),
        htmltools::div(),
        if (idx < n) {
          event_button("Next →", "go_exercise", idx + 1L, disabled = !already_passed,
                       title = "Complete this exercise to continue")
        } else {
          event_button("\U0001F3E0 Back to lessons", "go_home", TRUE)
        }
      )

      htmltools::tagList(
        htmltools::div(class = "course-banner", COURSE_BANNER),
        htmltools::h5(sprintf("%s  ›  Exercise %d/%d", lesson$title, idx, n)),
        htmltools::h3(ex$title),
        htmltools::hr(),
        bslib::layout_columns(col_widths = c(6, 6), gap = "2rem", left, right),
        result_ui,
        htmltools::hr(),
        nav
      )
    }
  }
}
