# Every exercise should be solvable, and none should pass without the
# student doing anything. Run with devtools::test() after editing lessons.

lessons <- load_lessons()

test_that("all lessons load and exercise ids are unique within a lesson", {
  expect_gt(length(lessons), 0)
  for (lesson in lessons) {
    ids <- vapply(lesson$exercises, `[[`, "", "id")
    expect_false(anyDuplicated(ids) > 0, label = paste(lesson$id, "has duplicate exercise ids"))
  }
})

for (lesson in lessons) {
  for (ex in lesson$exercises) {
    if (!identical(ex$type, "code")) next
    test_that(paste(lesson$id, ex$id, "- solution passes"), {
      withr::local_dir(withr::local_tempdir())   # exercises that write files do so in a temp folder
      result <- run_code(ex$solution, ex$tests)
      expect_true(result$passed, label = result$feedback)
    })
    test_that(paste(lesson$id, ex$id, "- starter code fails"), {
      withr::local_dir(withr::local_tempdir())
      expect_false(run_code(ex$starter_code, ex$tests)$passed)
    })
  }
}

test_that("completion codes match the Python app", {
  expect_equal(generate_code("Alice  Example", "01_r_basics"), "B3171-5AAD8")
  expect_equal(generate_code("Jan Pieter", "01_python_basics", secret = "s3cret"), "1EB4F-4EBA3")
  expect_true(verify_code("alice example", "01_r_basics", "b3171-5aad8", secret = "dafs-teach-default-secret-change-me"))
})

test_that("outdated or missing lesson packages are detected", {
  expect_equal(outdated_packages(), character(0))
  expect_equal(outdated_packages(c(dplyr = "999.0.0", readr = "0.1.0")), "dplyr")
  expect_equal(outdated_packages(c(notapackage = "1.0.0")), "notapackage")
  expect_match(outdated_message(c("dplyr", "readr")), 'install.packages(c("dplyr", "readr"))', fixed = TRUE)
})

test_that("lessons are numbered and every lesson belongs to exactly one week", {
  lessons <- load_lessons()
  deadlines <- load_deadlines(file.path(content_dir(), "deadlines.yaml"))
  expect_equal(lesson_label(lessons[[5]]), "Lesson 5: Printing, Text and Functions")
  weeks <- lessons_by_week(lessons, deadlines)
  expect_equal(weeks[[1]]$heading, "Week 1 · deadline Wednesday 25 November 2026")
  placed <- unlist(lapply(weeks, function(w) vapply(w$lessons, `[[`, "", "id")))
  expect_setequal(placed, vapply(lessons, `[[`, "", "id"))
  expect_false(anyDuplicated(placed) > 0)
  expect_false("More lessons" %in% vapply(weeks, `[[`, "", "heading"))
})

test_that("package startup messages stay out of the output when the app runs in RStudio", {
  # RStudio sets this for its R session; student code runs in a child process
  withr::local_envvar(RSTUDIO = "1", RSTUDIO_SESSION_PID = Sys.getpid())
  ex <- load_lessons()[[1]]$exercises[[1]]
  expect_true(run_code(ex$solution, ex$tests)$passed)
})

test_that("install.packages() in student code explains where to install instead", {
  res <- run_code('install.packages("writexl")\nprint(1)', list())
  expect_false(res$passed)
  expect_match(res$feedback, 'run install.packages("writexl") in the Console', fixed = TRUE)
  expect_true(run_code("x <- 1\nprint(ls())", list(list(type = "output_equals", value = '[1] "x"')))$passed)
})

test_that("help() prints text help even where R defaults to HTML help (Windows)", {
  profile <- withr::local_tempfile(lines = 'options(help_type = "html")')
  withr::local_envvar(R_PROFILE = profile)   # site profile, read even with --no-init-file
  ex <- load_lessons()[[4]]$exercises[[2]]
  res <- run_code(ex$solution, ex$tests)
  expect_true(res$passed)
  expect_no_match(res$stderr, "httpd")
})
