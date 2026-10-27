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
