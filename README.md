# DAFS Teaching App — R Track

An interactive R learning app for the **Data Analytics For Sustainability** course. Students work through lessons, write real R code, and get instant feedback — all from their browser, started from R.

This is the R version of the [Python teaching app](https://github.com/JPvdP/dafs-teach). Both use the same lesson format, the same completion codes and the same anonymous progress reporting.

## For students

A step-by-step guide for students, including troubleshooting, is in [`student_guide.qmd`](student_guide.qmd) (render it with Quarto to get a single HTML file to share).

### Installation

You need R 4.1 or higher (and RStudio is recommended). In the R console, run:

```r
install.packages("remotes")
remotes::install_github("JPvdP/dafs-teach-r", dependencies = TRUE)
```

### Running the app

```r
dafsteach::run_app()
```

The app opens in your browser (or in the RStudio Viewer). Your progress is saved on your own computer and will be there next time you open it. To stop the app, press **Esc** in the console, or the red **Stop** button in RStudio.

### Updating

When new lessons are released, run the installation command again.

---

## For instructors

### Adding a lesson

Lessons live in `inst/content/lessons/`. The format is the same as in the Python app, except that the code is R and `assert` checks use `.check(condition, "message shown when it fails")`:

```yaml
exercises:
  - id: unique_exercise_id
    title: Exercise Title
    instructions: |
      Markdown instructions.
    starter_code: |
      x <-
    tests:
      - type: output_equals          # or output_contains
        value: "[1] 42"
      - type: assert
        code: |
          .check(x == 42, "x should be 42")
        description: "x should equal 42"
    hints:
      - "First hint"
    solution: |
      x <- 42
      print(x)
```

"Explain the script" exercises (`type: explain`) work exactly as in the Python app.

After editing lessons, check that every solution passes and no starter code passes on its own:

```r
devtools::test()
```

### Checking completion codes

```r
Sys.setenv(DAFS_TEACH_SECRET = "your-private-secret")
dafsteach::verify_code("Alice Example", "01_r_basics", "ABCDE-12345")
```

The scheme is identical to the Python app, so `dafs-teach verify` from the Python package works for R lesson ids too. As in the Python app, set your own `DAFS_TEACH_SECRET` before relying on codes for grading, and give students the same value.

### Anonymous progress reporting

Works exactly as in the Python app, with the same `DAFS_TEACH_SYNC_URL` Google Apps Script endpoint. Deadlines are in `inst/content/deadlines.yaml`; their ids start with `r-` so R reports can be told apart in the sheet.

### Local development

```r
devtools::load_all()
dafsteach:::dafs_app()   # or run_app()
```
