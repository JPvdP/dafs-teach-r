`%||%` <- function(x, y) if (is.null(x)) y else x

content_dir <- function() system.file("content", package = "dafsteach")

assets_dir <- function() system.file("assets", package = "dafsteach")

# Progress is stored per user, in the standard place R packages keep data
# (e.g. ~/Library/Application Support/org.R-project.R/R/dafsteach on macOS).
data_dir <- function() {
  dir <- tools::R_user_dir("dafsteach", which = "data")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
