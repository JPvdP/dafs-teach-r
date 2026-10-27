# The lessons run on the student's own tidyverse, which is only a suggested
# dependency: an old installation is kept when the student declines updates
# during install_github(). These are the oldest versions the lessons work with:
# - tidyverse 2.0.0 attaches lubridate (ymd() and year() in lessons 14-15)
# - dplyr 1.1.0 has case_when(.default = ) and summarise(.by = )
# - readr 2.0.0 reads literal data wrapped in I() (lesson 8)
required_versions <- c(tidyverse = "2.0.0", dplyr = "1.1.0", readr = "2.0.0")

# Names of required packages that are missing or older than required.
outdated_packages <- function(required = required_versions) {
  ok <- vapply(names(required), function(pkg) {
    nzchar(system.file(package = pkg)) &&
      utils::packageVersion(pkg) >= package_version(required[[pkg]])
  }, logical(1))
  names(required)[!ok]
}

outdated_message <- function(pkgs) {
  paste0(
    "Some packages the lessons need are missing or outdated: ", paste(pkgs, collapse = ", "), ". ",
    "Stop the app and run this in the R console, then start the app again:\n",
    "install.packages(c(", paste0('"', pkgs, '"', collapse = ", "), "))"
  )
}
