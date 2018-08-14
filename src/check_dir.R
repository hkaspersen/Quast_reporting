# Creates output directory
check_dir <- function(output_dir) {
  folder <- paste0("results_", Sys.Date())
  dir.create(file.path(output_dir, folder), showWarnings = FALSE)
}