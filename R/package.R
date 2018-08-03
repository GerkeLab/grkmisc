#' Use Default .gitignore File
#'
#' Copies the grkmisc default .gitignore file into the provided directory.
#'
#' @param directory Directory for the .gitignore, defaults to project root.
#' @param browse Open the .gitignore file after modifying?
#' @export
use_gitignore <- function(directory = NULL, browse = TRUE) {
  if (is.null(directory)) directory <- here::here()
  proj_gitignore <- file.path(directory, ".gitignore")
  default_gitignore <- system.file("templates", ".gitignore", package = "grkmisc")
  append <- file.exists(proj_gitignore)
  if (append) cli::cat_line("Adding default .gitignore to existing file in ", directory)
  cat(readLines(default_gitignore), sep = "\n", file = proj_gitignore, append = append)
  if (browse && rstudioapi::isAvailable()) rstudioapi::navigateToFile(proj_gitignore)
}
