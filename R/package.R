#' Use Default .gitignore File
#'
#' Copies the grkmisc default .gitignore file into the provided directory.
#'
#' @param directory Directory for the .gitignore, defaults to project root.
#' @param browse Open the .gitignore file after modifying?
#' @export
use_gitignore <- function(directory = here::here(), browse = TRUE) {
  proj_gitignore <- file.path(directory, ".gitignore")
  default_gitignore <- system.file("templates", ".gitignore", package = "grkmisc")
  append <- file.exists(proj_gitignore)
  cat(readLines(default_gitignore), sep = "\n", file = proj_gitignore, append = append)
  if (browse && rstudioapi::isAvailable()) rstudioapi::navigateToFile(proj_gitignore)
  if (append) {
    cli::cat_bullet("Added default .gitignore lines to existing file in ", directory)
  } else cli::cat_bullet("Added default .gitignore in ", directory)
  return(invisible())
}

#' Use grkmisc git Pre-Commit Hook
#'
#' Installs a pre-commit hook that automatically bumps the lowest part of the
#' package version before committing - for example from `0.1.2.9000` to
#' `0.1.2.90001` - and tidies up the DESCRIPTION file using
#' [usethis::use_tidy_description]. If the version was manually changed as part
#' of the commit, the version number is not changed. If changes were made to the
#' DESCRIPTION file but this file was not committed, those additional changes
#' will be committed with the version bump.
#'
#' @section Disabling the hook:
#' You can disable the pre-commit hook in two ways. First you can set an
#' environment variable when committing your files:
#' `doIncrement=FALSE git commit -m "commit message"`. Alternatively, you can
#' skip the pre-commit hook with `git commit --no-verify -m "commit message"`.
#'
#' @param directory Root directory containing the git repository
#' @export
use_git_hook_precommit <- function(directory = here::here()) {
  if (!dir.exists(file.path(directory, ".git"))) rlang::abort(
    "This project is not contained in a git repository. You may need to manually specify the `directory`."
  )
  git_hooks_dir <- file.path(directory, ".git", "hooks")
  git_precommit_file <- file.path(git_hooks_dir, "pre-commit")
  if (file.exists(git_precommit_file)) {
   cli::cat_bullet(
     aste("A pre-commit hook already exists in", git_hooks_dir),
     bullet = "cross", bullet_col = "red")
    return(invisible())
  }
  file.copy(system.file("templates", "pre-commit.R", package = "grkmisc"),
            git_precommit_file)
  cli::cat_bullet("Added grkmisc pre-commit hook")
  return(invisible())
}
