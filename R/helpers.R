done <- function(...) cli::cat_bullet(..., bullet = "tick", bullet_col = "green")

`%||%` <- function(x, y) if (is.null(x)) y else x

suggest_package <- function(pkg, fname = deparse(sys.calls()[[sys.nframe()-1]])) {
  force(fname)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::warn(glue::glue("{fname} suggests that you install.packages(\"{pkg}\")"))
    return(FALSE)
  } else TRUE
}

require_package <- function(pkg, fname = deparse(sys.calls()[[sys.nframe()-1]])) {
  force(fname)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(glue::glue("{fname} requires install install.packages(\"{pkg}\")"))
  }
}
