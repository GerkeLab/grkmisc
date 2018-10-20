done <- function(...) cli::cat_bullet(..., bullet = "tick", bullet_col = "green")

`%||%` <- function(x, y) if (is.null(x)) y else x
