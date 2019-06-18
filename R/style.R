#' The grkmisc Code Style
#'
#' Use [styler::style_text()] to format code according to the unofficial
#' \pkg{grkmisc} style guide.
#'
#' @examples
#' \dontrun{
#' use_grk_style()
#' # Use styler addins
#' styler:::style_selection()
#' }
#'
#' grk_style_text(
#'   "x = paste(\n'a', 'b')",
#'   transformers = grk_style_transformer()
#' )
#'
#' @param ... Arguments passed to underling \pkg{styler} functions (identified
#'   by removing the `grk_` prefix)
#' @name grk_style
NULL

#' @describeIn grk_style A code transformer for use with [styler::style_text()]
#' @export
grk_style_transformer <- function(...) {
  require_styler()
  tidy_style <- styler::tidyverse_style(...)
  tidy_style$line_break$set_linebreak_each_argument_if_multi_line <- function(pd) {
    if (!(any(pd$token == "','"))) {
      return(pd)
    }
    # does this expression contain expressions with linebreaks?
    has_children <- purrr::some(pd$child, purrr::negate(is.null))
    has_internal_linebreak <- FALSE
    is_function_definition <- pd$token[1] == "FUNCTION"
    if (has_children && !is_function_definition) {
      has_internal_linebreak <- pd$child %>%
        purrr::discard(is.null) %>%
        purrr::map_int(~ sum(.$newlines, .$lag_newlines)) %>%
        any(. > 0)
    }

    if (!has_internal_linebreak && sum(pd$newlines, pd$lag_newlines) < 1) {
      return(pd)
    }

    idx_comma <- which(pd$token == "','")
    idx_open_paren <- grep("'[[(]'", pd$token)
    idx_close_paren <- grep("'(]|\\))'", pd$token)
    pd[idx_comma + 1L, "lag_newlines"] <- 1L
    if (length(idx_open_paren)) {
      pd[idx_open_paren[1] + 1L, "lag_newlines"] <- 1L
    }
    if (length(idx_close_paren)) {
      pd[idx_close_paren[length(idx_close_paren)], "lag_newlines"] <- 1L
    }
    pd
  }
  tidy_style
}

#' @describeIn grk_style Set the \pkg{grkmisc} style as the default style for
#'   \pkg{styler} addins.
#' @export
use_grk_style <- function() {
  require_styler()
  options(styler.addins_style_transformer = "grkmisc::grk_style_transformer()")
}

#' @describeIn grk_style Style a file using the \pkg{grkmisc} code style
#' @inheritParams styler::style_file
#' @export
grk_style_file <- function(path, ..., transformers = grk_style_transformer()) {
  require_styler()
  styler::style_file(path, ..., transformers = transformers)
}

#' @describeIn grk_style Style a directory using the \pkg{grkmisc} code style
#' @export
grk_style_dir <- function(path, ..., transformers = grk_style_transformer()) {
  require_styler()
  styler::style_dir(path, ..., transformers = transformers)
}

#' @describeIn grk_style Style a package using the \pkg{grkmisc} code style
#' @inheritParams styler::style_pkg
#' @export
grk_style_pkg <- function(pkg, ..., transformers = grk_style_transformer()) {
  require_styler()
  styler::style_pkg(pkg, ..., transformers = transformers)
}

require_styler <- function() {
  if (!requireNamespace("styler", quietly = TRUE)) {
    stop("`styler` is required: install.packages('styler')")
  }

}
