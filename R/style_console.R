#' Style text from console
#'
#' @param x Text to style
#' @inheritDotParams styler::style_text
#' @export
style_console <- function(x, ...) {
  if (!grepl("\n", x)) {
    if (grepl("%>%", x)) {
      x <- sub("%>%", "%>%\n", x)
    } else if (grepl("<-", x)) {
      x <- sub("<-", "<-\n", x)
    }
  }
  x <- strsplit(x, "\n")[[1]]
  styler::style_text(x, ...)
}

#' Insert Styled Text
#'
#' Inserts text from clipboard at cursor position, styled according to the
#' [tidyverse style guide](http://style.tidyverse.org/). Powers the RStudio
#' addin.
#'
#' @inheritParams style_console
insert_styled_text <- function(x = NULL, ...) {
  if (!requireNamespace("clipr", quietly = TRUE)) rlang::abort("Please install `clipr`")
  if (!clipr::clipr_available()) rlang::abort("clipr is unable to read from the clipboard")
  if (is.null(x)) x <- clipr::read_clip()
  x <- style_console(x, ...)
  x <- paste(x, collapse ="\n")
  context <- rstudioapi::getSourceEditorContext()
  rstudioapi::insertText(context$selection[[1]]$range, x, context$id)
}
