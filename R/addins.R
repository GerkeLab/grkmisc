#' Style text from console
#'
#' @param x Text to style
#' @inheritDotParams styler::style_text
#' @export
style_console <- function(x, ...) {
  x <- paste(x, collapse = "\n")
  if (!grepl("\n", x)) {
    if (grepl("%>%", x)) {
      x <- sub("%>%", "%>%\n", x)
    } else if (grepl("<-", x)) {
      x <- sub("<-", "<-\n", x)
    }
  }
  if (grepl("ggplot", x)) {
    # Add newline after `+` after finding start of ggplot()
    ggplot_starts_at <- stringr::str_locate(x, "ggplot")[1, 'start']
    x <- paste0(substr(x, 1, ggplot_starts_at - 1),
                gsub("\\+", "\\+\n", substring(x, ggplot_starts_at)))
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

#' Insert Path Containing Active File
#'
#' For the "Insert Relative Directory Path" RStudio Addin. Inserts the relative
#' path to the directory containing the active source document. If text is
#' selected, this text is assumed to be a file inside that directory and is
#' appended to the path. The "Insert Absolute Directory Path" Rstudio Addin does
#' the same but provides the absolute path to the active source document.
insert_path_dir_active <- function(relative = TRUE) {
  context <- rstudioapi::getSourceEditorContext()
  this_path <- if (relative) {
    fs::path_rel(fs::path_dir(context$path), here::here())
  } else {
    fs::path_dir(fs::path_abs(context$path))
  }
  selected <- context$selection[[1]]
  rstudioapi::insertText(selected$range, fs::path(this_path, selected$text), context$id)
}

#' @rdname insert_path_dir_active
insert_path_dir_active_abs <- function() {
  insert_path_dir_active(FALSE)
}
