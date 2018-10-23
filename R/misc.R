#' Pretty print scaled numbers
#'
#' @param x Vector of numbers
#' @param units Named list with lower bounds for units. Names are units or unit
#'   abbreviations.
#' @param decimal_digits Number of digits to show after the decimal point in the
#'   selected unit.
#' @examples
#' pretty_num(1234)
#' pretty_num(12345, decimal_digits = 3)
#' pretty_num(123456789)
#' pretty_num(12345678900)
#' pretty_num(c(1234, 1234567, 12345678900))
#' @export
pretty_num <- function(
  x,
  units = c("k" = 1e3, "M" = 1e6, "B" = 1e9),
  decimal_digits = 1,
  ...
) {
  if (!is.numeric(x)) {
    rlang::stop("`pretty_num()` inputs should be numeric")
  }
  if (is.null(names(units))) {
    rlang::stop("`units` must be named list of lower bounds")
  }
  if (!is.numeric(units)) {
    rlang::stop("`units` must be list of numeric lower bounds")
  }

  purrr::map_chr(x, prettify_number, units = units, decimal_digits = decimal_digits, ...)
}


prettify_number <- function(x, units = c('k' = 1000, 'M' = 1e6, "B" = 1e9), decimal_digits = 1, ...) {

  units <- sort(units)
  this_unit <- units[x >= units]
  if (!length(this_unit)) {
    this_unit <- 1
    this_name <- ""
  } else {
    this_unit <- this_unit[length(this_unit)]
    this_name <- names(this_unit)
  }

  sprintf(paste0("%0.", decimal_digits, "f%s"), x/this_unit, this_name)
}


#' Recode a value based on predicate condition
#'
#' A simple wrapper around [dplyr::recode()].
#'
#' @param x Input vector
#' @param condition Conditions upon which the replacement should occurr
#' @inheritDotParams dplyr::recode
#' @export
recode_if <- function(x, condition, ...) {
  dplyr::if_else(condition, dplyr::recode(x, ...), x)
}


#' Pipe-Capable Log Messages
#'
#' Writes a log message, suitable for use inside pipes, meaning that the first
#' argument is passed through untouched. The log message is processed using
#' [glue::glue], and you can reference the incoming data as `.data` inside the
#' log message.
#'
#' @section Options:
#'
#'   You can set the maximum debug level and the log output locations using the
#'   global options `grkmisc.log_level` and `grkmisc.log_output`. The defaults
#'   are `"INFO"` and `stdout()` respectively.
#'
#' @examples
#' library(dplyr)
#' iris %>%
#'   logger("Starting with {nrow(.data)} rows") %>%
#'   filter(Species == "setosa") %>%
#'   logger("Filtered to {nrow(.data)} rows and {ncol(.data)} columns") %>%
#'   head()
#'
#' @param .data This argument is returned as-is.
#' @param msg The log message, processed using [glue::glue]. You can refernce
#'   the data passed in via `.data`.
#' @param level The log level, default is "INFO" and possible values include
#'   "DEBUG", "INFO", "WARNING", "ERROR", and "FATAL".
#' @export
logger <- function(.data = NULL, msg = "", level = "INFO") {
  level <- logger_level(level)
  if (level < logger_level(getOption("grkmisc.log_level", "INFO"))) {
    return(invisible(.data))
  }

  msg <- glue::glue(msg)
  cli::cat_line(
    strftime(Sys.time(), "[%F %H:%M:%OS6] "),
    sprintf("%-8s", names(level)),
    msg,
    file = getOption("grkmisc.log_output", stdout())
  )
  .data
}

logger_level <- function(level) {
  levels <- c(
    "DEBUG"   = 0,
    "INFO"    = 1,
    "WARNING" = 2,
    "ERROR"   = 3,
    "FATAL"   = 4
  )
  level <- match.arg(toupper(level), names(levels), several.ok = FALSE)
  levels[level]
}
