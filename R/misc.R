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

  x <- purrr::map_chr(x, prettify_number, units = units, decimal_digits = decimal_digits, ...)
  prettify_remove_decimal(x, units)
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

prettify_remove_decimal <- function(x, units) {
  if (!any(grepl("\\.", x))) return(x)
  x_decimals <- x
  for (unit in names(units)) {
    # strip unit name from result
    x_decimals <- sub(unit, "", x_decimals, fixed = TRUE)
  }
  x_decimals <- sub(".+(\\.\\d*)$", "\\1", x_decimals)
  x_decimals <- unique(x_decimals)

  # Nothing to do here
  if (!any(nzchar(x_decimals))) return(x)
  # Decimals are required to differentiate
  if (length(x_decimals) > 1) return(x)
  # Decimals are not just ".0"
  if (grepl("[1-9]", x_decimals)) return(x)

  sub(x_decimals, "", x, fixed = TRUE)
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

#' Truncate and wrap strings
#'
#' Truncates, trims, and wraps strings. Built for ggplot2 plots with long
#' string labels.
#'
#' @param x Strings
#' @param truncate_at Maximum total string length prior to wrapping. Default is
#'   80 characters. Use `NULL` to skip truncation.
#' @param truncate_with Character string that is added at the end of each string
#'   to indicate that the string was truncated. Eats into string length. Default
#'   is `"..."`; set to `NULL` or `""` to skip.
#' @param trim Should whitespace be trimmed? Default is `TRUE`.
#' @param wrap_at Wraps string at given length, passed to [stringr::str_wrap()].
#' @examples
#' text <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit"
#' pretty_string(text, truncate_at = 20, wrap_at = NULL)
#' pretty_string(text, truncate_at = NULL, wrap_at = 10)
#'
#' library(ggplot2)
#' set.seed(654321)
#' ex <- dplyr::data_frame(
#'   label = sample(stringr::sentences, 3),
#'   value = runif(3, 0, 10)
#' )
#'
#' g <- ggplot(ex) +
#'   aes(value, label) +
#'   geom_point()
#'
#' g
#'
#' g + scale_y_discrete(
#'   label = format_pretty_string(truncate_at = 25)
#' )
#'
#' g + scale_y_discrete(
#'   label = format_pretty_string(truncate_at = NULL, wrap_at = 20)
#' )
#' @export
pretty_string <- function(
  x,
  truncate_at = 80,
  truncate_with = "...",
  trim = TRUE,
  wrap_at = 40
) {
  x <- as.character(x)
  if (trim) x <- stringr::str_trim(x)
  truncate <- !is.null(truncate_at) && any(nchar(x[!is.na(x)]) > truncate_at)
  if (truncate) {
    truncate_actual <- if (!is.null(truncate_with)) {
      truncate_at - nchar(truncate_with)
    } else truncate_at
    x[nchar(x) > truncate_at] <- paste0(
      substr(x[nchar(x) > truncate_at], 1, truncate_actual),
      truncate_with
    )
  }
  if (!is.null(wrap_at) && wrap_at < max(nchar(x), na.rm = TRUE)) {
    x <- stringr::str_wrap(x, wrap_at)
  }
  x
}

#' @describeIn pretty_string Provides a pretty string formatter for ggplot2 labels
#' @export
format_pretty_string <- function(
  truncate_at = 80,
  truncate_with = "...",
  trim = TRUE,
  wrap_at = 40
) {
  function(x) pretty_string(x, truncate_at, truncate_with, trim, wrap_at)
}
