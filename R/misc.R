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

#' Replace NA values
#'
#' Replaces NA values with the given value, or optionally conditions the
#' replacement on the expressions given in `...`, which are applied to the
#' inputs in the same way as `filter`.
#'
#' @examples
#' library(dplyr)
#' storms <- storms %>%
#'   filter(name == "Erika", year == 2003) %>%
#'   select(name, year, status, wind, contains("diameter"))
#'
#' # Default is replacement with appropriate default (0, FALSE, "", etc.)
#' storms %>%
#'   mutate(hu_diameter = if_na(hu_diameter))
#'
#' # Replacement can be single value or vector of values
#' storms %>%
#'   mutate(
#'     ts_diameter = if_na(ts_diameter, 30),
#'     hu_diameter = if_na(hu_diameter, rnorm(11, 100))
#'   )
#'
#' # Replacement can be predicated on other conditions in addition to missingness
#' storms %>%
#'   mutate(
#'     hu_diameter = if_na(hu_diameter, 30, status == "hurricane")
#'   )
#'
#' # Can provide a secondary value for missing values when the conditions
#' # are not met
#' storms %>%
#'   mutate(
#'     ts_diameter = if_na(ts_diameter, wind, status == "tropical storm", value_else = 0)
#'   )
#'
#'
#' @param x Input vector of values
#' @param value Replacement value, the default value is based on the input
#'   value type and favors `0`, `FALSE`, or empty strings, lists or data frames
#'   according to the input type.
#' @param value_else If given, then provides a secondary replacement value to
#'   be subsituted for missing values when the conditions in `...` are `FALSE`.
#' @param ... Additional expressions to condition replacement. NA values are
#'   only replaced when the additional expression matches.
#' @export
if_na <- function(x, value = default_value(x), ..., value_else = NULL) {
  value <- check_type(x, value)

  conditions <- list(...)
  conditions <- if (length(conditions)) {
    purrr::reduce(conditions, `&`)
  } else rep(TRUE, length(x))

  stopifnot(length(conditions) == length(x))

  if (!is.null(value_else)) {
    dplyr::if_else(
      !is.na(x),
      x,
      dplyr::if_else(conditions, value, value_else),
    )
  } else {
    dplyr::if_else(!(is.na(x) & conditions), x, value)
  }
}

default_value <- function(x) UseMethod("default_value")
default_value.integer    <- function(x) 0L
default_value.numeric    <- function(x) 0.0
default_value.logical    <- function(x) FALSE
default_value.character  <- function(x) ""
default_value.matrix     <- function(x) x[0,]
default_value.list       <- function(x) list(NULL)
default_value.data.frame <- function(x) x[0,]
default_value.tibble     <- function(x) x[0,]
default_value.default <- function(x) {
  msg <- paste("Please provide a `value` of type", typeof(x))
  rlang::abort(msg)
}
default_value.factor <- function(x) {
  warn_factor(x[1])
  x[1]
}

warn_factor <- function(value = NULL) {
  rlang::warn(paste(
    "if_na() may not handle factors very well.",
    "It's probably safer to coerce to integer or character first.",
    if (!is.null(value)) paste("Used", x[1], "as the default value.")
  ))
}

check_type <- function(x, value) {
  if (identical(typeof(x), typeof(value))) return(value)
  if (is.factor(x)) warn_factor()

  if (!canCoerce(value, class(x))) rlang::abort(
    paste("Unable to coerce `value` from", class(value)[1], "to", class(x)[1])
  )

  tryCatch({
    if (is.integer(value) & is.numeric(x)) {
      # apparently as(1L, "numeric") doesn't work...
      # https://twitter.com/grrrck/status/1053330846856429568
      as.numeric(value)
    } else {
      as(value, class(x))
    }
  },
  warning = function(w) {
    rlang::warn(
      paste("Missing values may have been introduced when coercing `value` from",
            class(value)[1], "to", class(x)[1])
    )
    suppressWarnings(as(value, class(x)))
  })
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
