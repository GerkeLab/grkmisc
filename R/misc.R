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
#' # Default is replacement with 0
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
#'     ts_diameter = if_na(ts_diameter, wind, status == "tropical_storm", value_else = 0)
#'   )
#'
#'
#' @param x Input vector of values
#' @param value Replacement value
#' @param value_else If given, then provides a secondary replacement value to
#'   be subsituted for missing values when the conditions in `...` are `FALSE`.
#' @param ... Additional expressions to condition replacement. NA values are
#'   only replaced when the additional expression matches.
#' @export
if_na <- function(x, value = 0L, ..., value_else = NULL) {
  conditions <- list(...)
  conditions <- if (length(conditions)) {
    purrr::reduce(conditions, `&`)
  } else rep(TRUE, length(x))

  stopifnot(length(conditions) == length(x))

  if (!is.null(value_else)) {
    ifelse(
      is.na(x),
      ifelse(conditions, value, value_else),
      x
    )
  } else {
    ifelse(is.na(x) & conditions, value, x)
  }
}
