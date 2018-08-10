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
