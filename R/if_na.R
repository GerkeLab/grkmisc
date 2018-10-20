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
#'     ts_diameter = if_na(ts_diameter, value = 30),
#'     hu_diameter = if_na(hu_diameter, value = rnorm(11, 100))
#'   )
#'
#' # Replacement can be predicated on other conditions in addition to missingness
#' storms %>%
#'   mutate(
#'     hu_diameter = if_na(hu_diameter, value = 30, status == "hurricane")
#'   )
#'
#' # Can provide a secondary value for missing values when the conditions
#' # are not met
#' storms %>%
#'   mutate(
#'     ts_diameter = if_na(ts_diameter, value = wind, status == "tropical storm", value_else = 0)
#'   )
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
if_na <- function(x, ..., value = default_value(x), value_else = NULL) {
  UseMethod("if_na")
}

#' @export
if_na.default <- function(x, ..., value = default_value(x), value_else = NULL) {
  value <- check_type(x, value)
  if (!is.null(value_else)) value_else <- check_type(x, value_else)

  conditions <- get_conditions(length(x), ...)

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

#' @export
if_na.factor <- function(x, ..., value = "(Missing)", value_else = NULL) {
  # if_na() for factors uses forcats::fct_explicit_na()
  # which does not work with the if_else() workflow
  value <- as.character(value)
  value_else <- if (!is.null(value_else)) as.character(value_else)

  conditions <- get_conditions(length(x), ...)

  if (!is.null(value_else)) {
    conditions_else <- is.na(x) & !conditions
    x <- forcats::fct_explicit_na(x, value)
    x[conditions_else] <- NA
    forcats::fct_explicit_na(x, value_else)
  } else {
    forcats::fct_explicit_na(x, value)
  }
}

get_conditions <- function(n, ...) {
  conditions <- list(...)
  conditions <- if (length(conditions)) {
    purrr::reduce(conditions, `&`)
  } else rep(TRUE, n)
  stopifnot(length(conditions) == n)
  conditions
}

default_value <- function(x) UseMethod("default_value")
default_value.list      <- function(x) list(NULL)
default_value.integer   <- function(x) 0L
default_value.numeric   <- function(x) 0.0
default_value.logical   <- function(x) FALSE
default_value.character <- function(x) ""
default_value.default   <- function(x) {
  msg <- paste("Please provide a `value` of type", typeof(x))
  rlang::abort(msg)
}

check_type <- function(x, value) {
  if (identical(typeof(x), typeof(value))) return(value)

  if (!canCoerce(value, class(x))) rlang::abort(
    paste("Unable to coerce `value` from", class(value)[1], "to", class(x)[1])
  )

  tryCatch({
    if (inherits(x, "numeric")) {
      # apparently as(1L, "numeric") doesn't work...
      # https://twitter.com/grrrck/status/1053330846856429568
      as.double(value)
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
