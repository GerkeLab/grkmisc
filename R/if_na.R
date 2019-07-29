#' Replace Missing Values
#'
#' Replaces missing values with the given value, or optionally conditions the
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
#'     ts_diameter = if_na(ts_diameter, then = 30),
#'     hu_diameter = if_na(hu_diameter, then = rnorm(11, 100))
#'   )
#'
#' # Replacement can be predicated on other conditions in addition to missingness
#' storms %>%
#'   mutate(
#'     hu_diameter = if_na(hu_diameter, then = 30, status == "hurricane")
#'   )
#'
#' # Can provide a secondary value for missing values when the conditions
#' # are not met
#' storms %>%
#'   mutate(
#'     ts_diameter = if_na(ts_diameter, then = wind, status == "tropical storm", otherwise = 0)
#'   )
#'
#' @param .x Input vector of values
#' @param then Replacement value. Defaults to a value based on the input type
#'   and favors `0`, `FALSE`, `""` or empty lists according to the input type.
#' @param otherwise If given, provides a secondary replacement value to be
#'   subsituted for missing values when the conditions in `...` are `FALSE`.
#' @param ... Additional expressions to condition replacement. NA values are
#'   only replaced when the additional expression matches.
#' @export
if_na <- function(.x, ..., then = default_value(.x), otherwise = NULL) {
  UseMethod("if_na")
}

#' @export
if_na.default <- function(.x, ..., then = default_value(.x), otherwise = NULL) {
  then <- check_type(.x, then)
  if (!is.null(otherwise)) otherwise <- check_type(.x, otherwise)

  conditions <- get_conditions(length(.x), ...)

  missing_and_conditions <- which(!is_complete(.x) & conditions)
  .x[missing_and_conditions] <- if (length(then) > 1) {
    stopifnot(length(then) == length(.x))
    then[missing_and_conditions]
  } else then

  if (!is.null(otherwise)) {
    missing_not_conditions <- which(!is_complete(.x) & !conditions)
    .x[missing_not_conditions] <- if (length(otherwise) > 1) {
      stopifnot(length(otherwise) == length(.x))
      otherwise[missing_not_conditions]
    } else otherwise
  }

  .x
}

#' Replace Missing Values in Data Frames
#'
#' Replaces missing values in data frames, similar to [tidyr::replace_na()] with
#' a few extras. There are several ways to use this function. If called without
#' any arguments, all missing values will be replaced by the default type.
#' You can change the global default value with the `then` argument, and you
#' can subset the columns to be affected with the `vars` argument. Finally, you
#' can specify column-wise replacement values as named arguments, in which case
#' the other arguments are ignored.
#'
#' @examples
#' df <- dplyr::tibble(
#'   x = c(1, 2, NA), y = c("a", NA, "b"), z = list(1:5, NULL, 10:20)
#' )
#'
#' df %>% if_na()
#'
#' df %>% if_na(then = "0")
#'
#' df %>% if_na(vars = dplyr::vars(x, y))
#'
#' df %>% if_na(x = 3, y = "c")
#'
#' @inheritParams if_na
#' @param ... Optional named arguments specifying column-wise replacement values
#'   for missing values. Other arguments are ignored by `if_na()` when used.
#' @param then Global replacement value to be used in place of the default.
#' @param otherwise Ignored.
#' @param vars Optional vector of column names that should receive the missing
#'   value replacement. Use the `vars()` helper to provide bare column names.
#' @export
if_na.data.frame <- function(.x, ..., then = NULL, otherwise = NULL, vars = NULL) {
  # otherwise is ignored
  # then is ignored if ... are provided
  vals <- list(...)
  if (length(vals)) {
    replace_vars <- intersect(names(vals), names(.x))
    if (!is.null(then))
      rlang::warn("`then` is ignored when values are provided in `...`")
  } else {
    replace_vars <- names(.x)
    if (!is.null(vars)) {
      vars <- get_vars(vars)
      replace_vars <- intersect(vars, replace_vars)
    }
    vals <- if (is.null(then)) {
      purrr::set_names(replace_vars) %>%
        purrr::map(function(v) default_value(.x[[v]]))
    } else {
      purrr::set_names(replace_vars) %>%
        purrr::map(~ then)
    }
  }
  for (var in replace_vars) {
    .x[[var]] <- if_na(.x[[var]], then = vals[[var]])
  }

  .x
}

#' @export
if_na.factor <- function(.x, ..., then = "(Missing)", otherwise = NULL) {
  # if_na() for factors uses forcats::fct_explicit_na()
  # which does not work with the if_else() workflow
  then <- as.character(then)
  otherwise <- if (!is.null(otherwise)) as.character(otherwise)

  conditions <- get_conditions(length(.x), ...)

  if (!is.null(otherwise)) {
    conditions_else <- is.na(.x) & !conditions
    .x <- forcats::fct_explicit_na(.x, then)
    .x[conditions_else] <- NA
    forcats::fct_explicit_na(.x, otherwise)
  } else {
    forcats::fct_explicit_na(.x, then)
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

is_complete <- function(x) {
  # tidyr:::is_complete
  if (typeof(x) == "list") {
    !purrr::map_lgl(x, purrr::is_empty)
  } else {
    !is.na(x)
  }
}

vars <- function(...) {
  rlang::quos(...)
}

get_vars <- function(x) {
  if (inherits(x, "quosures")) {
    x <- purrr::map_chr(x, rlang::quo_text)
    return(x)
  } else if (inherits(x, "character")) {
    return(x)
  } else {
    rlang::abort("Please use provide variable names in vars or use the vars() helper.")
  }
}

default_value <- function(x) UseMethod("default_value")
default_value.list      <- function(x) list(NULL)
default_value.integer   <- function(x) 0L
default_value.numeric   <- function(x) 0.0
default_value.logical   <- function(x) FALSE
default_value.character <- function(x) ""
default_value.default   <- function(x) {
  msg <- paste("Please provide a value of type", typeof(x))
  rlang::abort(msg)
}

#' @importFrom methods as
check_type <- function(x, value) {
  if (identical(typeof(x), typeof(value))) return(value)

  if (!methods::canCoerce(value, class(x))) rlang::abort(
    paste("Unable to coerce value from", class(value)[1], "to", class(x)[1])
  )

  tryCatch({
    if (inherits(x, "numeric")) {
      # apparently as(1L, "numeric") doesn't work...
      # https://twitter.com/grrrck/status/1053330846856429568
      as.double(value)
    } else if (inherits(x, "list")) {
      list(value)
    } else {
      as(value, class(x))
    }
  },
  warning = function(w) {
    rlang::warn(
      paste("Missing values may have been introduced when coercing value from",
            class(value)[1], "to", class(x)[1])
    )
    suppressWarnings(as(value, class(x)))
  })
}
