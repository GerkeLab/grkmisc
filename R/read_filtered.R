#' Read TSV and Filter on the Fly
#'
#' @param file <chr> Filename to read in
#' @param filter_expression <bare> The expression passed to `dplyr::filter()`
#' @inheritDotParams readr::read_tsv
#' @export
read_tsv_filtered <- function(file, filter_expression, ...) {
  stopifnot(requireNamespace("dplyr", quietly = TRUE),
            requireNamespace("readr", quietly = TRUE))

  filter_expression <- rlang::enexpr(filter_expression)

  filter_by_id <- function(.data, pos) {
    dplyr::filter(.data, !!filter_expression)
  }

  readr::read_tsv_chunked(
    file,
    readr::DataFrameCallback$new(filter_by_id),
    ...
  )
}

#' Read CSV and Filter on the Fly
#'
#' @param file <chr> Filename to read in
#' @param filter_expression <bare> The expression passed to `dplyr::filter()`
#' @inheritDotParams readr::read_csv
#' @export
read_csv_filtered <- function(file, filter_expression, ...) {
  stopifnot(requireNamespace("dplyr", quietly = TRUE),
            requireNamespace("readr", quietly = TRUE))

  filter_expression <- rlang::enexpr(filter_expression)

  filter_by_id <- function(.data, pos) {
    dplyr::filter(.data, !!filter_expression)
  }

  readr::read_csv_chunked(
    file,
    readr::DataFrameCallback$new(filter_by_id),
    ...
  )
}

