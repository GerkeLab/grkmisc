#' Lazy load results from a knitr cache
#'
#' @param chunk Name or names of chunk to load
#' @param cache_path Path to cache directory
#' @family knitr helpers
#' @export
knitr_load_cache <- function(chunk = NULL, cache_path = NULL) {
  if (!is.null(chunk)) {
    # make sure chunks exist and get full chunk name
    chunk_names <- knitr_list_cache(cache_path, names_only = FALSE)
    chunk_good <- purrr::map_lgl(chunk, ~ any(grepl(., chunk_names)))
    if (any(!chunk_good)) {
      plural <- if (sum(!chunk_good) > 1) "Chunks" else "Chunk"
      rlang::warn(glue::glue("{plural} {paste0(\"'\", chunk[!chunk_good], \"'\", collapse = ',')} not found in cache."))
    }

    chunk <- purrr::map_chr(chunk[chunk_good], ~ grep(., chunk_names, value = TRUE))
  } else {
    chunk <- knitr_list_cache(cache_path, names_only = FALSE)
  }

  if (!length(chunk)) {
    message("No chunks to import.")
    return(invisible(FALSE))
  }

  purrr::walk(chunk, ~ try(lazyLoad(., envir = .GlobalEnv)))
  invisible(TRUE)
}


#' List cached knitr chunks
#'
#' @param cache_path Path to cache directory
#' @param names_only Print only names of cached knitr chunks? Otherwise returns
#'   full absolute path names for loading the chunks.
#' @family knitr helpers
#' @export
knitr_list_cache <- function(cache_path = NULL, names_only = TRUE) {
  cache_path <- find_cache_path(cache_path)

  chunks <- fs::file_info(fs::dir_ls(cache_path, regexp = "\\.rdb$")) %>%
    dplyr::arrange(birth_time) %>%
    dplyr::pull(path)

  if (!names_only) return(sub("\\.rdb$", "", chunks))

  chunks %>%
    fs::path_rel(start = cache_path) %>%
    sub("_[a-f0-9]+\\.rdb$", "", .)
}

find_cache_path <- function(cache_path = NULL) {
  if (is.null(cache_path)) {
    cache_path <- fs::path_real(fs::dir_ls(type = "dir", regexp = "_cache$")[1])
  }
  if (is.na(cache_path)) {
    rlang::abort("No knitr cache found in working directory.")
  }
  if (!fs::file_exists(cache_path)) {
    rlang::abort(glue::glue("{cache_path} does not exist."))
  }

  # inside cache_path is an output specific path, pick the most recent
  cache_path <- fs::file_info(fs::dir_ls(cache_path)) %>%
    dplyr::arrange(change_time) %>%
    dplyr::slice(1) %>%
    dplyr::pull(path)
}

#' Embolden rows matching an expression
#'
#' Emboldens rows matching an expression, with optionally selected columns. For
#' use with [knitr::kable()].
#'
#' @examples
#' x <- dplyr::starwars[1:4, 1:5]
#' knitr_bold_row(x, height < 100)
#'
#' knitr_bold_row(x, mass <= 75, height <= 170, cols = c("name", "height"), format = "html")
#'
#' x %>%
#'   knitr_bold_row(height > 170, cols = "hair_color") %>%
#'   knitr::kable()
#'
#' @param x A data frame
#' @param ... The filter expressions
#' @param cols Columns that should be emphasized, default `NULL` or all columns
#' @param format One of `"markdown"`, `"html"`, or `"latex"`
#' @export
knitr_bold_row <- function(x, ..., cols = NULL, format = c("markdown", "html", "latex")) {
  f_expr <- rlang::enexprs(...)
  wrap <- switch(
    match.arg(format),
    "markdown" = c("**", "**"),
    "html" = c("<strong>", "</strong>"),
    "latex" = c("\textbf{", "}")
  )
  cols <- if (is.null(cols)) colnames(x) else intersect(colnames(x), cols)
  i <- x %>%
    dplyr::ungroup() %>%
    dplyr::mutate(i = dplyr::row_number()) %>%
    dplyr::filter(!!!f_expr) %>%
    dplyr::pull(i)
  x[i, cols] <- apply(x[i, cols], 2, function(x) ifelse(!is.na(x) & x != "", paste0(wrap[1], x, wrap[2]), x))
  x
}
