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
