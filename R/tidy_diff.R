#' Compare Two Data.Frames
#'
#' Finds row-wise differences in two data frames that are assumed to be
#' pre-arranged.
#'
#' @param x <tbl|df> Reference data frame
#' @param y <tbl|df> Comparison data frame
#' @param .id <chr> ID column(s) that are ignored from the differencing
#' @return Object of class `tidy_diff` that can be printed via `print(obj)` or
#'   plotted with [ggplot2] via `plot(obj)`.
#' @export
tidy_diff <- function(x, y, .id = NULL) {
  # x <- arrange(x, .id)
  if (!is.null(.id)) {
    ids <- select(x, .id)
    x <- select(x, -.id)
    y <- select(y, -.id)
  } else ids <- NULL
  xt <- purrr::map_dfr(x, ~ data_frame(value = list(.)), .id = "variable")
  yt <- purrr::map_dfr(y, ~ data_frame(value = list(.)), .id = "variable")
  z <- left_join(xt, yt, by = "variable") %>%
    mutate(
      misses = purrr::map2(value.x, value.y, ~ which(..1 != ..2)),
      miss_count = purrr::map_int(misses, length),
      state = ifelse(miss_count == 0, "same", "changed"),
    ) %>%
    split(.$state)

  z_tidy_diff <- z$changed
  z_tidy_diff$value.x <- purrr::map2(z$changed$value.x, z$changed$misses, function(x, y) x[y])
  z_tidy_diff$value.y <- purrr::map2(z$changed$value.y, z$changed$misses, function(x, y) x[y])
  z_tidy_diff <- z_tidy_diff %>%
    select(-miss_count, -state) %>%
    tidyr::unnest() %>%
    rename(miss_index = misses)

  structure(list(
    tidy = z_tidy_diff,
    changed = z$changed,
    same = z$same,
    ids = ids
  ), class = "tidy_diff")
}

#' @export
print.tidy_diff <- function(z, n = 5) {
  if (!is.null(z$ids) && ncol(z$ids == 1)) {
    z$tidy <- mutate(z$tidy,
      miss_index = purrr::map(miss_index, ~ z$ids[[1]][.])
    )
  }
  z_tidy <- z$tidy %>%
    split(.$variable) %>%
    purrr::map(
      function(x) {
        tidyr::gather(x, set, value, contains("value")) %>%
          tidyr::spread(miss_index, value) %>%
          mutate(set = sub("value\\.", "", set))
      }
    )

  n <- min(length(z_tidy), n)
  cli::cat_line(pillar::style_subtle(glue::glue("Showing differences in first {n} columns...\n\n")))
  for (i in 1:n) {
    print(z_tidy[[i]])
    cat("\n")
  }
  if (length(z_tidy) > n) {
    extra_cols <- glue::glue_collapse(glue::glue("`{names(z_tidy)[-1:-n]}`"), sep = ", ")
    cli::cat_line(pillar::style_subtle(
      pillar::colonnade(glue::glue("\n... with differences in {length(z_tidy) - n} more columns: {extra_cols}"))
    ))
  }
}

#' @export
vis_changed <- function(x, y, ...) {
  z <- tidy_diff(x, y, ...)
  plot(z)
}

#' @export
plot.tidy_diff <- function(z) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  z$changed %>%
    mutate(misses = purrr::map2(misses, value.x, ~ 1:length(..2) %in% ..1)) %>%
    select(variable, misses) %>%
    mutate(id = purrr::map(misses, ~ 1:length(.))) %>%
    tidyr::unnest() %>%
    {
      ggplot2::ggplot() +
        ggplot2::aes(x = variable, y = -id, fill = misses) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_manual(values = c("lightsteelblue1", "firebrick3")) +
        ggplot2::scale_y_continuous(labels = function(x) abs(x), expand = c(0,0)) +
        ggplot2::scale_x_discrete(position = "top", expand = c(0,0)) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Column", y = "Row", fill = "Diff.")
    }
}
