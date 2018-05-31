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
  x_name = rlang::quo_name(rlang::enquo(x))
  y_name = rlang::quo_name(rlang::enquo(y))
  if (!is.null(.id)) {
    ids <- select(x, .id)
    x <- select(x, -.id)
    y <- select(y, -.id)
  } else ids <- NULL

  # De-factorize into characters
  x <- mutate_if(x, is.factor, as.character)
  y <- mutate_if(y, is.factor, as.character)

  xt <- purrr::map_dfr(x, ~ data_frame(value = list(.)), .id = "variable")
  yt <- purrr::map_dfr(y, ~ data_frame(value = list(.)), .id = "variable")
  z <- full_join(xt, yt, by = "variable") %>%
    mutate(
      misses     = purrr::map2(value.x, value.y, ~ not_equal(..1, ..2)),
      miss_count = purrr::map_int(misses, length),
      miss_count = ifelse(purrr::map_lgl(value.x, is.null), NA, miss_count),
      miss_count = ifelse(purrr::map_lgl(value.y, is.null), NA, miss_count),
      state      = ifelse(miss_count == 0, "same", "changed"),
      state      = case_when(
        miss_count > 0    ~ "changed",
        is.na(miss_count) ~ "unique",
        miss_count == 0   ~ "same",
        TRUE ~ "other"
      )
    ) %>%
    split(.$state)

  # TODO: catch no changes
  z_tidy_diff <- z$changed
  z_tidy_diff$value.x <- purrr::map2(z$changed$value.x, z$changed$misses, function(x, y) x[y])
  z_tidy_diff$value.y <- purrr::map2(z$changed$value.y, z$changed$misses, function(x, y) x[y])
  z_tidy_diff <- z_tidy_diff %>%
    select(-miss_count, -state) %>%
    split(.$variable) %>%
    purrr::map(~ {
      tidyr::unnest(.) %>%
        rename(miss_index = misses)
    })

  z_tidy_diff <- purrr::map(z_tidy_diff, function(z_td) {
    bind_cols(z_td, x[z_td$miss_index,  z$same$variable])
  })

  meta <- list()
  meta$names <- c(x = x_name, y = y_name)
  meta$dims <- purrr::map(list(x = x, y = y), ~ dim(.))
  meta$colnames <- purrr::map(list(x = x, y = y), ~ colnames(.))

  structure(list(
    tidy    = z_tidy_diff,
    changed = z$changed,
    same    = z$same,
    unique  = z$unique,
    ids     = ids,
    meta    = meta
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
  {
    x <- .
    if (inherits(x, "data.frame")) split(x, x$variable) else x
  } %>%
    purrr::map(
      function(x) {
        select(x, variable, contains("value"), miss_index) %>%
          tidyr::gather(set, value, contains("value")) %>%
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

not_equal <- function(x, y) {
  # Check class of x and y and if not the same then hope for the best
  if (!identical(class(x), class(y))) class(y) <- class(x)
  switch(
    class(x),
    "double" = which(abs(x - y) > .Machine$double.eps),
    which(x != y)
  )
}

#' @inheritParams glue glue glue_collapse
summary.tidy_diff <- function(z) {
  # 1. Compare dims
  # 2. Column names
  # 3. Column order
  # 4. Column type
  # 5. Number of differing columns
  # 6. Number of differing rows

  dims <- purrr::map_dfr(setNames(z$meta$dims, z$meta$names),
    ~ data_frame(rows = .[1], cols = .[2]), .id = "set")

  cli::cat_rule("Comparison Summary")
  print(dims)
  cli::cat_line()

  x_uniq <- setdiff(z$meta$colnames$x, z$meta$colnames$y)
  y_uniq <- setdiff(z$meta$colnames$y, z$meta$colnames$x)

  trunc_bullet <- function(x, ...) {
    if (x > getOption("width")) x <- paste0(strtrim(x, getOption("width") - 3), "...\033[0m")
    cli::cat_bullet(x, ...)
  }
  style_vars <- function(x) pillar::style_subtle(glue_collapse(glue("`{x}`"), sep = ", "))

  cat_thing_w_count <- function(thing, values, styler = crayon::bold) {
    if (!length(values)) return(invisible())
    plural <- if (length(values) > 1) "s" else ""
    trunc_bullet(glue(
      "'{thing}' has {styler(length(values))} unique column{plural}: ",
      style_vars(values)
    ))
  }
  cat_thing_w_count(z$meta$names["x"], x_uniq)
  cat_thing_w_count(z$meta$names["y"], y_uniq)
  if (!length(x_uniq) && !length(y_uniq)) trunc_bullet(
    glue(
      "\'{z$meta$names['x']}\' and \'{z$meta$names['y']}\' have the same columns: ",
      style_vars(z$meta$colnames$x)
    )
  )

  # TODO: Catch no changes
  trunc_bullet(glue(
    "There are {crayon::bold(sum(z$changed$miss_count))} differing values ",
    "across {crayon::bold(length(unique(unlist(z$changed$misses))))} rows"
  ))

}


test <- function(x, y) browser()
