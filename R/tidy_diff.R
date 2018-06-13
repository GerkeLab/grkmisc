#' Compare Two Data.Frames
#'
#' Finds row-wise differences in two data frames that are assumed to be
#' pre-arranged.
#'
#' @param x `<tbl|df>` Reference data frame
#' @param y `<tbl|df>` Comparison data frame
#' @param ignore `<chr:NULL>` ID column(s) that are ignored from the differencing
#' @param group_vars `<chr:ignore>` Column names of grouping variables for alignment,
#'   takes default from `ignore`.
#' @param align `<lgl:FALSE>` Should alignmment be performed based on the
#'   grouping variables or the `group_vars` parameter?
#' @return Object of class `tidy_diff` that can be printed via `print(obj)` or
#'   plotted with [ggplot2] via `plot(obj)`.
#' @export
tidy_diff <- function(x, y, ignore = NULL, group_vars = ignore, align = FALSE) {
  # x <- arrange(x, .id)
  x_name = rlang::quo_name(rlang::enquo(x))
  y_name = rlang::quo_name(rlang::enquo(y))

  meta <- list()
  meta$names <- c(x = x_name, y = y_name)
  meta$dims <- purrr::map(list(x = x, y = y), ~ dim(.))
  meta$colnames <- purrr::map(list(x = x, y = y), ~ colnames(.))
  meta$coltypes <- purrr::map(list(x = x, y = y), ~ purrr::map_chr(., ~ class(.)[1]))

  # Check number of rows and address
  if (align || nrow(x) != nrow(y)) {
    xy <- align_data_frames(x, y, group_vars = group_vars)
    x <- xy$x
    y <- xy$y
  }

  # Drop ignored columns
  if (!is.null(ignore)) {
    x <- x[, intersect(colnames(x), ignore)]
    y <- y[, intersect(colnames(y), ignore)]
  }

  # De-factorize into characters
  x <- mutate_if(x, is.factor, as.character)
  y <- mutate_if(y, is.factor, as.character)

  xt <- purrr::map_dfr(x, ~ data_frame(value = list(.)), .id = "variable")
  yt <- purrr::map_dfr(y, ~ data_frame(value = list(.)), .id = "variable")
  z <- full_join(xt, yt, by = "variable") %>%
    mutate(
      misses     = purrr::map2(value.x, value.y, ~ not_equal(..1, ..2)),
      misses     = purrr::map2(misses, value.x, ~ union(..1, which(is.na(..2)))),
      misses     = purrr::map2(misses, value.y, ~ union(..1, which(is.na(..2)))),
      misses     = purrr::map(misses, sort),
      miss_count = purrr::map_int(misses, length),
      miss_count = ifelse(purrr::map_lgl(value.x, is.null), NA, miss_count),
      miss_count = ifelse(purrr::map_lgl(value.y, is.null), NA, miss_count),
      state      = ifelse(miss_count == 0, "same", "diff"),
      state      = ifelse(purrr::map_lgl(value.x, is.null), "unique_x", state),
      state      = ifelse(purrr::map_lgl(value.y, is.null), "unique_y", state),
      state      = ifelse(grepl("^_row\\.", variable), "same", state)             # Manually move row indices to "same" group
    ) %>%
    split(.$state)

  if (!is.null(z$diff)) {
    z_tidy_diff <- z$diff
    z_tidy_diff$value.x <- purrr::map2(z$diff$value.x, z$diff$misses, function(x, y) x[y])
    z_tidy_diff$value.y <- purrr::map2(z$diff$value.y, z$diff$misses, function(x, y) x[y])
    z_tidy_diff <- z_tidy_diff %>%
      select(-miss_count, -state) %>%
      split(.$variable) %>%
      purrr::map(~ {
        tidyr::unnest(.) %>%
          rename(miss_index = misses)
      })

    if (!is.null(z$same)) {
      # Add common columns to z_tidy_diff (i.e. ids, etc.)
      z_tidy_diff <- purrr::map(z_tidy_diff, function(z_td) {
        bind_cols(z_td, x[z_td$miss_index,  z$same$variable])
      })
    }

    if ("_row.x" %in% names(z_tidy_diff[[1]])) {
      z_tidy_diff <- purrr::map(
        z_tidy_diff,
        ~ rename(., miss_index.x = `_row.x`, miss_index.y = `_row.y`) %>%
          select(-`_row.z`) %>%
          select(variable, starts_with("value"), starts_with("miss"), everything())
      )
    }
  } else {
    z_tidy_diff <- NULL
  }

  z <- purrr::map_dfr(z, ~ {
    select(., variable, state, miss_count, misses)
  })

  structure(list(
    tidy   = z_tidy_diff,
    diff   = z,
    meta   = meta,
    ignore = ignore
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
  if (n == 0) {
    cli::cat_line(glue::glue("There were no differences found bewtween {paste0('`', zs$meta$names, '`', collapse = ' and ')}"))
    return(invisible())
  }
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
  x_row_ids <- 1:z$meta$dims$x[1]
  z$diff %>%
    select(variable, state, id = misses) %>%
    tidyr::unnest() %>%
    {
      ggplot2::ggplot(.) +
        ggplot2::aes(x = variable, y = -id, color = state) +
        ggplot2::geom_point(shape = 16) +
        ggplot2::scale_fill_manual(values = c("same" = "lightsteelblue1",
                                              "diff" = "firebrick3")) +
        ggplot2::scale_y_continuous(labels = function(x) abs(x), expand = c(0.04,0)) +
        ggplot2::scale_x_discrete(position = "top", expand = c(0.04,0)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 45, vjust = 0.5)) +
        ggplot2::guides(color = FALSE) +
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

#' @export
summary.tidy_diff <- function(z) {
  # 1. Compare dims
  # 2. Column names
  # 3. Column order
  # 4. Column type
  # 5. Number of differing columns
  # 6. Number of differing rows

  x_name <- z$meta$names["x"]
  y_name <- z$meta$names["y"]
  x_uniq <- setdiff(z$meta$colnames$x, z$meta$colnames$y)
  y_uniq <- setdiff(z$meta$colnames$y, z$meta$colnames$x)
  z_same <- purrr::reduce(z$meta$colnames, intersect)

  capture_tibble_print <- function(x, exclude = c(-1, -3)) {
    x <- capture.output(tibble:::print.tbl(x))[exclude]
    paste0("  ", x, collapse = "\n")
  }

  dims <- purrr::map_dfr(setNames(z$meta$dims, z$meta$names),
    ~ data_frame(rows = .[1], cols = .[2]), .id = "set")

  cli::cat_rule("Comparison Summary")
  dims <- capture_tibble_print(dims)
  cli::cat_bullet("Dimensions\n", dims)
  cli::cat_line()

  trunc_bullet <- function(x, ...) {
    if (nchar(x) > getOption("width")) x <- paste0(strtrim(x, getOption("width")), "...\033[0m")
    cli::cat_bullet(x, ...)
  }
  style_vars <- function(x) pillar::style_subtle(glue::glue_collapse(glue("`{x}`"), sep = ", "))

  cat_thing_w_count <- function(thing, values, styler = crayon::bold) {
    if (!length(values)) return(invisible())
    plural <- if (length(values) > 1) "s" else ""
    trunc_bullet(glue::glue(
      "'{thing}' has {styler(length(values))} unique column{plural}: ",
      style_vars(values)
    ))
  }
  cat_thing_w_count(x_name, x_uniq)
  cat_thing_w_count(y_name, y_uniq)
  if (!length(x_uniq) && !length(y_uniq)) trunc_bullet(
    glue::glue(
      "\'{x_name}\' and \'{y_name}\' have the same ",
      "{crayon::bold(length(z$meta$colnames[[1]]))} columns: ",
      style_vars(z$meta$colnames$x)
    )
  )

  coltype_diffs <- purrr::map(z$meta$coltypes,
                              ~ data_frame(column = names(.), type = .)) %>%
    purrr::reduce(left_join, by = "column") %>%
    filter(type.x != type.y)
  if (nrow(coltype_diffs)) {
    names(coltype_diffs)[2:3] <- c(x_name, y_name)
    coltype_diffs_out <- capture_tibble_print(coltype_diffs)
    plural <- if (nrow(coltype_diffs) > 1) "s" else ""
    cli::cat_bullet(glue::glue(
      "'{x_name}' and '{y_name}' have differing data types in",
      " {crayon::bold(nrow(coltype_diffs))} column{plural}:\n",
      "{coltype_diffs_out}\n\n"
    ))
  }

  if (is.null(z$tidy)) {
    cli::cat_bullet(glue::glue("No differences in the values between '{x_name}' and '{y_name}'"))
    return(invisible())
  }
  n_misses <- filter(z$diff, state == "diff") %>% pull(miss_count) %>% sum(na.rm = TRUE)
  n_miss_rows <- filter(z$diff, state == "diff") %>% pull(misses) %>% unlist() %>% unique() %>% length()
  trunc_bullet(glue::glue(
    "There are {crayon::bold(n_misses)} differing values ",
    "across {crayon::bold(n_miss_rows)} rows{if (n_misses) ':'}"
  ))
  if (n_misses) {
    z$diff %>%
      filter(!grepl("^_row\\.", variable)) %>%
      mutate(misses = purrr::map_chr(misses, paste, collapse = ", ")) %>%
      rename(`misses (row id)` = misses) %>%
      capture_tibble_print() %>%
      cli::cat_line()
  }

}

#' Align tibbles or data frames using grouping vars
#'
#' Uses `group_vars` to find missing rows in `x` or `y` and align final rows.
align_data_frames <- function(x, y, group_vars = NULL) {
  if (!"grouped_df" %in% union(class(x), class(y))) {
    if (is.null(group_vars)) {
      rlang::abort("`x` and `y` contain a different number of rows. Use `group_by()` to provide grouping variables to inform alignment.")
    }
  }
  if (!is.null(group_vars)) {
    # Use manually provided groups
    x <- group_by(x, !!!rlang::syms(group_vars))
    y <- group_by(y, !!!rlang::syms(group_vars))
  } else {
    # Use groups from grouped_df
    group_vars <- purrr::map(list(x = x, y = y), dplyr::group_vars)

    # Handle mismatched groups
    if (length(unique(purrr::map_int(group_vars, length))) != 1) {
      # If group vars are available in both, use union, else abort
      group_vars_all <- purrr::reduce(group_vars, union)
      if (length(setdiff(group_vars_all, union(names(x), names(y)))) == 0) {
        group_vars <- list(group_vars_all)
      } else {
        common <- purrr::reduce(group_vars, intersect)
        uniq   <- purrr::map(group_vars, setdiff, y = common) %>% purrr::compact()
        msg    <- glue::glue(
          "`x` and `y` have group column(s) ", glue::glue_collapse('"{common}"', sep = ", "),
          " in common but ", glue::glue_collapse(
            purrr::imap_chr(uniq, ~ glue::glue("`{.y}` has column(s) ", glue::glue_collapse('"{.x}"', sep = ", "))),
            sep = " and "
          )
        )
        rlang::abort(msg)
      }
    }
    group_vars <- group_vars[[1]]
  }

  # Check duplicates in grouping vars
  purrr::imap(list(x = x, y = y), ~ {
    distinct_rows <- nrow(distinct(.x, !!!rlang::syms(group_vars)))
    if (nrow(.x) != distinct_rows) {
      rlang::abort(glue::glue("`{.y}` only has {distinct_rows} distinct rows out of {nrow(.x)} rows"))
    }
  })

  # Add original row numbers
  xy <- purrr::map(list(x = x, y  = y), ~ ungroup(.))
  for (v in c("x", "y")) {
    xy[[v]][[paste0("_row.", v)]] <- 1:nrow(xy[[v]])
  }

  # Join by IDs and generate new row numbers
  row_ids <- xy %>%
    purrr::map(~ select(., !!!rlang::syms(group_vars), starts_with("_row"))) %>%
    purrr::reduce(full_join, by = group_vars) %>%
    mutate(`_row.z` = 1:nrow(.))

  # Merge old data frames with new row numbers, arrange by new row
  xy %>%
    purrr::map(
      ~ full_join(., row_ids, by = intersect(names(.), names(row_ids))) %>%
        arrange(`_row.z`)
    )
}
