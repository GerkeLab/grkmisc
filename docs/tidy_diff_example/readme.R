#' ---
#' author: Garrick Aden-Buie
#' date: '`r Sys.Date()`'
#' output: github_document
#' ---

#+ echo=FALSE, message=FALSE
knitr::opts_chunk$set(cache = TRUE, comment = "#")
library(dplyr)

#' - [Functions to Create Data](#functions-to-create-fake-data)
#' - [Example Data](#example-data)
#' - [tidy_diff()](#tidy-diff)
#' - [Data Frames with Mismatched Rows](#data-frames-with-mismatched-rows)
#'
#' ## Functions to Create Data

#' First, we need some fake data. The following functions make fake data, with
#' a core data structure of columns named `colname_NN` and ID columns `id_NN`.
#' The data are randomly generated as integers, doubles, characters and factors,
#' and the IDs are drawn from integer or character labels.
#'
#' A single big tibble is created and then copied to a new tibble where it is
#' "corrupted" in a few rows and columns.
make_char   <- function(iter, len) sapply(1:iter, function(x) paste0(sample(letters, len, replace = TRUE), collapse = ""))
make_factor <- function(iter, len) sapply(1:iter, function(x) factor(sample(letters[1:len], 1), levels = letters[1:len]))

make_core_fake <- function(n, rows = 100) {
  x <- list()
  for (i in 1:n) {
    type <- sample(c("int", "dbl", "char", "factor"), 1)
    x[[sprintf("colname_%02d", i)]] <- switch(
      type,
      int    = sample(-50:50, rows, replace = TRUE),
      dbl    = runif(rows, 0, 1) * 10^(sample(1:5, 1)),
      char   = make_char(rows, 6),
      factor = make_factor(rows, 10)
    )
  }
  tibble::as_tibble(x)
}

factorize <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))[-1]
  all_fct <- div[x %% div == 0L]
  factors <- c()
  flag <- TRUE
  while(flag) {
    this <- all_fct[x %% all_fct == 0L]
    if (!length(this)) {
      flag <- FALSE
    } else {
      factors <- c(factors, this[1])
      x <- x / this[1]
    }
  }
  return(factors)
}

add_ids <- function(df, n_ids = NULL) {
  x <- list()
  rows <- nrow(df)
  n <- if (is.null(n_ids)) factorize(rows) else n_ids
  types <- sample(c("int", "char"), length(n), replace = TRUE)
  for (i in 1:length(n)) {
    x[[sprintf("id_%02d", i)]] <- switch(
      types[i],
      int = sample(1:rows, n[i]),
      char = make_char(n[i], 6)
    )
  }
  df_id <- if (is.null(n_ids)) {
    expand.grid(x)
  } else {
    x <- lapply(x, function(k) rep(k, ceiling(rows/length(k)))[1:rows])
  }
  x <- dplyr::bind_cols(df_id, df)
  tibble::as_tibble(x)
}

corrupt_values <- function(df, ..., n_rows = nrow(df)/5) {
  vars <- rlang::enexprs(...)
  vars <- tidyselect::vars_select(names(df), !!!vars)
  for (var in vars) {
    df[[var]][sample(1:nrow(df), n_rows)] <- sample(df[[var]], n_rows)
  }
  df
}

#' ## Example Data

#' Here is the fake data with 10 data columns, 2 ID columns and 10^5 rows.
x <- make_core_fake(10, 10^5) %>%
  add_ids(n_ids = c(50^4, 300))
y <- corrupt_values(x, dplyr::contains("colname"), n_rows = 20)

#' Here I additionally remove some columns from each side and change data types
x <- x[, sample(-3:-13, 1)]
y <- y[, sample(-3:-13, 2)]
y_ints <- sapply(y, is.integer)
y_ints[1:2] <- rep(FALSE, 2)
if (any(y_ints)) {
  y[[which(y_ints)[1]]] <- as.character(y[[which(y_ints)[1]]])
}

tibble:::print.tbl_df(x)
tibble:::print.tbl_df(y)

#' ## Tidy Diff

#' We can compare the two data frames with `tidy_diff()`.
#+ message=FALSE
library(grkmisc)
system.time(
  z <- tidy_diff(x, y)
)

#' This creates a `tidy_diff` object with `print`, `summary` and `plot` methods
#+ prompt=TRUE, comment=""
print(z)

#+ prompt=TRUE, comment=""
summary(z)

#+ prompt=TRUE, comment=""
plot(z)

#' You can also pull out a list of comparisons by differing columns by
#' subsetting the to the `.$tidy` element of the tidy diff object.
#+ prompt=TRUE, comment=""
z$tidy[1]

#' The tidy diff object also includes a tidy dataframe with "diff", "same" or
#' "unique" column values.
#+ prompt=TRUE, comment=""
z$diff

#' In terms of size
#+ prompt=TRUE, comment=""
pryr::object_size(x)
pryr::object_size(y)
pryr::object_size(z)


#' ## Data Frames with Mismatched Rows
#'
#' The following two lines scramble the order of rows in `x` and `y` while
#' taking a subsample of the original rows. This means that the two data frames
#' will be out of order and that there are rows appearing only in `x` or `y`.
#' Grouping variables are added to indicate the data frame keys that will be
#' used to align the two input data frames (or
#' `group_vars = c("id_01", "id_02")` could be set inside `tidy_diff()`).
x2 <- group_by(x, id_01, id_02) %>% {.[sample(1:nrow(.), floor(nrow(x) * 0.9952)), ]}
y2 <- group_by(y, id_01, id_02) %>% {.[sample(1:nrow(.), floor(nrow(y) * 0.9921)), ]}

#+ prompt=TRUE, comment=""
z2 <- tidy_diff(x2, y2)
summary(z2)

z2$diff

z2$tidy[1]
