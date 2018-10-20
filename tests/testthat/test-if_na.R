context("test-if_na")

test_that("if_na() works", {
  storms <- dplyr::filter(dplyr::storms, name == "Erika", year == 2003)

  expect_equal(if_na(storms$hu_diameter, then = 0), rep(0, nrow(storms)))

  storms_1 <- storms %>%
    dplyr::mutate(
      ts_diameter = if_na(ts_diameter, then = wind, status == "tropical storm"),
      ts_diameter = if_na(ts_diameter, then = 0)
    )

  storms_2 <- storms %>%
    dplyr::mutate(
      ts_diameter = ifelse(status != "tropical storm", 0, wind)
    )
  expect_equal(storms_1, storms_2)

  storms_3 <- storms %>%
    dplyr::mutate(ts_diameter =
      if_na(ts_diameter, then = wind, status == "tropical storm", otherwise = pressure)
    )
  storms_4 <- storms %>%
    dplyr::mutate(
      ts_diameter = dplyr::if_else(status == "tropical storm", wind, pressure),
      ts_diameter = as.double(ts_diameter)
    )

  expect_equal(storms_3, storms_4)

  expect_equal(if_na(c(1L, NA)), c(1L, 0L))
  expect_equal(if_na(c(1.0, NA)), c(1.0, 0))
  expect_equal(if_na(c("a", NA)), c("a", ""))
  expect_equal(if_na(c("1", NA_character_)), c("1", ""))
  expect_equal(if_na(c("1", NA_character_), then = 0), c("1", "0"))
  expect_equal(if_na(factor(1:3, 1:2), then = 4), factor(c(1L, 2L, 4L)))
  expect_equal(
    if_na(factor(1:4, 1:2), c(T, T, T, F), then = 10, otherwise = 11),
               factor(c(1L, 2L, 10L, 11L))
  )
  expect_equal(if_na(list(NULL, 1, list(2)), then = 3), list(3, 1, list(2)))
  expect_equal(suppressWarnings(if_na(c(1L, NA_integer_), then = "a")), c(1, NA_integer_))
  expect_warning(if_na(c(1L, NA_integer_), then = "a"))
  expect_equal(if_na(c(TRUE, NA)), c(TRUE, FALSE))
  expect_error(if_na(structure(1L, class = "unknown")), "Please provide")
  expect_error(if_na(structure(1L, class = "unknown"), then = 0), "Unable to coerce")
  expect_error(if_na(c(1, NA, 2), then = c(1, 2)))
  expect_error(if_na(c(1, NA, 2), c(T, F, T), otherwise = c(1, 2)))
})

test_that("if_na() works with data frames", {
  df <- dplyr::tibble(
    x = c(1, 2, NA), y = c("a", NA, "b"), z = list(1:5, NULL, 10:20)
  )

  x <- df %>% if_na()
  expect_equal(x$x, c(1, 2, 0))
  expect_equal(x$y, c("a", "", "b"))
  expect_equal(x$z, list(1:5, NULL, 10:20))

  x <- df %>% if_na(then = "0")
  expect_equal(x$x, c(1, 2, 0))
  expect_equal(x$y, c("a", "0", "b"))
  expect_equal(x$z, list(1:5, "0", 10:20))

  x <- df %>% if_na(vars = vars(x, y))
  expect_equal(x$x, c(1, 2, 0))
  expect_equal(x$y, c("a", "", "b"))
  expect_equal(x$z, list(1:5, NULL, 10:20))

  x <- df %>% if_na(vars = c("x", "y"))
  expect_equal(x$x, c(1, 2, 0))
  expect_equal(x$y, c("a", "", "b"))
  expect_equal(x$z, list(1:5, NULL, 10:20))

  x <- df %>% if_na(x = 3, y = "c")
  expect_equal(x$x, c(1, 2, 3))
  expect_equal(x$y, c("a", "c", "b"))
  expect_equal(x$z, list(1:5, NULL, 10:20))

  expect_warning(if_na(x, x = 3, then = 5))
  expect_error(if_na(x, vars = 1:2))
})
