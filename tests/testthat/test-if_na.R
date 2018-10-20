context("test-if_na")

test_that("if_na() works", {
  storms <- dplyr::filter(dplyr::storms, name == "Erika", year == 2003)

  expect_equal(if_na(storms$hu_diameter, value = 0), rep(0, nrow(storms)))

  storms_1 <- storms %>%
    dplyr::mutate(
      ts_diameter = if_na(ts_diameter, value = wind, status == "tropical storm"),
      ts_diameter = if_na(ts_diameter, value = 0)
    )

  storms_2 <- storms %>%
    dplyr::mutate(
      ts_diameter = ifelse(status != "tropical storm", 0, wind)
    )
  expect_equal(storms_1, storms_2)

  storms_3 <- storms %>%
    dplyr::mutate(ts_diameter =
      if_na(ts_diameter, value = wind, status == "tropical storm", value_else = 0)
    )
  storms_4 <- storms %>%
    dplyr::mutate(ts_diameter = ifelse(status == "tropical storm", wind, 0))

  expect_equal(storms_3, storms_4)

  expect_equal(if_na(c(1L, NA)), c(1L, 0L))
  expect_equal(if_na(c(1.0, NA)), c(1.0, 0))
  expect_equal(if_na(c("a", NA)), c("a", ""))
  expect_equal(if_na(c("1", NA_character_)), c("1", ""))
  expect_equal(if_na(c("1", NA_character_), value = 0), c("1", "0"))
  expect_equal(if_na(factor(1:3, 1:2), value = 4), factor(c(1L, 2L, 4L)))
  expect_equal(
    if_na(factor(1:4, 1:2), c(T, T, T, F), value = 10, value_else = 11),
               factor(c(1L, 2L, 10L, 11L))
  )
  expect_equal(if_na(list(NA, 1, list(2))), list(NULL, 1, list(2)))
  expect_equal(suppressWarnings(if_na(c(1L, NA_integer_), value = "a")), c(1, NA_integer_))
  expect_warning(if_na(c(1L, NA_integer_), value = "a"))
  expect_equal(if_na(c(TRUE, NA)), c(TRUE, FALSE))
  expect_error(if_na(structure(1L, class = "unknown")), "Please provide")
  expect_error(if_na(structure(1L, class = "unknown"), value = 0), "Unable to coerce")
})
