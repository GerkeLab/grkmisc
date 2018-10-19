context("test-if_na")

test_that("if_na() works", {
  storms <- dplyr::filter(dplyr::storms, name == "Erika", year == 2003)

  expect_equal(if_na(storms$hu_diameter, 0), rep(0, nrow(storms)))

  storms_1 <- storms %>%
    dplyr::mutate(
      ts_diameter = if_na(ts_diameter, wind, status == "tropical storm"),
      ts_diameter = if_na(ts_diameter, 0)
    )

  storms_2 <- storms %>%
    dplyr::mutate(
      ts_diameter = ifelse(status != "tropical storm", 0, wind)
    )
  expect_equal(storms_1, storms_2)

  storms_3 <- storms %>%
    dplyr::mutate(ts_diameter =
      if_na(ts_diameter, wind, status == "tropical storm", value_else = 0)
    )
  storms_4 <- storms %>%
    dplyr::mutate(ts_diameter = ifelse(status == "tropical storm", wind, 0))

  expect_equal(storms_3, storms_4)

  expect_equal(if_na(c("1", NA_character_)), c("1", ""))
  expect_equal(if_na(c("1", NA_character_), 0), c("1", "0"))
  expect_equal(suppressWarnings(if_na(c(1L, NA_integer_), "a")), c(1, NA_integer_))
  expect_warning(if_na(c(1L, NA_integer_), "a"))
  expect_warning(if_na(factor(1:3, 1:2)))
  expect_warning(if_na(factor(1:3, 1:2), 1L))
})
