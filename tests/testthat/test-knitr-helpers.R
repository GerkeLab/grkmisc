context("test-knitr-helpers")

test_that("knitr_bold_row", {
  x <- dplyr::starwars[1:4, 1:5]
  expect_equal(knitr_bold_row(x, height < 100)$name[3],
               "**R2-D2**")

  luke_darth <- knitr_bold_row(x, height > 170, cols = "hair_color")
  expect_equal(luke_darth$name[1], "Luke Skywalker") # didn't bold Luke's name
  expect_equal(luke_darth$hair_color, c("**blond**", NA, NA, "**none**"))

  robots <- knitr_bold_row(x, mass <= 75, height <= 170, cols = c("name", "height"), format = "latex")
  expect_equal(robots$name[2], "\textbf{C-3PO}")
  expect_equal(robots$mass[2], 75)
  robots <- knitr_bold_row(x, mass <= 75, height <= 170, cols = c("name", "height"), format = "html")
  expect_equal(robots$name[3], "<strong>R2-D2</strong>")
  expect_equal(robots$mass[3], 32)
})
