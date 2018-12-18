context("test-pretty_num")

test_that("pretty_num() works", {
  expect_equal(pretty_num(1234), "1.2k")
  expect_equal(pretty_num(12345, decimal_digits = 3), "12.345k")
  expect_equal(pretty_num(123456789), "123.5M")
  expect_equal(pretty_num(12345678900), "12.3B")
  expect_equal(pretty_num(c(1234, 1234567, 12345678900)), c("1.2k", "1.2M", "12.3B"))
  expect_equal(
    pretty_num(c(0.00123, 0.0123, 0.123, 1.23),
      units = c(mm = 0.001, cm = 0.01, m = 1)
    ),
    c("1.2mm", "1.2cm", "12.3cm", "1.2m")
    )
  expect_equal(pretty_num(123456, units = c(M = 1e6)), "123456")
})

test_that("pretty_num() removes decimals when not needed", {
  expect_equal(pretty_num(1:4 * 1000), paste0(1:4, "k"))
  expect_equal(pretty_num(c(1, 1.5, 2) * 1000), c("1.0k", "1.5k", "2.0k"))
})
