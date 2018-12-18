context("test-pretty_string")

test_that("pretty_string", {
  text <- c(
    "A dash of pepper spoils beef stew.",
    "Fill your pack with bright trinkets for the poor."
  )

  expect_equal(
    nchar(pretty_string(text, truncate_at = 10)), c(10, 10)
  )

  expect_equal(pretty_string(text, NULL, wrap_at = NULL), text)
  expect_equal(
    pretty_string(text[1], truncate_at = 16, wrap_at = 5),
    "A\ndash\nof\npep...")
  expect_equal(
    pretty_string(text[1], truncate_at = 16, truncate_with = "", wrap_at = 5),
    "A\ndash\nof\npepper")
  expect_equal(
    pretty_string(text[1], truncate_at = NULL, wrap_at = 5),
    gsub(" ", "\n", text[1]))
  expect_equal(
    pretty_string(text[1], truncate_at = NULL, wrap_at = 80),
    text[1]
  )
  expect_equal(
    pretty_string(paste0("    ", text), truncate_at = NULL, wrap_at = NULL),
    text
  )
  expect_equal(
    format_pretty_string()(text),
    pretty_string(text)
  )
})
