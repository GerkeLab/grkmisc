context("test-labelize_values")

test_that("labelize_values generally works", {
  value_good <- "value dcf_unddeath_ccodef\n    1 = \"Abdomen\"\n    2 = \"Adrenal glands\"\n"
  expct_good <- c("Abdomen" = 1, "Adrenal glands" = 2)
  expect_equal(labelize_values(value_good, "factor"), expct_good)

  value_miss <- "value dcf_unddeath_ccodef\n    .F = \"No Icd9code\""
  expct_miss <- c("No Icd9code" = NA)
  expect_equal(labelize_values(value_miss, "factor", ".F"), expct_miss)

  value_none <- "value $buildf\n   "
  expct_none <- character(0)
  expect_equal(labelize_values(value_none, "character"), expct_none)
})

test_that("labelize_values works when = appears in label", {
  value_equal <- "value dcf_unddeath_ccodef\n    .F = \"No Icd9code, deathstat=4,5\"\n    1 = \"Abdomen\"\n    2 = \"Adrenal glands\"\n"
  expct_equal <- c("No Icd9code, deathstat=4,5" = NA, "Abdomen" = 1, "Adrenal glands" = 2)
  expect_equal(labelize_values(value_equal, "character", ".F"), expct_equal)
})

test_that("insert_quoted_equals works", {
  expect_equal(insert_quoted_equals(c("A", "\"a", "b\"")),
               c("A", "\"a=b\""))

  expect_equal(insert_quoted_equals(c("\"A", "a\"", "b")),
               c("\"A=a\"", "b"))

  expect_equal(insert_quoted_equals(c("A", "B")),
               c("A", "B"))
})


test_that("labelize_values works when value of '.' is given", {
  value_miss <- "value exc_death_dhqf\n    . = \"missing\"\n    0 = \"no\"\n    1 = \"yes\"\n  "
  expct_miss <- c("missing" = NA_real_, "no" = 0, "yes" = 1)
  expect_equal(labelize_values(value_miss, "factor", "."), expct_miss)
})

test_that("proc format statements covering multiple variables x0-5", {
  pf_statement <- "** FORMAT: biopplink0v **\n** FOR VARIABLE: biopplink0-5 **\nvalue biopplink0v\n    0 = \"(0) No\"\n    1 = \"(1) Yes\"\n  "
  pfdf <- extract_statement(pf_statement) %>% expand_varnames()

  expect_equal(pfdf$varname, paste0("biopplink", 0:5))
  expect_equal(unique(pfdf$format), "biopplink0v")
  expect_equal(nrow(pfdf), length(0:5))
})
