context("test_shinyDataFilter_df")
skip_on_cran()

test_that("testing that extended getInitializationCode function returns code attribute", {
  x <- structure(1, code = "this_is_the_code", class = "shinyDataFilter_df")
  expect_equal(
    IDEAFilter::getInitializationCode.shinyDataFilter_df(x),
    "this_is_the_code")
})
