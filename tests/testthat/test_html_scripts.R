context("test assorted html scripts")
skip_on_cran()

app_path <- IDEAFilter:::shinytest_path("shinytest_html_scripts")
app <- shinytest2::AppDriver$new(app_path)

test_that("shinyDataFilter css gets added to header", {
  expect_true(grepl('style id="shinyDataFilter-animations"', app$get_html('html')))
  expect_true(grepl('style id="shinyDataFilter-css"', app$get_html('html')))
  expect_true(grepl('style id="shinyDataFilter-SortableJS-css"', app$get_html('html')))
  expect_true(grepl('script id="shinyDataFilter-SortableJS-js"', app$get_html('html')))
})

app$stop()
