context("test_preselection")
skip_on_cran()

app_path <- IDEAFilter:::shinytest_path("shinytest_preselection")
app <- shinytest2::AppDriver$new(app_path)

app$wait_for_idle()

test_that("test that a new filter item has been added", {
  expect_true(!"data_filter-filter_2-column_select" %in% lapply(app$get_values(), names)$input)
})

test_that("test that a new filter item has been added", {
  expect_equal(
    app$get_value(output = "data_summary"),
    renderPrint(subset(airquality, Ozone >= 30 & Month == 9))())
})

app$set_inputs(filter_select = "filter_2")
app$wait_for_idle()

test_that("test that nrow reactive value is accurate", {
  expect_equal(
    app$get_value(output = "data_summary"),
    renderPrint(subset(airquality,
                       (is.na(Ozone) | (Ozone >= 30 & Ozone <= 90)) &
                         (is.na(Wind) | (Wind >= 5 & Wind <= 10))
    ))())
})

app$stop()
