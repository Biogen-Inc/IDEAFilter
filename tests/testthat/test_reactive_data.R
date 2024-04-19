context("test_reactive_data")
skip_on_cran()

# reflects data used in shinytest
mtcars2 <- mtcars
mtcars2[which((mtcars2 * 0.987) %% 0.2 < 0.01, arr.ind = TRUE)] <- NA

app_path <- IDEAFilter:::shinytest_path("shinytest_reactive_data")
app <- shinytest2::AppDriver$new(app_path)

app$set_inputs(`data_filter-add_filter_select` = "Ozone")
app$wait_for_js('document.getElementById("data_filter-filter_1-remove_filter_btn")')
app$wait_for_idle()
app$set_inputs(`data_filter-filter_1-vector_filter-param_many` = c(30, 90))

test_that("test that a new filter item has been added", {
  expect_equivalent(
    app$get_value(output = "data_summary"),
    renderPrint(subset(airquality, is.na(Ozone) | (Ozone >= 30 & Ozone <= 90)))())
})

app$set_inputs(select_data = "mtcars")

test_that("test that a new filter item has been added", {
  expect_equivalent(
    app$get_value(output = "data_summary"),
    renderPrint(mtcars2)())
})

app$set_inputs(`data_filter-add_filter_select` = "mpg")
app$wait_for_js('document.getElementById("data_filter-filter_2-remove_filter_btn")')
app$wait_for_idle()
app$set_inputs(`data_filter-filter_2-vector_filter-param_many` = c(20, 25))

test_that("test that a new filter item has been added", {
  expect_equivalent(
    app$get_value(output = "data_summary"),
    renderPrint(subset(mtcars2, is.na(mpg) | (mpg >= 20 & mpg <= 25)))())
})

app$set_inputs(select_data = "airquality")

test_that("test that a new filter item has been added", {
  expect_equivalent(
    app$get_value(output = "data_summary"),
    renderPrint(subset(airquality, is.na(Ozone) | (Ozone >= 30 & Ozone <= 90)))())
})
