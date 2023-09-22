context("test_shiny_data_filter")
skip_on_cran()

app_path <- IDEAFilter:::shinytest_path("shinytest_shiny_data_filter")
app <- shinytest2::AppDriver$new(app_path)

app$set_inputs(`data_filter-add_filter_select` = "Wind")
app$wait_for_js('document.getElementById("data_filter-filter_1-remove_filter_btn")')
app$set_inputs(`data_filter-filter_1-remove_filter_btn` = "click")

test_that("test that a new filter item has been added", {
  expect_true(!"data_filter-filter_1-column_select" %in% lapply(app$get_values(), names)$input)
})



app$set_inputs(`data_filter-add_filter_select` = "Ozone")
app$wait_for_js('document.getElementById("data_filter-filter_2-vector_filter-param_many")')
app$set_inputs(`data_filter-filter_2-vector_filter-param_many` = c(30, 90))

test_that("test that a new filter item has been added", {
  expect_equal(
    app$get_value(output = "data_summary"),
    renderPrint(subset(airquality, is.na(Ozone) | (Ozone >= 30 & Ozone <= 90)))())
})



app$set_inputs(`data_filter-filter_2-filter_na_btn` = "click")

test_that("test that a new filter item has been added", {
  expect_equal(
    app$get_value(output = "data_summary"),
    renderPrint(subset(airquality, Ozone >= 30 & Ozone <= 90))())
})




app$set_inputs(`data_filter-add_filter_select` = "Wind")
app$wait_for_js('document.getElementById("data_filter-filter_3-vector_filter-param_many")')
app$set_inputs(`data_filter-filter_3-vector_filter-param_many` = c(5, 10))

test_that("test that nrow reactive value is accurate", {
  expect_equal(
    app$get_value(output = "data_summary"),
    renderPrint(subset(airquality, 
                       (Ozone >= 30 & Ozone <= 90) &
                         (is.na(Wind) | (Wind >= 5 & Wind <= 10))
    ))())
})

app$stop()
