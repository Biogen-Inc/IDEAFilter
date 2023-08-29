context("test_shiny_data_filter_item")
skip_on_cran()

# reflects data used in shinytest
data <- mtcars
data[which((data * 0.987) %% 0.2 < 0.01, arr.ind = TRUE)] <- NA

app_path <- IDEAFilter:::shinytest_path("shinytest_shiny_data_filter_item")
app <- shinytest2::AppDriver$new(app_path)


test_that("test that filter item initializes with column select", {
  expect_true(!"filter-vector_filter-param" %in% lapply(app$get_values(), names))
})



app$set_inputs(`filter-column_select` = "mpg")
app$wait_for_js('document.getElementById("filter-vector_filter-param_many")')
app$set_inputs(`filter-vector_filter-param_many` = c(20, 25))



test_that("test that nrow reactive value is accurate", {
  expect_equal(
    app$get_value(output = "filter-nrow"),
    as.character(nrow(subset(data, is.na(mpg) | (mpg >= 20 & mpg <= 25)))))
})



app$set_inputs(`filter-filter_na_btn` = "click")

test_that("test that filtering NAs works", {
  expect_equal(
    app$get_value(output = "filter-nrow"),
    as.character(nrow(subset(data, mpg >= 20 & mpg <= 25))))
})



app$set_inputs(`filter-column_select_edit_btn` = "click")

test_that("test editing column removes vector filter", {
  expect_true(!"filter-vector_filter-param" %in% lapply(app$get_values(), names))
})


app$stop()
