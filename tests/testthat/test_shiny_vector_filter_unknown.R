context("test_shiny_vector_filter_unknown")

app_path <- IDEAFilter:::shinytest_path("shinytest_shiny_vector_filter")
app <- shinytest2::AppDriver$new(app_path)

data <- list(1, 2, 3)
app$set_inputs(`data_dput` = paste(capture.output(dput(data)), paste = "\n"))
app$wait_for_js('document.getElementById("data_display")')



test_that("testing that unknown datatypes show error message", {
  expect_true(grepl("don't know how to ", app$get_value(output = "data_display")$html))
})



app$stop()
