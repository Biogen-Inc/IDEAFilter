context("test_shiny_vector_filter_numeric")
skip_on_cran()

app_path <- IDEAFilter:::shinytest_path("shinytest_shiny_vector_filter")
app <- shinytest2::AppDriver$new(app_path)

data <- c(1:9, NA)
app$set_inputs(`data_dput` = paste(capture.output(dput(data)), paste = "\n"))
app$wait_for_js('document.getElementById("test_in-param")')


test_that("testing that numeric vectors get filtered properly", {
  app$set_inputs(`test_in-param` = c(3, 6))
  app$set_inputs(`filter_na` = TRUE)
  
  expect_equal(
    app$get_value(output = "test_mask"), 
    renderPrint(data >= 3 & data <= 6)())
  
  expect_true({
    filtered_data <- eval(parse(text = app$get_value(output = "test_filtered_dput")))
    !any(is.na(filtered_data))
  })
  
  app$set_inputs(`filter_na` = FALSE)
  
  expect_true({
    filtered_data <- eval(parse(text = app$get_value(output = "test_filtered_dput")))
    any(is.na(filtered_data))
  })
})



test_that("testing that numeric vector filter code builds properly", {
  app$set_inputs(`test_in-param` = c(5, 8))
  app$set_inputs(`filter_na` = TRUE)
  
  expect_equal(
    app$get_value(output = "test_code"), 
    renderPrint(quote(.x >= 5 & .x <= 8))())
  
  app$set_inputs(`filter_na` = FALSE)
  
  expect_equal(
    app$get_value(output = "test_code"), 
    renderPrint(quote(is.na(.x) | (.x >= 5 & .x <= 8)))())
})


## Plot is no longer output
# test_that("testing that numeric vector filter builds a plot", {
#   # app$setInputs(`test_in-param` = c(5, 8))
#   # app$setInputs(`filter_na` = TRUE)
#   app$set_inputs(`test_in-param` = c(6, 8))
#   app$set_inputs(`filter_na` = TRUE)
#   
#   expect_true({
#     all(grep("data:image/png", app$getAllValues()$output$`test_in-plot`$src))
#   })
# })


app$stop()
