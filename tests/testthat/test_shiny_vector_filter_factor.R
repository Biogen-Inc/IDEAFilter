context("test_shiny_vector_filter_factor")
skip_on_cran()

app_path <- IDEAFilter:::shinytest_path("shinytest_shiny_vector_filter")
app <- shinytest2::AppDriver$new(app_path)

data <- as.factor(c(letters[1:10], NA))
app$set_inputs(`data_dput` = paste(capture.output(dput(data)), collapse = "\n"))
app$wait_for_js('document.getElementById("test_in-param")')



test_that("testing that factor (many) vectors get filtered properly", {
  app$set_inputs(`test_in-param` = c("a", "b"))
  app$set_inputs(`filter_na` = TRUE)
  
  expect_equal(
    app$get_value(output = "test_mask"), 
    renderPrint(data %in% c("a", "b"))())
  
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



test_that("testing that factor (many) vector filter code builds properly", {
  app$set_inputs(`test_in-param` = c("d", "c"))
  app$set_inputs(`filter_na` = TRUE)
  
  expect_equal(
    app$get_value(output = "test_code"), 
    renderPrint(quote(.x %in% c("d", "c")))())
  
  app$set_inputs(`filter_na` = FALSE)
  
  expect_equal(
    app$get_value(output = "test_code"), 
    renderPrint(quote(.x %in% c(NA, "d", "c")))())
})



data <- as.factor(c(letters[1:3], NA))
app$set_inputs(`data_dput` = paste(capture.output(dput(data)), collapse = "\n"))



test_that("testing that factor (few) vectors get filtered properly", {
  app$set_inputs(`test_in-param` = c("a", "b"))
  app$set_inputs(`filter_na` = TRUE)
  
  expect_equal(
    app$get_value(output = "test_mask"), 
    renderPrint(data %in% c("a", "b"))())
  
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



test_that("testing that factor (few) vector filter code builds properly", {
  app$set_inputs(`test_in-param` = c("c", "a"))
  app$set_inputs(`filter_na` = TRUE)
  
  expect_equal(
    app$get_value(output = "test_code"), 
    renderPrint(quote(.x %in% c("a", "c")))())
  
  app$set_inputs(`filter_na` = FALSE)
  
  expect_equal(
    app$get_value(output = "test_code"),
    renderPrint(quote(.x %in% c(NA, "a", "c")))())
})



app$stop()
