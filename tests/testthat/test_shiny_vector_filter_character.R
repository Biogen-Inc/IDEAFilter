context("test_shiny_vector_filter_character")

skip("Test built on old structure, no longer appropriate.")

app_path <- IDEAFilter:::shinytest_path("shinytest_shiny_vector_filter")
# app <- shinytest::ShinyDriver$new(app_path)
app <- shinytest2::AppDriver$new(app_path)

data <- c(paste(letters[1:2], letters[1:6], sep = ""), NA)
# app$setInputs(`data_dput` = paste(capture.output(dput(data)), paste = "\n"))
# app$waitFor("test_in-param")
app$set_inputs(`data_dput` = paste(capture.output(dput(data)), paste = "\n"))
app$wait_for_js('document.getElementById("test_in-param")')



test_that("testing that character vectors get filtered properly", {
  # app$setInputs(`test_in-param` = "a")
  # app$setInputs(`filter_na` = TRUE)
  app$set_inputs(`test_in-param` = "a")
  app$set_inputs(`filter_na` = TRUE)
  
  expect_equal(
    app$getValue("test_mask"), 
    renderPrint(grepl("a", data))())
  
  expect_true({
    filtered_data <- eval(parse(text = app$getValue("test_filtered_dput")))
    !any(is.na(filtered_data))
  })
  
  app$setInputs(`filter_na` = FALSE)
  
  expect_true({
    filtered_data <- eval(parse(text = app$getValue("test_filtered_dput")))
    any(is.na(filtered_data))
  })
})



test_that("testing that character vector filter code builds properly", {
  app$setInputs(`test_in-param` = "b")
  app$setInputs(`filter_na` = TRUE)
  
  expect_equal(
    app$getValue("test_code"), 
    renderPrint(quote(grepl("b", tolower(.x), fixed = TRUE)))())
  
  app$setInputs(`filter_na` = FALSE)
  
  expect_equal(
    app$getValue("test_code"), 
    renderPrint(quote(is.na(.x) | grepl("b", tolower(.x), fixed = TRUE)))())
})



app$stop()
