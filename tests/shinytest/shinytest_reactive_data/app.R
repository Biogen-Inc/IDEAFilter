mtcars2 <- mtcars
mtcars2[which((mtcars2 * 0.987) %% 0.2 < 0.01, arr.ind = TRUE)] <- NA

ui <- fluidPage(
  fluidRow(
    column(8, 
           selectInput("select_data", "Select Data", c("airquality", "mtcars"), selected = "airquality"),
           verbatimTextOutput("data_summary"),
           verbatimTextOutput("data_filter_code")),
    column(4, IDEAFilter::IDEAFilter_ui("data_filter"))))

srv <- function(input, output, session) {
  data <- reactiveVal(airquality)
  filtered_data <- IDEAFilter::IDEAFilter(
    "data_filter", 
    data = data, 
    verbose = FALSE)
  
  exportTestValues(
    filtered_data = filtered_data()
  )
  
  observeEvent(input$select_data, {
    if (input$select_data == "airquality")
      data(airquality)
    else
      data(mtcars2)
  })
  
  output$data_filter_code <- renderPrint({
    cat(gsub("%>%", "%>% \n ",
             gsub("\\s{2,}", " ",
                  paste0(
                    capture.output(attr(filtered_data(), "code")),
                    collapse = " "))
    ))
  })
  
  output$data_summary <- renderPrint({
    if (nrow(filtered_data())) show(filtered_data())
    else "No data available"
  })
  
}

shinyApp(ui, srv)
