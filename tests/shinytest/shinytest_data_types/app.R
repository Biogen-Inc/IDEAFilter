ui <- fluidPage(
  titlePanel("Filter Data Example"),
  fluidRow(
    column(8,
           verbatimTextOutput("data_summary"),
           verbatimTextOutput("data_filter_code")),
    column(4, IDEAFilter_ui("data_filter"))))

srv <- function(input, output, session) {
  filtered_data <- IDEAFilter(
    "data_filter",
    data = vector_data,
    verbose = FALSE)
  
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