ui <- fluidPage(
  titlePanel("Filter Data Example"),
  fluidRow(
    column(8,
      selectInput("filter_select", NULL, choices = c("filter_1", "filter_2")),
      verbatimTextOutput("data_summary"),
      verbatimTextOutput("data_filter_code")),
    column(4, IDEAFilter::IDEAFilter_ui("data_filter"))))

srv <- function(input, output, session) {
  
  preselection <- reactiveVal(list(Ozone = list(filter_na = TRUE, filter_fn = ~ .x >= 30),
                                   Solar = list(filter_fn = ~ .x > 100),
                                   Month = list(filter_fn = ~ .x == 9)))
  
  observeEvent(input$filter_select, {
    if (input$filter_select == "filter_1")
      preselection(list(Ozone = list(filter_na = TRUE, filter_fn = ~ .x >= 30),
                        Solar = list(filter_fn = ~ .x > 100),
                        Month = list(filter_fn = ~ .x == 9)))
    else
      preselection(list(Ozone = list(filter_fn = ~ .x >= 30 & .x <= 90),
                        Wind = list(filter_fn = ~.x >= 5 & .x <= 10)))
  })
  
filtered_data <- IDEAFilter::IDEAFilter(
    "data_filter",
    data = airquality,
    preselection = preselection,
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
