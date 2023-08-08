data <- mtcars
data[which((data * 0.987) %% 0.2 < 0.01, arr.ind = TRUE)] <- NA

ui <- fluidPage(
  mainPanel(verbatimTextOutput("data_display")),
  sidebarPanel(IDEAFilter::IDEAFilter_item_ui("filter")))

srv <- function(input, output, session) {
  filtered_data <- IDEAFilter::IDEAFilter_item(
    "filter",
    data = reactive(data))
  
  filter_logical <- reactiveVal(TRUE)
  observe({
    filter_exprs <- Filter(
      Negate(isTRUE), 
      Map(function(fi) filtered_data$code(), filtered_data$filters()))
    
    filter_logical(if (!length(filter_exprs)) rep(TRUE,nrow(data)) else Reduce("&", Map(function(x) with(data, eval(x)), filter_exprs)))
  })
  
  output$data_display <- renderPrint({
    print(IDEAFilter:::`%||%`(subset(data, filter_logical()), data.frame()))
  })
}

shinyApp(ui, srv)
