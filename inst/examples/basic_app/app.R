library(shiny)
library(IDEAFilter)
library(dplyr)
library(haven)

starwars2 <- iris

# View(starwars2)

ui <- fluidPage(
  titlePanel("Filter Data Example"),
  fluidRow(
    column(8,
           verbatimTextOutput("data_filter_code"),
           dataTableOutput("data_summary")
    ),
    column(4, shiny_data_filter_ui("data_filter"))))

server <- function(input, output, session) {
  
  filtered_data <- callModule(
    IDEAFilter::shiny_data_filter,
    "data_filter",
    data = starwars2,
    verbose = FALSE)
  
  output$data_filter_code <- renderPrint({
    cat(gsub("%>%", "%>% \n ",
             gsub("\\s{2,}", " ",
                  paste0(
                    capture.output(attr(filtered_data(), "code")),
                    collapse = " "))
    ))
  })
  
  output$data_summary <- renderDataTable({
    filtered_data() 
  }, 
  options = list(
    scrollX = TRUE,
    pageLength = 5
  ))
  
}

shinyApp(ui = ui, server = server)