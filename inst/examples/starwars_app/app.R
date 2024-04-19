library(shiny)
library(IDEAFilter)
library(dplyr)  # for data pre-processing and example data
 
# prep a new data.frame with more diverse data types
starwars2 <- starwars %>%
 mutate_if(~is.numeric(.) && all(Filter(Negate(is.na), .) %% 1 == 0), as.integer) %>%
 mutate_if(~is.character(.) && length(unique(.)) <= 25, as.factor) %>%
 mutate(is_droid = species == "Droid") %>%
 select(name, gender, height, mass, hair_color, eye_color, vehicles, is_droid)

# create some labels to showcase column select input
attr(starwars2$name, "label")     <- "name of character"
attr(starwars2$gender, "label")   <- "gender of character"
attr(starwars2$height, "label")   <- "height of character in centimeters"
attr(starwars2$mass, "label")     <- "mass of character in kilograms"
attr(starwars2$is_droid, "label") <- "whether character is a droid"

ui <- fluidPage(
 titlePanel("{IDEAFilter} Example: Star Wars App"),
 fluidRow(
   column(8, 
     dataTableOutput("data_summary"),
     h4("Generated code:"),
     verbatimTextOutput("data_filter_code")),
   column(4, 
          varSelectizeInput("col_subset", "Choose Column Subset", starwars2, multiple = TRUE),
          div(
            class = "form-group",
            tags$label(class = "control-label", "Choose Pre-selection"),
            div(
              style = "display: flex",
              actionButton("ex1", HTML("<b>gender:</b> feminine<br><b>height:</b> >= 180cm"), width = "50%"),
              actionButton("ex2", HTML("<b>is_droid:</b> TRUE; not NA<br><b>mass:</b> < 50kg"), width = "50%")
            )
          ),
          hr(),
          br(),
          IDEAFilter_ui("data_filter"))
   ))

server <- function(input, output, session) {
  
  preselection <- reactiveVal(NULL)
  observeEvent(input$ex1, {
    preselection(
      list(gender = list(filter_fn = ~ .x == "feminine"),
           height = list(filter_fn = ~ .x >= 180))
    )
  })
  observeEvent(input$ex2, {
    preselection(
      list(is_droid = list(filter_na = TRUE, filter_fn = ~ isTRUE(.x)),
           mass = list(filter_fn = ~ .x < 50))
    )
  })
  
 filtered_data <- IDEAFilter("data_filter", data = starwars2, col_subset = reactive(input$col_subset), preselection = preselection, verbose = FALSE)
 
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
   pageLength = 8
 ))
 
}
 
shinyApp(ui = ui, server = server)