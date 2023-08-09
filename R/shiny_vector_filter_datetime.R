#' @importFrom shinyTime timeInput
#' @importFrom shiny NS uiOutput
#' @export
#' @keywords internal
shiny_vector_filter_ui.POSIXct <- function(data, inputId) {
  ns <- shiny::NS(inputId)
  shiny::uiOutput(ns("ui"))
}
 

#' @export
#' @keywords internal
shiny_vector_filter.POSIXct <- function(data, inputId, ...) {
  function(input, output, session, x = shiny::reactive(), 
           filter_na = shiny::reactive(FALSE), filter_fn = NULL, verbose = FALSE) {
    
    ns <- session$ns
    module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
    fn <- if (is.null(filter_fn)) function(x) TRUE else purrr::possibly(filter_fn, otherwise = TRUE)
    
    x_filtered <- Filter(function(x) !is.na(x) & fn(x), x())
    
    tzone <- reactive(attr(x(), "tzone"))

    output$ui <- shiny::renderUI({
      filter_log("updating ui", verbose = verbose)
      shiny::div(
        shiny::div(style = "
                   margin: 0px 11px -25px 11px;
                   height: 25px;
                   animation: 
                   0.75s ease-out 0s 1 shinyDataFilterEnlargeY, 
                   0.5s ease-in  0s 1 shinyDataFilterFadeIn; 
                   transform-origin: bottom;"),
        if (any(!is.na(x()))) {
          my_date <- as.Date(x())
          div( 
            div(style = "display: inline-block; vertical-align:middle;",
                    shiny::dateInput(ns("st_date"), "Start Date", value = isolate(input$st_date) %||% min(as.Date(x_filtered))
                                 , min = min(my_date, na.rm = TRUE), max = max(my_date, na.rm = TRUE)),
                shinyTime::timeInput(ns("st_time"), "Start Time (HH:MM:SS)", value = isolate(input$st_time) %||% min(x_filtered))# automatically takes the time element
                ),    
            
            div(style = "display: inline-block; vertical-align:middle;",
                    shiny::dateInput(ns("end_date"), "End Date", value = isolate(input$end_date) %||% max(as.Date(x_filtered))
                                 , min = min(my_date, na.rm = TRUE), max = max(my_date, na.rm = TRUE)),
                shinyTime::timeInput(ns("end_time"), "End Time (HH:MM:SS)", value = isolate(input$end_time) %||% max(x_filtered))  # automatically takes the time element
            )
          )
        } else {
          shiny::div(
            style = "padding-top: 10px; opacity: 0.3; text-align: center;",
            shiny::tags$h5(shiny::tags$i("no POSIXct values")))
        })
    })
    
    st_dt <- reactive({
      st <- substr(strftime(input$st_time, "%Y-%m-%d %H:%M:%S", tz = tzone()),12,20)
      as.POSIXct(paste(input$st_date, st), tz = tzone())
    })
    end_dt <- reactive({
      end <- substr(strftime(input$end_time, "%Y-%m-%d %H:%M:%S", tz = tzone()),12,20)
      as.POSIXct(paste(input$end_date, end), tz = tzone())
    })
    
    module_return$code <- shiny::reactive({
      exprs <- list()

      if (!is.null(input$st_date) & !is.null(input$st_time) & !is.null(input$end_date) & !is.null(input$end_time)) {
        if (st_dt() > min(x(), na.rm = TRUE))
          exprs <- append(exprs, bquote(.x >= .(st_dt())))
        if (end_dt() < max(x(), na.rm = TRUE))
          exprs <- append(exprs, bquote(.x <= .(end_dt())))
      }
      
      if (length(exprs) > 1) {
        expr <- Reduce(function(l, r) bquote(.(l) & .(r)), exprs)
        if (!filter_na()) expr <- bquote(is.na(.x) | (.(expr)))
      } else if (length(exprs) == 1) {
        expr <- exprs[[1]]
        if (!filter_na()) expr <- bquote(is.na(.x) | .(expr))
      } else if (filter_na()) {
        expr <- bquote(!is.na(.x))
      } else {
        return(TRUE)
      }
      
      expr
    })
    
    module_return$mask <- shiny::reactive({
      eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
    })
    
    module_return
  }
}
