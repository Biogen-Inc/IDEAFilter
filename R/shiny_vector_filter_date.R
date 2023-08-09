#' @importFrom shiny NS uiOutput
#' @export
#' @keywords internal
shiny_vector_filter_ui.Date <- function(data, inputId) {
  ns <- shiny::NS(inputId)
  shiny::uiOutput(ns("ui"))
}



#' @export
#' @keywords internal
shiny_vector_filter.Date <- function(data, inputId, ...) {
  function(input, output, session, x = shiny::reactive(Date()), 
           filter_na = shiny::reactive(FALSE), filter_fn = NULL, verbose = FALSE) {
    
    ns <- session$ns
    module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
    fn <- if (is.null(filter_fn)) function(x) TRUE else purrr::possibly(filter_fn, otherwise = TRUE)
    
    x_filtered <- Filter(function(x) !is.na(x) & fn(x), x())
    
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
          shiny::dateRangeInput(ns("param"), NULL,
                             #value = shiny::isolate(input$param) %||% range(x(), na.rm = TRUE), 
                             start = isolate(input$param[[1]]) %||% min(x_filtered), 
                             end = isolate(input$param[[2]]) %||% max(x_filtered),
                             min = min(x(), na.rm = TRUE), 
                             max = max(x(), na.rm = TRUE)
                             )
        } else {
          shiny::div(
            style = "padding-top: 10px; opacity: 0.3; text-align: center;",
            shiny::tags$h5(shiny::tags$i("no numeric values")))
        })
    })
    
    module_return$code <- shiny::reactive({
      exprs <- list()
      
      if (!is.null(input$param)) {
        if (input$param[[1]] > min(x(), na.rm = TRUE))
          exprs <- append(exprs, bquote(.x >= .(as.numeric(input$param[[1]]))))
        if (input$param[[2]] < max(x(), na.rm = TRUE))
          exprs <- append(exprs, bquote(.x <= .(as.numeric(input$param[[2]]))))
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
