#' @importFrom shiny NS uiOutput
#' @export
shiny_vector_filter_ui.character <- function(data, inputId) {
  ns <- shiny::NS(inputId)
  shiny::uiOutput(ns("ui"))
}

#' @importFrom shiny reactive reactiveValues renderUI textInput isolate
#' @importFrom purrr map reduce
#' @export
#' @keywords internal
shiny_vector_filter.character <- function(data, inputId, ...) {
    function(input, output, session, x = shiny::reactive(character()), 
             filter_na = shiny::reactive(FALSE), filter_fn = NULL, verbose = FALSE,
             erase_filters = shiny::reactive(0)) {
  
  ns <- session$ns
  
  #x = unique(x())
  x_wo_NAs <- shiny::reactive(Filter(Negate(is.na), x()))
  
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
  fn <- if (is.null(filter_fn)) function(x) FALSE else purrr::possibly(filter_fn, otherwise = FALSE)
  
  x_filtered <- Filter(function(x) !is.na(x) & fn(x), x())
  
  output$ui <- shiny::renderUI({
    
    filter_log("updating ui", verbose = verbose)
    
    if (purrr::reduce(purrr::map(x(), is.empty), `&`)) {
      shiny::div(style = "opacity: 0.5;",
                 p(width = "100%", 
                   align = "center", 
                   "Variable only contains blank values"))
    } else {
      proportionSelectInput(ns("param"), NULL,
                            vec = x,
                            selected = isolate(input$param) %||% x_filtered,
                            multiple = TRUE,
                            width = "100%")
    }

  })
  session$userData$eraser_observer <-
    observeEvent(
      erase_filters(), 
      updateSelectizeInput(session, "param", selected = ""),
      ignoreInit = TRUE
    )
  
  module_return$code <- shiny::reactive({
    if (length(input$param))
      bquote(.x %in% .(c(if (filter_na()) c() else NA, input$param)))
    else if (filter_na())
      bquote(!is.na(.x))
    else
      TRUE
  })
  
  module_return$mask <- shiny::reactive({
    eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
  })
  module_return
    }
}
