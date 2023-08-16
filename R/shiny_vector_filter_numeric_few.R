#' A vector filter for numeric variables with only a few choices
#'
#' @param input requisite shiny module field specifying incoming ui input
#'   reactiveValues
#' @param output requisite shiny module field capturing output for the shiny
#'   data filter ui
#' @param session requisite shiny module field containing the active shiny
#'   session
#' @param x The TODO
#' @param filter_na The \code{logical} TODO
#' @param filter_fn A function to modify, specified in one of the following ways:
#'   * A named function, e.g. `mean`.
#'   * An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.
#'   * A formula, e.g. `~ .x + 1`.
#' @param verbose a \code{logical} value indicating whether or not to print log
#'   statements out to the console
#'
#' @importFrom shiny reactive reactiveValues renderUI div plotOutput sliderInput
#'   isolate tags validate need renderPlot
#' @importFrom ggplot2 ggplot aes aes_ geom_area theme_void scale_x_continuous
#'   scale_y_continuous
#' @importFrom grDevices rgb
#' @importFrom stats density
#'
#' @return a \code{\link[shiny]{reactiveValues}} list containing a logical
#'   vector called "mask" which can be used to filter the provided vector and an
#'   element "code" which is the expression used to generate the mask.
#' @export
#' @keywords internal
shiny_vector_filter_numeric_few <- function(input, output, session,
            x = shiny::reactive(factor()),  #important: changed x to factor here
           filter_na = shiny::reactive(FALSE), filter_fn = NULL, verbose = FALSE,
           erase_filters = shiny::reactive(0)) {
    
  ns <- session$ns
  
  x_wo_NA <- shiny::reactive(Filter(Negate(is.na), x()))
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
  fn <- if (is.null(filter_fn)) function(x) FALSE else purrr::possibly(filter_fn, otherwise = FALSE)
  
  x_filtered <- Filter(function(x) !is.na(x) & fn(x), x())
  
  choices <- shiny::reactive(unique(as.character(sort(x_wo_NA()))))
  
  output$ui <- shiny::renderUI({
    filter_log("updating ui", verbose = verbose)
    shiny::div(style = "position: relative;",
               shiny::div(style = "
                          position: absolute; 
                          top: -2px; right: 16px; bottom: -2px; left: 16px;
                          animation: 
                          0.75s ease-out 0s 1 shinyDataFilterEnlargeX, 
                          0.5s ease-in  0s 1 shinyDataFilterFadeIn; 
                          transform-origin: left;" #,
               ),
               shiny::checkboxGroupInput(ns("param"), NULL,
                                         choices = choices(),
                                         selected = isolate(input$param) %||% x_filtered,
                                         width = "100%"))
  })
  session$userData$eraser_observer <-
    observeEvent(
      erase_filters(), 
      updateCheckboxGroupInput(session, "param", selected = ""),
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
    eval(do.call(substitute, list(module_return$code(), list(.x = x())))) # added numeric() to return val, got errors. Then removed
  })
  
  module_return
}
