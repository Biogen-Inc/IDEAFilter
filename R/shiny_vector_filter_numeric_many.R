#' A vector filter for numeric variables with only many choices
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
#'  statements out to the console
#'  
#' @importFrom shiny reactive reactiveValues renderUI div plotOutput sliderInput
#'   isolate tags validate need renderPlot
#' @importFrom ggplot2 ggplot aes aes_ geom_area theme_void scale_x_continuous
#'   scale_y_continuous
#' @importFrom grDevices rgb
#' @importFrom stats density
#' @importFrom purrr possibly
#' 
#' @return a \code{\link[shiny]{reactiveValues}} list containing a logical
#'   vector called "mask" which can be used to filter the provided vector and an
#'   element "code" which is the expression used to generate the mask.
#' @export
#' @keywords internal
shiny_vector_filter_numeric_many <- function(input, output, session, x = shiny::reactive(numeric()), 
           filter_na = shiny::reactive(FALSE), filter_fn = NULL, verbose = FALSE,
           erase_filters = shiny::reactive(0)) {
    
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
          shiny::sliderInput(ns("param_many"), NULL,
                             value = range(isolate(input$param_many) %||% x_filtered), 
                             min = min(round(x(), 1), na.rm = TRUE), 
                             max = max(round(x(), 1), na.rm = TRUE))
        } else {
          shiny::div(
            style = "padding-top: 10px; opacity: 0.3; text-align: center;",
            shiny::tags$h5(shiny::tags$i("no numeric values")))
        })
    })
    session$userData$eraser_observer <-
      observeEvent(
        erase_filters(), 
        updateSliderInput(session, "param_many", value = range(x(), na.rm = TRUE)),
        ignoreInit = TRUE
      )
    
    module_return$code <- shiny::reactive({
      exprs <- list()
      last_n <- length(input$param_many)
      
      if (!is.null(input$param_many)) {
        if (input$param_many[[1]] > min(x(), na.rm = TRUE))
          exprs <- append(exprs, bquote(.x >= .(as.numeric(input$param_many[[1]]))))
        if (input$param_many[[last_n]] < max(x(), na.rm = TRUE))
          exprs <- append(exprs, bquote(.x <= .(as.numeric(input$param_many[[last_n]]))))
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
