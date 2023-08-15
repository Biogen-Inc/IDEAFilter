#' A vector filter for factors with only a few choices
#'
#' @param input requisite shiny module field specifying incoming ui input
#'   reactiveValues
#' @param output requisite shiny module field capturing output for the shiny
#'   data filter ui
#' @param session requisite shiny module field containing the active shiny
#'   session
#' @param x a reactive expression resolving to the vector to filter
#' @param filter_na a logical value indicating whether to filter \code{NA}
#'   values from the \code{x} vector
#' @param filter_fn A function to modify, specified in one of the following ways:
#'   * A named function, e.g. `mean`.
#'   * An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.
#'   * A formula, e.g. `~ .x + 1`.
#' @param verbose a \code{logical} value indicating whether or not to print log
#'   statements out to the console
#'
#' @return a \code{\link[shiny]{reactiveValues}} list containing a logical
#'   vector called "mask" which can be used to filter the provided vector and an
#'   element "code" which is the expression used to generate the mask.
#'
#' @importFrom shiny reactive reactiveValues renderUI selectInput isolate
#' @keywords internal
shiny_vector_filter_factor_many <- function(input, output, session, 
    x = shiny::reactive(factor()), filter_na = shiny::reactive(FALSE), filter_fn = NULL, 
    verbose = FALSE,
    erase_filters = shiny::reactive(0)) {
  
  ns <- session$ns

  x_wo_NAs <- shiny::reactive(Filter(Negate(is.na), x()))
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
  fn <- if (is.null(filter_fn)) function(x) FALSE else purrr::possibly(filter_fn, otherwise = FALSE)
  
  x_filtered <- Filter(function(x) !is.na(x) & fn(x), x())
  
  output$ui <- shiny::renderUI({
    filter_log("updating ui", verbose = verbose)
    proportionSelectInput(ns("param"), NULL,
      vec = x,
      selected = isolate(input$param) %||% x_filtered,
      multiple = TRUE,
      width = "100%")
  })
  observeEvent(erase_filters(), updateSelectizeInput(session, "param", selected = ""))
  
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
