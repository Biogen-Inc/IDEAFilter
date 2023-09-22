#' @importFrom shiny div
#' @export
#' @keywords internal
shiny_vector_filter_ui.NULL = function(data, inputId) {
  shiny::div()
}


#' @importFrom shiny reactive reactiveValues
#' @export
#' @keywords internal
shiny_vector_filter.NULL <- function(data, inputId, ...) {
  function(input, output, session, x = shiny::reactive(NULL), 
    filter_na = shiny::reactive(FALSE), filter_fn = NULL, verbose = FALSE,
    erase_filters = shiny::reactive(0)) { 
    
    module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
    module_return$code <- shiny::reactive(TRUE)
    module_return$mask <- shiny::reactive(TRUE) 
    
    module_return
  }
}
