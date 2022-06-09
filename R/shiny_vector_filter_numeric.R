#' @importFrom shiny uiOutput
#' @export
#' @keywords internal
shiny_vector_filter_ui.numeric = function(data, inputId) {
  ns <- NS(inputId)
  shiny::uiOutput(ns("ui"))
}



#' @export
#' @keywords internal
shiny_vector_filter.numeric <- function(data, inputId, ...) {
  n_vals <- length(unique(as.character(data)))
  if (n_vals > 7)
    shiny_vector_filter_numeric_many
  else
    shiny_vector_filter_numeric_few
}