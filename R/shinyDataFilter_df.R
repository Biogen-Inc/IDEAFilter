#' Handler for retrieving static code for a shinyDataFilter_df
#'
#' A function for scriptgloss::getInitializationCode to dispatch to
#'
#' @param obj shinyDataFilter_df object
#' @param name unused
#' @return Returns the 'code' attribute as a string from any object of class
#'   shinyDataFilter_df
#'
#' @export
#' @keywords internal
getInitializationCode.shinyDataFilter_df <- function(obj, name = NULL) {
  attr(obj, "code")
}