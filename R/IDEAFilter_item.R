#' A single filter item as part of a IDEA filter module panel
#' 
#' This is a wrapper for \code{\link{shiny_data_filter_item_ui}} created to
#' match up with the module server function \code{\link{IDEAFilter_item}}.
#' 
#' @param id a module id name
#'   
#' @return a shiny \code{\link[shiny]{wellPanel}} to house the filter
#' 
#' @importFrom shiny NS uiOutput
#' @export
#' @keywords internal
#' 
IDEAFilter_item_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"),
                  class = "list-group-item well",
                  style = "padding: 6px 14px 0px 14px; margin-bottom: 6px;",
                  `data-id` = id)
}

#' The server function for the IDEA filter item module
#' 
#' Serves as a wrapper fo \code{\link{shiny_data_filter_item}} and utilizes
#' \code{moduleSever()} for a more modern implementation of the data item
#' filter.
#' 
#' @param id a module id name
#' @param data a \code{reactive expression} returning a \code{data.frame} to use
#'   as the input to the filter item module
#' @param column_name a value indicating the name of the column to be filtered
#' @param ... placeholder for inclusion of additional parameters in future development
#' @param verbose a \code{logical} value indicating whether or not to print log
#'   statements out to the console
#'   
#' @return a \code{\link[shiny]{reactiveValues}} list of three reactive elements;
#'   (1) a reactive data frame, (2) the code to filter a vector with the name of
#'   the specified data column, and (3) a flag indicating when to remove this
#'   filter.
#'   
#' @importFrom shiny reactiveValues wellPanel fillRow selectInput h4 actionLink
#'   icon uiOutput div HTML span textOutput eventReactive renderUI tag
#'   renderText reactive observeEvent callModule
#' @export
#' @keywords internal
#' 
IDEAFilter_item <- function(id, data, column_name = NULL, ..., filters = list(), verbose = FALSE) {
  filters <- if (is.reactive(filters)) filters else reactiveVal(filters)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    module_return <- shiny::reactiveValues(
      # data = data,
      code = TRUE, 
      remove = FALSE, 
      filters = filters,
      column_name = column_name)
    
    filter_logical <- reactive({
      if (!length(filters())) rep(TRUE, nrow(data())) else Reduce("&", Map(function(x) with(data(), eval(x)), filters()))
      })
    
    
    filter_log("calling filter item", verbose = verbose)
    
    # ui to show to select a new filter column
    column_select_ui <- reactive(shiny::fillRow(
      flex = c(NA, 1, NA), 
      height = "40px",
      shiny::h4(style = 'margin-right: 8px;',
                shiny::icon("grip-vertical", class = "sortableJS-handle")),
      columnSelectInput(
        ns("column_select"),
        NULL,
        data = data,
        multiple = TRUE,
        width = '100%'),
      shiny::h4(style = 'float: right; margin: 8px 0px 0px 8px;',
                shiny::actionLink(
                  ns('remove_filter_btn'),
                  NULL,
                  shiny::icon('times-circle')))))
    
    # filter column-specific ui
    column_filter_ui <- shiny::tagList(
      shiny::h4(
        shiny::icon("grip-vertical", class = "sortableJS-handle"),
        shiny::uiOutput(ns("column_name"), inline = TRUE),
        shiny::actionLink(
          ns("column_select_edit_btn"),
          NULL,
          shiny::icon("edit")),
        shiny::div(style = "display: inline-block; opacity: 0.3; font-style: italic;", 
                   shiny::HTML(paste0(
                     shiny::span("("),
                     shiny::textOutput(ns("nrow"), inline = TRUE),
                     shiny::uiOutput(ns("filter_na_btn_ui"), inline = TRUE),
                     shiny::span(")")))),
        shiny::actionLink(ns("remove_filter_btn"), NULL,
                          style = 'float: right;',
                          shiny::icon("times-circle"))
      ),
      shiny::uiOutput(ns("vector_filter_ui")))
    
    ui <- shiny::eventReactive(module_return$column_name, ignoreNULL = FALSE, {
      if (is.null(module_return$column_name)) column_select_ui()
      else column_filter_ui
    })
    
    output$ui <- shiny::renderUI({
      filter_log("updating ui", verbose = verbose)
      ui()
    })
    
    output$filter_na_btn_ui <- shiny::renderUI({
      if (nna() <= 0) return()
      shiny::HTML(paste0(
        shiny::span(", "),
        shiny::actionLink(style = "text-decoration: none;",
                          ns("filter_na_btn"), 
                          shiny::uiOutput(ns("nna_out"), inline = TRUE))))
    })
    
    output$nna_out <- shiny::renderUI({
      shiny::span(class = if (filter_na()) "shinyDataFilterStrikeout" else c(),
                  shiny::HTML(paste0(
                    nna(), 
                    shiny::tags$small(
                      style = "position: relative; bottom: 0.025em; left: 0.1em;", 
                      "NA"))))
    })
    
    
    output$vector_filter_ui <- shiny::renderUI({
      shiny_vector_filter_ui(vec(), ns("vector_filter"))
    })
    
    output$column_name <- shiny::renderUI({
      module_return$column_name
    })
    
    output$nrow <- shiny::renderText({
      out_log <- if (isTRUE(module_return$code())) filter_logical() else with(data(), eval(module_return$code())) & filter_logical()
      sum(out_log, na.rm = TRUE)
      })
    
    filter_na <- shiny::reactive({
      if (is.null(input$filter_na_btn)) FALSE
      else input$filter_na_btn %% 2 == 1
    })
    
    x <- shiny::eventReactive(filter_na(), { filter_log("observing filter_na")})
    
    nna <- shiny::reactive(sum(is.na(vec())))
    
    vec <- shiny::reactive({
      if (is.null(module_return$column_name) || !(module_return$column_name %in% names(data()))) NULL
      else data()[filter_logical(), module_return$column_name, drop = TRUE]
    })
    
    shiny::observeEvent(input$column_select, {
      module_return$column_name <- input$column_select
    })
    
    shiny::observeEvent(input$column_select_edit_btn, {
      module_return$column_name <- NULL
    })
    
    shiny::observeEvent(input$remove_filter_btn, {
      module_return$remove <- TRUE
    })
    
    vector_module_srv <- shiny::reactive(shiny_vector_filter(vec(), "vec"))
    
    vector_module_return <- shiny::reactive({
      shiny::callModule(
        vector_module_srv(), 
        "vector_filter", 
        x = vec, 
        filter_na = filter_na,
        verbose = verbose)
    })
    
    module_return$code <- shiny::reactive({
      if (is.null(module_return$column_name)) return(TRUE)
      
      do.call(substitute, list(
        vector_module_return()$code(), 
        list(.x = as.name(module_return$column_name))))
    })
    
    module_return$filters <- shiny::reactive({
      Filter(Negate(isTRUE), 
             append(filters(), module_return$code()))
    })
    
    module_return
  })
}
