#' User interface function to add a data filter panel
#' 
#' This is a wrapper for \code{\link{shiny_data_filter_ui}} created to match up
#' with the module server function \code{\link{IDEAFilter}}.
#' 
#' @param id a module id name
#' @return a shiny \code{\link[shiny]{tagList}} containing the filter ui
#' 
#' @import shiny
#' 
#' @importFrom shiny NS tagList div actionButton icon
#' @export
#' @keywords internal
#' @seealso \link{shiny_data_filter_ui}, \link{IDEAFilter}
#' 
#' @inherit shiny_data_filter examples
#' 
IDEAFilter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinyDataFilter_resourcePath()
  
  shiny::tagList(
    css_sortableJS_style_script(),
    js_sortableJS_script(),
    css_shinyDataFilter_animation_script(),
    css_shinyDataFilter_style_script(),
    shiny::div(
      id = ns("sortableList"), 
      class = "listWithHandle list-group",
      style = "margin-bottom: 0;"),
    shiny::div(
      id = "shinyDataFilter-addFilter",
      width = "100%",
      uiOutput(ns("add_filter_select_ui")))
  )
}

#' IDEA data filter module server function
#' 
#' Serves as a wrapper for \code{\link{shiny_data_filter}} and utilizes
#' \code{moduleSever()} for a more modern implementation of the data item
#' filter.
#' 
#' @param id a module id name
#' @param data a \code{data.frame} or \code{reactive expression} returning a
#'   \code{data.frame} to use as the input to the filter module
#' @param ... placeholder for inclusion of additional parameters in future development
#' @param col_subset a \code{vector} containing the list of allowable columns to filter on
#' @param preselection a \code{list} that can be used to pre-populate the filter
#' @param verbose a \code{logical} value indicating whether or not to print log
#'   statements out to the console
#' 
#' @return a \code{reactive expression} which returns the filtered data wrapped
#'   in an additional class, "shinyDataFilter_df". This structure also contains
#'   a "code" field which represents the code needed to generate the filtered
#'   data.
#'
#' @seealso \link{IDEAFilter_ui}, \link{shiny_data_filter}
#'
#' @import shiny
#' @importFrom utils head tail
#' @importFrom stats setNames
#' @export
#' 
#' @examples
#' if(all(c(interactive(), require("dplyr"), require("IDEAFilter")))) {
#' library(shiny)
#' library(IDEAFilter)
#' library(dplyr)  # for data pre-processing and example data
#' 
#' # prep a new data.frame with more diverse data types
#' starwars2 <- starwars %>%
#'   mutate_if(~is.numeric(.) && all(Filter(Negate(is.na), .) %% 1 == 0), as.integer) %>%
#'   mutate_if(~is.character(.) && length(unique(.)) <= 25, as.factor) %>%
#'   mutate(is_droid = species == "Droid") %>%
#'   select(name, gender, height, mass, hair_color, eye_color, vehicles, is_droid)
#' 
#' # create some labels to showcase column select input
#' attr(starwars2$name, "label")     <- "name of character"
#' attr(starwars2$gender, "label")   <- "gender of character"
#' attr(starwars2$height, "label")   <- "height of character in centimeters"
#' attr(starwars2$mass, "label")     <- "mass of character in kilograms"
#' attr(starwars2$is_droid, "label") <- "whether character is a droid"
#' 
#' ui <- fluidPage(
#'   titlePanel("Filter Data Example"),
#'   fluidRow(
#'     column(8, 
#'       verbatimTextOutput("data_summary"),
#'       verbatimTextOutput("data_filter_code")),
#'     column(4, IDEAFilter_ui("data_filter"))))
#' 
#' server <- function(input, output, session) {
#'   filtered_data <- IDEAFilter("data_filter", data = starwars2, verbose = FALSE)
#'   
#'   output$data_filter_code <- renderPrint({
#'     cat(gsub("%>%", "%>% \n ", 
#'       gsub("\\s{2,}", " ", 
#'         paste0(
#'           capture.output(attr(filtered_data(), "code")), 
#'           collapse = " "))
#'     ))
#'   })
#'   
#'   output$data_summary <- renderPrint({
#'     if (nrow(filtered_data())) show(filtered_data())
#'     else "No data available"
#'   })
#' }
#' 
#' shinyApp(ui = ui, server = server)
#' }
#' 
IDEAFilter <- function(id, data, ..., col_subset = NULL, preselection = NULL, verbose = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filter_log("calling module", verbose = verbose)
    
    data_call <- as.list(sys.call(-7L))$data
    datar <- if (is.reactive(data)) data else reactive(data)
    col_subsetr <- if (is.reactive(col_subset)) col_subset else reactive(col_subset)
    preselectionr <- if (is.reactive(preselection)) preselection else reactive(preselection)
    
    filter_counter <- 0
    next_filter_id <- function() {
      filter_counter <<- filter_counter + 1
      sprintf("filter_%d", filter_counter)
    }
    
    filters <- reactiveVal(c("filter_0"))
    filter_returns <- list(filter_0 = reactiveValues(
      data = datar, 
      pre_filters = reactive(list()), 
      filters = reactive(list()),
      remove = NULL))
    
    update_filter <- function(fid, in_fid, column_name = NULL, preselection = NULL) {
      fs <- isolate(filters())
      
      if (missing(in_fid))
        if (fid %in% fs) in_fid <- fs[[utils::head(which(fid == fs), 1) - 1]]
      else in_fid <- utils::tail(fs, 1)
      
      if (!in_fid %in% fs | !in_fid %in% names(filter_returns))
        stop('no known filter for inbound filter id.')
      
      if (fid %in% names(filter_returns)) {
        filter_returns[[fid]]$pre_filters <- filter_returns[[in_fid]]$filters
      } else {
        filter_returns[[fid]] <<- IDEAFilter_item(
          fid,
          data = datar,
          column_name = column_name,
          filters = filter_returns[[in_fid]]$filters,
          col_subset = col_subsetr,
          preselection = preselection,
          verbose = verbose)
      }
    }
    
    apply_preselection <- function(preselection = NULL) {
      
      for (col_sel in (names(preselectionr()) %||% preselectionr())) {
        if (!col_sel %in% names(datar())) {
          warning(sprintf("Unable to add `%s` to filter list.", col_sel))
          next()
        }
        
        update_filter(fid <- next_filter_id(), column_name = col_sel, preselection = if(is.list(preselectionr())) preselectionr()[[col_sel]])
        filters(append(filters(), fid))
        
        insertUI(
          selector = sprintf("#%s", ns("sortableList")),
          where = "beforeEnd",
          ui = IDEAFilter_item_ui(ns(fid)))
        
        updateSelectInput(session, "add_filter_select", selected = "")
      }
    }
    
    output$add_filter_select_ui <- renderUI({
      req(datar())
      columnSelectInput(
        ns("add_filter_select"),
        label = NULL, 
        data = datar,
        col_subset = col_subsetr,
        placeholder = "Add Filter",
        width = "100%")
    })
    
    observe({
      filter_log("scrubbing filters tagged for removal", verbose = verbose)
      for (fid in filters()[-1])
        if (isTRUE(filter_returns[[fid]]$remove)) {
          idx <- utils::head(which(filters() == fid), 1)
          filter_returns[[fid]]$destroy
          
          filters(setdiff(filters(), fid))
          
          # overwrite existing module call with one taking new input data
          if (!idx > length(filters())) update_filter(filters()[[idx]])
          
          removeUI(selector = sprintf("#%s-ui", ns(fid)))
          break
        }
    })
    
    observeEvent(input$add_filter_btn, {
      filter_log("observing add filter button press", verbose = verbose)
      update_filter(fid <- next_filter_id())
      filters(append(filters(), fid))
      
      insertUI(
        selector = sprintf("#%s", ns("sortableList")),
        where = "beforeEnd",
        ui = IDEAFilter_item_ui(ns(fid)))
    })  
    
    observeEvent(input$add_filter_select, {
      req(preselectionr())
      
      filter_log("observing pre-selected columns", verbose = verbose)
      
      apply_preselection(preselectionr())
    }, once = TRUE)
    
    observeEvent(preselectionr(), {
      req(!is.null(input$add_filter_select))
      
      filter_log("scrubbing all filters", verbose = verbose)
      for (fid in filters()[-1]) {
        idx <- utils::head(which(filters() == fid), 1)
        filter_returns[[fid]]$destroy
        
        filters(setdiff(filters(), fid))
        
        # overwrite existing module call with one taking new input data
        if (!idx > length(filters())) update_filter(filters()[[idx]])
        
        removeUI(selector = sprintf("#%s-ui", ns(fid)))
      }
      
      filter_log("applying updated selection", verbose = verbose)
      apply_preselection(preselectionr())
      
    })
    
    observeEvent(input$add_filter_select, {
      if (!input$add_filter_select %in% names(datar())) return()
      
      filter_log("observing add filter button press", verbose = verbose)
      update_filter(fid <- next_filter_id(), column_name = input$add_filter_select)
      filters(append(filters(), fid))
      
      insertUI(
        selector = sprintf("#%s", ns("sortableList")),
        where = "beforeEnd",
        ui = IDEAFilter_item_ui(ns(fid)))
      
      updateSelectInput(session, "add_filter_select", selected = "")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # observe drag-and-drop and update data flow
    observeEvent(input$sortableList, {
      old_filters <- filters()
      
      filters(c(
        filters()[1],  # preserve input 'filter'
        gsub(sprintf("^%s", ns("")), "", Filter(nchar, input$sortableList))
      ))
      
      filter_log("updating sortableJS list: ", 
                 paste(filters(), collapse = ", "),
                 verbose = verbose)
      
      # update filters downstream of change, isolate to prevent premature updates
      idxs <- which(cumsum(old_filters != filters()) > 0)
      
      isolate(for (idx in idxs) update_filter(filters()[[idx]]))
    }) 
    
    filter_logical <- reactiveVal(TRUE)
    observeEvent(datar(), {
      filter_logical(TRUE)
    })
    code <- reactive({
      req(datar())
      filter_log("building code", verbose = verbose)
      filter_exprs <- filter_returns[[utils::tail(filters(), 1)]]$filters()
      
      filter_logical(if (!length(filter_exprs)) rep(TRUE,nrow(datar())) else Reduce("&", Map(function(x) with(datar(), eval(x)), filter_exprs)))
      
      Reduce(
        function(l,r) bquote(.(l) %>% filter(.(r))), 
        filter_exprs,
        init = data_call)
    })
    
    bindEvent(
      reactive({
        filter_log("recalculating filtered data", verbose = verbose)
        structure(
          d <- subset(datar(), filter_logical()) %||% data.frame(),
          code = code(),
          class = c("shinyDataFilter_df", class(d)))
      }),
      code())
  })
}