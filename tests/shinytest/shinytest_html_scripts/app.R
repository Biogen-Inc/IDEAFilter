ui <- fluidPage(
  IDEAFilter:::css_shinyDataFilter_animation_script(),
  IDEAFilter:::css_shinyDataFilter_style_script(),
  IDEAFilter:::css_sortableJS_style_script(),
  IDEAFilter:::js_sortableJS_script())

srv <- function(input, output, session) { }

shinyApp(ui, srv)
