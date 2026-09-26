mod_widgets_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::checkboxInput(ns("layer_control"), "Layer control", value = TRUE),
    shiny::checkboxInput(ns("search"), "Search control", value = FALSE),
    shiny::checkboxInput(ns("scale_bar"), "Scale bar", value = FALSE),
    shiny::checkboxInput(ns("fullscreen"), "Fullscreen button", value = FALSE),
    shiny::checkboxInput(ns("reset_view"), "Reset view button", value = FALSE),
    shiny::checkboxInput(ns("mouse_coords"), "Mouse coordinates", value = FALSE),
    shiny::checkboxInput(ns("minimap"), "Mini map", value = FALSE),
    shiny::checkboxInput(ns("measure"), "Measurement tools", value = FALSE),
    shiny::helpText(if (requireNamespace("leaflet.extras", quietly = TRUE)) {
      "Search and fullscreen use leaflet.extras; the other controls use base leaflet."
    } else {
      "Search and fullscreen need the optional leaflet.extras package; the other controls work without it."
    })
  )
}

mod_widgets_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    widget_settings <- function() utils::modifyList(default_widgets(), if (is.null(project$widgets)) list() else project$widgets)
    widget_names <- names(default_widgets())

    shiny::observeEvent(TRUE, {
      settings <- widget_settings()
      if (is.null(project$widgets)) project$widgets <- settings
      for (name in widget_names) shiny::updateCheckboxInput(session, name, value = isTRUE(settings[[name]]))
    }, ignoreInit = FALSE, once = TRUE)

    shiny::observeEvent(list(input$layer_control, input$search, input$scale_bar,
      input$fullscreen, input$reset_view, input$mouse_coords, input$minimap, input$measure), {
      if (any(vapply(lapply(widget_names, function(name) input[[name]]), is.null, logical(1)))) return()
      updated <- widget_settings()
      for (name in widget_names) updated[[name]] <- isTRUE(input[[name]])
      if (!identical(project$widgets, updated)) project$widgets <- updated
    }, ignoreInit = TRUE)
  })
}
