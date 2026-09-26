mod_polygons_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "editor"))
}

mod_polygons_server <- function(id, project, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    inputs <- function() editor_inputs(input, active_layer(), editor_revision(), c(
      "editor_id", "fill_opacity", "outline", "width", "opacity", "dash",
      "highlight", "hover_weight", "hover_opacity", "bring_to_front", "send_to_back"
    ))

    output$editor <- shiny::renderUI({
      id <- active_layer()
      rev <- editor_revision()
      ns <- function(field) session$ns(editor_input_key(id, rev, field))
      layer <- shiny::isolate(project$layers[[id]])
      if (is.null(layer) || !layer$type %in% c("polygon", "polyline")) {
        return(shiny::helpText("Select a polygon or polyline layer to edit its geometry styling."))
      }
      polygon <- layer$polygon
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::helpText("Set single, category, numeric, bins, or quantile fill in the Colours tab. Those colours are applied independently to this layer."),
        shiny::sliderInput(ns("fill_opacity"), "Fill opacity (polygons)", min = 0, max = 1,
          value = polygon$fill_opacity, step = 0.05),
        shiny::textInput(ns("outline"), "Outline colour (polygons)", value = polygon$outline),
        shiny::numericInput(ns("width"), "Outline/line width (px)", value = polygon$width, min = 0, max = 20),
        shiny::sliderInput(ns("opacity"), "Outline/line opacity", min = 0, max = 1,
          value = polygon$opacity, step = 0.05),
        shiny::textInput(ns("dash"), "Dash array (e.g. 5, 5; blank = solid)", value = polygon$dash),
        shiny::checkboxInput(ns("highlight"), "Highlight on hover", value = isTRUE(polygon$highlight)),
        shiny::numericInput(ns("hover_weight"), "Hover outline width", value = polygon$hover_weight, min = 0, max = 20),
        shiny::sliderInput(ns("hover_opacity"), "Hover opacity", min = 0, max = 1,
          value = polygon$hover_opacity, step = 0.05),
        shiny::checkboxInput(ns("bring_to_front"), "Bring highlighted shape to front", value = isTRUE(polygon$bring_to_front)),
        shiny::checkboxInput(ns("send_to_back"), "Send highlighted shape to back", value = isTRUE(polygon$send_to_back))
      )
    }) |> shiny::bindEvent(active_layer(), editor_revision())

    shiny::observeEvent(inputs(), {
      input <- inputs()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || !layer$type %in% c("polygon", "polyline")) return()
      if (any(vapply(list(input$fill_opacity, input$outline, input$opacity, input$dash,
        input$highlight, input$hover_opacity, input$bring_to_front, input$send_to_back),
        is.null, logical(1)))) return()
      updated <- layer
      polygon <- layer$polygon
      polygon$fill_opacity <- valid_number(input$fill_opacity, polygon$fill_opacity, 0, 1)
      polygon$outline <- valid_colour(input$outline, polygon$outline)
      polygon$width <- valid_number(input$width, polygon$width, 0, 20)
      polygon$opacity <- valid_number(input$opacity, polygon$opacity, 0, 1)
      polygon$dash <- trimws(input$dash)
      polygon$highlight <- isTRUE(input$highlight)
      polygon$hover_weight <- valid_number(input$hover_weight, polygon$hover_weight, 0, 20)
      polygon$hover_opacity <- valid_number(input$hover_opacity, polygon$hover_opacity, 0, 1)
      polygon$bring_to_front <- isTRUE(input$bring_to_front)
      polygon$send_to_back <- isTRUE(input$send_to_back)
      if (polygon$send_to_back) polygon$bring_to_front <- FALSE
      updated$polygon <- polygon
      set_project_layer(project, updated)
    }, ignoreInit = TRUE)
  })
}
