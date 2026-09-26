mod_markers_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "editor"))
}

mod_markers_server <- function(id, project, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    inputs <- function() editor_inputs(input, active_layer(), editor_revision(), c(
      "editor_id", "type", "radius", "stroke", "stroke_color", "stroke_width",
      "stroke_opacity", "fill_opacity", "cluster", "cluster_radius", "spiderfy",
      "min_zoom", "max_zoom"
    ))

    output$editor <- shiny::renderUI({
      id <- active_layer()
      rev <- editor_revision()
      ns <- function(field) session$ns(editor_input_key(id, rev, field))
      layer <- shiny::isolate(project$layers[[id]])
      if (is.null(layer) || !layer$type %in% c("circleMarker", "marker")) {
        return(shiny::helpText("Select a point layer to edit markers."))
      }
      marker <- layer$marker
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::radioButtons(ns("type"), "Marker type",
          choices = c("Circle" = "circleMarker", "Standard pin" = "marker"), selected = layer$type, inline = TRUE),
        shiny::numericInput(ns("radius"), "Circle radius (px)", value = marker$radius, min = 1, max = 100),
        shiny::checkboxInput(ns("stroke"), "Draw marker outline", value = isTRUE(marker$stroke)),
        shiny::textInput(ns("stroke_color"), "Outline colour (blank = fill colour)", value = marker$stroke_color),
        shiny::numericInput(ns("stroke_width"), "Outline width (px)", value = marker$stroke_width, min = 0, max = 20),
        shiny::sliderInput(ns("stroke_opacity"), "Outline opacity", min = 0, max = 1,
          value = marker$stroke_opacity, step = 0.05),
        shiny::sliderInput(ns("fill_opacity"), "Fill opacity", min = 0, max = 1,
          value = marker$fill_opacity, step = 0.05),
        shiny::checkboxInput(ns("cluster"), "Cluster overlapping points", value = isTRUE(marker$cluster)),
        shiny::numericInput(ns("cluster_radius"), "Maximum cluster radius (px)",
          value = marker$cluster_radius, min = 10, max = 300),
        shiny::checkboxInput(ns("spiderfy"), "Spiderfy clusters at maximum zoom", value = isTRUE(marker$spiderfy)),
        shiny::numericInput(ns("min_zoom"), "Minimum zoom for this layer", value = marker$min_zoom, min = 0, max = 22),
        shiny::numericInput(ns("max_zoom"), "Maximum zoom for this layer", value = marker$max_zoom, min = 0, max = 22),
        shiny::helpText("Circle-only options are ignored by standard pins. Zoom limits also apply to layer-control toggles.")
      )
    }) |> shiny::bindEvent(active_layer(), editor_revision())

    shiny::observeEvent(inputs(), {
      input <- inputs()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || !layer$type %in% c("circleMarker", "marker")) return()
      required <- list(input$type, input$stroke, input$stroke_color, input$stroke_opacity,
        input$fill_opacity, input$cluster, input$spiderfy)
      if (any(vapply(required, is.null, logical(1)))) return()
      marker <- layer$marker
      updated <- layer
      updated$type <- if (input$type %in% c("circleMarker", "marker")) input$type else layer$type
      marker$radius <- valid_number(input$radius, marker$radius, 1, 100)
      marker$stroke <- isTRUE(input$stroke)
      marker$stroke_color <- trimws(input$stroke_color)
      marker$stroke_width <- valid_number(input$stroke_width, marker$stroke_width, 0, 20)
      marker$stroke_opacity <- valid_number(input$stroke_opacity, marker$stroke_opacity, 0, 1)
      marker$fill_opacity <- valid_number(input$fill_opacity, marker$fill_opacity, 0, 1)
      marker$cluster <- isTRUE(input$cluster)
      marker$cluster_radius <- valid_number(input$cluster_radius, marker$cluster_radius, 10, 300)
      marker$spiderfy <- isTRUE(input$spiderfy)
      marker$min_zoom <- valid_number(input$min_zoom, marker$min_zoom, 0, 22)
      marker$max_zoom <- valid_number(input$max_zoom, marker$max_zoom, 0, 22)
      if (marker$max_zoom < marker$min_zoom) marker$max_zoom <- marker$min_zoom
      updated$marker <- marker
      set_project_layer(project, updated)
      if (!identical(updated$type, layer$type)) editor_revision(editor_revision() + 1L)
    }, ignoreInit = TRUE)
  })
}
