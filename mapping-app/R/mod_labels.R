mod_labels_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("editor"))
}

mod_labels_server <- function(id, project, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    inputs <- function() editor_inputs(input, active_layer(), editor_revision(), c(
      "editor_id", "enabled", "column", "permanent", "size", "style", "direction", "opacity"
    ))

    output$editor <- shiny::renderUI({
      id <- active_layer()
      rev <- editor_revision()
      ns <- function(field) session$ns(editor_input_key(id, rev, field))
      layer <- shiny::isolate(project$layers[[id]])
      if (is.null(layer)) return(shiny::helpText("Select a layer to configure labels."))
      dataset <- shiny::isolate(project$datasets[[layer$dataset]])
      columns <- field_names(dataset)
      label <- utils::modifyList(list(enabled = FALSE, column = "", permanent = FALSE,
        size = 12, style = "normal", direction = "auto", opacity = 1),
        if (is.null(layer$label)) list() else layer$label)
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::checkboxInput(ns("enabled"), "Enable labels", value = isTRUE(label$enabled)),
        shiny::selectInput(ns("column"), "Label column",
          choices = c("(none)" = "", columns), selected = if (label$column %in% columns) label$column else ""),
        shiny::checkboxInput(ns("permanent"), "Keep labels visible", value = isTRUE(label$permanent)),
        shiny::numericInput(ns("size"), "Text size (px)", value = valid_number(label$size, 12, 1, 72), min = 1, max = 72),
        shiny::selectInput(ns("style"), "Text style",
          choices = c("Normal" = "normal", "Bold" = "bold", "Italic" = "italic", "Bold italic" = "bolditalic"),
          selected = if (label$style %in% c("normal", "bold", "italic", "bolditalic")) label$style else "normal"),
        shiny::selectInput(ns("direction"), "Direction",
          choices = c("Automatic" = "auto", "Top" = "top", "Bottom" = "bottom", "Left" = "left", "Right" = "right", "Center" = "center"),
          selected = if (label$direction %in% c("auto", "top", "bottom", "left", "right", "center")) label$direction else "auto"),
        shiny::sliderInput(ns("opacity"), "Label opacity", min = 0, max = 1,
          value = valid_number(label$opacity, 1, 0, 1), step = 0.05)
      )
    }) |> shiny::bindEvent(active_layer(), editor_revision())

    shiny::observeEvent(inputs(), {
      input <- inputs()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || any(vapply(list(input$enabled, input$column, input$permanent,
        input$style, input$direction, input$opacity), is.null, logical(1)))) return()
      columns <- field_names(project$datasets[[layer$dataset]])
      old <- utils::modifyList(list(enabled = FALSE, column = "", permanent = FALSE,
        size = 12, style = "normal", direction = "auto", opacity = 1),
        if (is.null(layer$label)) list() else layer$label)
      updated <- layer
      updated$label <- old
      updated$label$enabled <- isTRUE(input$enabled)
      updated$label$column <- if (input$column %in% columns) input$column else ""
      updated$label$permanent <- isTRUE(input$permanent)
      updated$label$size <- valid_number(input$size, old$size, 1, 72)
      updated$label$style <- if (input$style %in% c("normal", "bold", "italic", "bolditalic")) input$style else "normal"
      updated$label$direction <- if (input$direction %in% c("auto", "top", "bottom", "left", "right", "center")) input$direction else "auto"
      updated$label$opacity <- valid_number(input$opacity, old$opacity, 0, 1)
      set_project_layer(project, updated)
    }, ignoreInit = TRUE)
  })
}
