mod_legends_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("editor"))
}

mod_legends_server <- function(id, project, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    inputs <- function() editor_inputs(input, active_layer(), editor_revision(), c(
      "editor_id", "enabled", "title", "position", "opacity", "show_na", "behaviour"
    ))

    output$editor <- shiny::renderUI({
      id <- active_layer()
      rev <- editor_revision()
      ns <- function(field) session$ns(editor_input_key(id, rev, field))
      layer <- shiny::isolate(project$layers[[id]])
      if (is.null(layer)) return(shiny::helpText("Select a layer to configure its legend."))
      legend <- utils::modifyList(list(enabled = TRUE, title = "", position = "bottomright",
        opacity = 1, show_na = TRUE, behaviour = "auto"),
        if (is.null(layer$legend)) list() else layer$legend)
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::checkboxInput(ns("enabled"), "Show legend", value = isTRUE(legend$enabled)),
        shiny::textInput(ns("title"), "Legend title", value = legend$title),
        shiny::selectInput(ns("position"), "Position",
          choices = c("Bottom right" = "bottomright", "Bottom left" = "bottomleft", "Top right" = "topright", "Top left" = "topleft"),
          selected = if (legend$position %in% c("bottomright", "bottomleft", "topright", "topleft")) legend$position else "bottomright"),
        shiny::sliderInput(ns("opacity"), "Legend opacity", min = 0, max = 1,
          value = valid_number(legend$opacity, 1, 0, 1), step = 0.05),
        shiny::checkboxInput(ns("show_na"), "Include missing-value entry", value = isTRUE(legend$show_na)),
        shiny::selectInput(ns("behaviour"), "Legend behaviour",
          choices = c("Automatic" = "auto", "Always show" = "always", "Hide when layer is hidden" = "layer"),
          selected = if (legend$behaviour %in% c("auto", "always", "layer")) legend$behaviour else "auto")
      )
    }) |> shiny::bindEvent(active_layer(), editor_revision())

    shiny::observeEvent(inputs(), {
      input <- inputs()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || any(vapply(list(input$enabled, input$position, input$opacity,
        input$show_na, input$behaviour), is.null, logical(1)))) return()
      old <- utils::modifyList(list(enabled = TRUE, title = "", position = "bottomright",
        opacity = 1, show_na = TRUE, behaviour = "auto"),
        if (is.null(layer$legend)) list() else layer$legend)
      updated <- layer
      updated$legend <- old
      updated$legend$enabled <- isTRUE(input$enabled)
      updated$legend$title <- if (is.null(input$title)) old$title else input$title
      updated$legend$position <- if (input$position %in% c("bottomright", "bottomleft", "topright", "topleft")) input$position else "bottomright"
      updated$legend$opacity <- valid_number(input$opacity, old$opacity, 0, 1)
      updated$legend$show_na <- isTRUE(input$show_na)
      updated$legend$behaviour <- if (input$behaviour %in% c("auto", "always", "layer")) input$behaviour else "auto"
      set_project_layer(project, updated)
    }, ignoreInit = TRUE)
  })
}
