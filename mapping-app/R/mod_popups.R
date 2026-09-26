mod_popups_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("editor"))
  )
}

mod_popups_server <- function(id, project, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    inputs <- function() editor_inputs(input, active_layer(), editor_revision(), c(
      "editor_id", "enabled", "mode", "title", "fields", "labels", "layout", "template"
    ))

    output$editor <- shiny::renderUI({
      id <- active_layer()
      rev <- editor_revision()
      ns <- function(field) session$ns(editor_input_key(id, rev, field))
      layer <- shiny::isolate(project$layers[[id]])
      if (is.null(layer)) return(shiny::helpText("Select a layer to configure its popup."))
      dataset <- shiny::isolate(project$datasets[[layer$dataset]])
      columns <- field_names(dataset)
      popup <- utils::modifyList(list(enabled = TRUE, mode = "html", title = "",
        fields = character(), labels = "", layout = "table", template = "<b>{id}</b>"),
        if (is.null(layer$popup)) list() else layer$popup)
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::checkboxInput(ns("enabled"), "Enable popups", value = isTRUE(popup$enabled)),
        shiny::radioButtons(ns("mode"), "Popup editor",
          choices = c("Simple builder" = "simple", "HTML template" = "html"),
          selected = if (popup$mode %in% c("simple", "html")) popup$mode else "html", inline = TRUE),
        shiny::conditionalPanel(
          sprintf("input['%s'] === 'simple'", ns("mode")),
          shiny::textInput(ns("title"), "Popup title", value = popup$title),
          shiny::selectInput(ns("fields"), "Fields", choices = columns,
            selected = intersect(popup$fields, columns), multiple = TRUE),
          shiny::textAreaInput(ns("labels"), "Field labels (one per line, optional)",
            value = popup$labels, rows = 3,
            placeholder = "Display labels are stored as text; for example: Name\nPopulation"),
          shiny::radioButtons(ns("layout"), "Layout",
            choices = c("Table" = "table", "Stacked" = "stacked", "Compact" = "compact"),
            selected = if (popup$layout %in% c("table", "stacked", "compact")) popup$layout else "table", inline = TRUE)
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] === 'html'", ns("mode")),
          shiny::textAreaInput(ns("template"), "HTML template", value = popup$template, rows = 5),
          shiny::helpText("Use {column} or {id} placeholders. Template markup is stored only; data values must be escaped by the renderer.")
        )
      )
    }) |> shiny::bindEvent(active_layer(), editor_revision())

    shiny::observeEvent(inputs(), {
      input <- inputs()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || any(vapply(list(input$enabled, input$mode, input$layout), is.null, logical(1)))) return()
      columns <- field_names(project$datasets[[layer$dataset]])
      old <- utils::modifyList(list(enabled = TRUE, mode = "html", title = "", fields = character(),
        labels = "", layout = "table", template = "<b>{id}</b>"), if (is.null(layer$popup)) list() else layer$popup)
      updated <- layer
      updated$popup <- old
      updated$popup$enabled <- isTRUE(input$enabled)
      updated$popup$mode <- if (input$mode %in% c("simple", "html")) input$mode else "html"
      updated$popup$title <- if (is.null(input$title)) old$title else input$title
      updated$popup$fields <- intersect(if (is.null(input$fields)) character() else input$fields, columns)
      updated$popup$labels <- if (is.null(input$labels)) old$labels else input$labels
      updated$popup$layout <- if (input$layout %in% c("table", "stacked", "compact")) input$layout else "table"
      updated$popup$template <- if (is.null(input$template)) old$template else input$template
      set_project_layer(project, updated)
    }, ignoreInit = TRUE)
  })
}
