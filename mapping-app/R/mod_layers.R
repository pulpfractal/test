layer_type_choices <- function(dataset) {
  all <- c("Circle markers" = "circleMarker", "Standard markers" = "marker",
    "Polygons" = "polygon", "Polylines" = "polyline")
  all[all %in% layer_types(dataset)]
}

mod_layers_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$strong("Create a layer"),
    shiny::selectInput(ns("new_source"), "Source dataset", choices = character()),
    shiny::selectInput(ns("new_type"), "Layer type", choices = character()),
    shiny::textInput(ns("new_name"), "Layer name (optional)"),
    shiny::actionButton(ns("add"), "Add layer", class = "btn-primary"),
    shiny::hr(),
    shiny::tags$strong("Layer order (bottom to top)"),
    shiny::tableOutput(ns("layer_list")),
    shiny::uiOutput(ns("editor"))
  )
}

mod_layers_server <- function(id, project, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    deleting <- shiny::reactiveVal(NULL)

    shiny::observeEvent(project$datasets, {
      datasets <- project$datasets
      ids <- names(datasets)
      if (is.null(ids)) ids <- character()
      choices <- stats::setNames(ids,
        vapply(datasets, function(dataset) dataset$name, character(1)))
      selected <- input$new_source
      if (is.null(selected) || !selected %in% names(datasets)) {
        selected <- if (length(datasets)) names(datasets)[[1]] else ""
      }
      shiny::updateSelectInput(session, "new_source", choices = choices, selected = selected)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$new_source, {
      choices <- layer_type_choices(project$datasets[[input$new_source]])
      selected <- if (!is.null(input$new_type) && input$new_type %in% choices) input$new_type else if (length(choices)) unname(choices[[1]]) else ""
      shiny::updateSelectInput(session, "new_type", choices = choices, selected = selected)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$add, {
      dataset <- project$datasets[[input$new_source]]
      if (is.null(dataset) || !input$new_type %in% layer_types(dataset)) {
        shiny::showNotification("Choose a dataset and compatible layer type.", type = "warning")
        return()
      }
      name <- if (is.null(input$new_name) || !nzchar(trimws(input$new_name))) dataset$name else trimws(input$new_name)
      layer <- new_layer(dataset, existing = names(project$layers), name = name, type = input$new_type)
      layers <- project$layers
      layers[[layer$id]] <- layer
      project$layers <- layers
      active_layer(layer$id)
      editor_revision(editor_revision() + 1L)
      shiny::updateTextInput(session, "new_name", value = "")
    })

    output$layer_list <- shiny::renderTable({
      layers <- project$layers
      if (!length(layers)) return(data.frame())
      data.frame(
        Order = seq_along(layers),
        Layer = vapply(layers, function(x) x$name, character(1)),
        Dataset = vapply(layers, function(x) {
          dataset <- project$datasets[[x$dataset]]
          if (is.null(dataset)) "(removed)" else dataset$name
        }, character(1)),
        Type = vapply(layers, function(x) x$type, character(1)),
        Shown = vapply(layers, function(x) if (isTRUE(x$visible)) "Yes" else "No", character(1)),
        check.names = FALSE
      )
    }, striped = TRUE, spacing = "s")

    editor_fields <- function() {
      editor_inputs(input, active_layer(), editor_revision(),
        c("editor_id", "name", "source", "type", "visible", "group",
          "duplicate", "up", "down", "delete"))
    }
    editor_field <- function(field) {
      input[[editor_input_key(active_layer(), editor_revision(), field)]]
    }

    output$editor <- shiny::renderUI({
      id <- active_layer()
      rev <- editor_revision()
      ns <- function(field) session$ns(editor_input_key(id, rev, field))
      layer <- shiny::isolate(project$layers[[id]])
      if (is.null(layer)) return(shiny::helpText("Select or add a layer to edit it."))
      datasets <- shiny::isolate(project$datasets)
      ids <- names(datasets)
      if (is.null(ids)) ids <- character()
      choices <- stats::setNames(ids,
        vapply(datasets, function(x) x$name, character(1)))
      shiny::tagList(
        shiny::hr(),
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::textInput(ns("name"), "Active layer name", value = layer$name),
        shiny::selectInput(ns("source"), "Dataset", choices = choices, selected = layer$dataset),
        shiny::selectInput(ns("type"), "Type", choices = layer_type_choices(datasets[[layer$dataset]]), selected = layer$type),
        shiny::checkboxInput(ns("visible"), "Visible in preview", value = isTRUE(layer$visible)),
        shiny::textInput(ns("group"), "Layer group", value = layer$group),
        shiny::helpText("Groups organize the layer control. Each layer remains independently toggleable."),
        shiny::actionButton(ns("duplicate"), "Duplicate"),
        shiny::actionButton(ns("up"), "Move up"),
        shiny::actionButton(ns("down"), "Move down"),
        shiny::actionButton(ns("delete"), "Delete", class = "btn-outline-danger")
      )
    }) |> shiny::bindEvent(active_layer(), editor_revision())

    shiny::observeEvent(editor_field("source"), {
      input <- editor_fields()
      id <- active_layer()
      rev <- editor_revision()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || is.null(input$source)) return()
      choices <- layer_type_choices(project$datasets[[input$source]])
      chosen <- if (!is.null(input$type) && input$type %in% choices) input$type else if (length(choices)) unname(choices[[1]]) else ""
      shiny::updateSelectInput(session, editor_input_key(id, rev, "type"),
        choices = choices, selected = chosen)
    })

    shiny::observeEvent(list(editor_field("editor_id"), editor_field("name"),
                             editor_field("source"), editor_field("type"),
                             editor_field("visible"), editor_field("group")), {
      input <- editor_fields()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || any(vapply(list(input$name, input$source,
        input$type, input$visible, input$group), is.null, logical(1)))) return()
      dataset <- project$datasets[[input$source]]
      if (is.null(dataset)) return()
      updated <- layer
      updated$name <- if (nzchar(trimws(input$name))) trimws(input$name) else layer$name
      updated$dataset <- dataset$id
      updated$type <- if (input$type %in% layer_types(dataset)) input$type else layer_types(dataset)[[1]]
      updated$visible <- isTRUE(input$visible)
      updated$group <- if (nzchar(trimws(input$group))) trimws(input$group) else updated$name
      if (!identical(updated$dataset, layer$dataset)) updated$filter <- ""
      changed_source <- !identical(updated$dataset, layer$dataset) || !identical(updated$type, layer$type)
      set_project_layer(project, updated)
      if (changed_source) editor_revision(editor_revision() + 1L)
    }, ignoreInit = TRUE)

    shiny::observeEvent(editor_field("duplicate"), {
      input <- editor_fields()
      if (is.null(input$duplicate) || input$duplicate < 1L) return()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer)) return()
      copy <- layer
      copy$name <- paste(layer$name, "copy")
      copy$id <- project_id(copy$name, names(project$layers), "layer")
      layers <- project$layers
      project$layers <- append(layers, stats::setNames(list(copy), copy$id),
        after = match(layer$id, names(layers)))
      active_layer(copy$id)
      editor_revision(editor_revision() + 1L)
    })

    move_layer <- function(direction) {
      ids <- names(project$layers)
      pos <- match(active_layer(), ids)
      if (is.na(pos) || pos + direction < 1L || pos + direction > length(ids)) return()
      ids[c(pos, pos + direction)] <- ids[c(pos + direction, pos)]
      project$layers <- project$layers[ids]
    }
    shiny::observeEvent(editor_field("up"), {
      input <- editor_fields()
      if (is.null(input$up) || input$up < 1L) return()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer)) return()
      move_layer(1L)
    })
    shiny::observeEvent(editor_field("down"), {
      input <- editor_fields()
      if (is.null(input$down) || input$down < 1L) return()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer)) return()
      move_layer(-1L)
    })

    shiny::observeEvent(editor_field("delete"), {
      input <- editor_fields()
      if (is.null(input$delete) || input$delete < 1L) return()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer)) return()
      deleting(layer$id)
      shiny::showModal(shiny::modalDialog(paste("Delete the layer", layer$name, "?"),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete"), "Delete", class = "btn-danger"))))
    })

    shiny::observeEvent(input$confirm_delete, {
      id <- deleting()
      shiny::removeModal()
      if (is.null(id) || !id %in% names(project$layers)) return()
      layers <- project$layers
      layers[[id]] <- NULL
      project$layers <- layers
      deleting(NULL)
      active_layer(if (length(layers)) names(layers)[[1]] else "")
      editor_revision(editor_revision() + 1L)
    })
  })
}
