filter_type_choices <- function(values) {
  choices <- c("Categories" = "categorical")
  if (is.logical(values)) choices <- c(choices, "True or false" = "logical")
  if (!inherits(values, c("Date", "POSIXt"))) {
    numbers <- suppressWarnings(as.numeric(as.character(values)))
    if (any(is.finite(numbers))) choices <- c(choices, "Numeric range" = "numeric")
  }
  if (inherits(values, c("Date", "POSIXt")) || any(!is.na(as_filter_date(values)))) {
    choices <- c(choices, "Date range" = "date")
  }
  choices
}

filter_rule_label <- function(rule) {
  detail <- switch(rule$type,
    categorical = paste(rule$values, collapse = ", "),
    numeric = paste(rule$min, "to", rule$max),
    logical = as.character(rule$value),
    date = paste(rule$min, "to", rule$max), "")
  paste(rule$column, rule$type, detail,
    if (isTRUE(rule$include_na)) "(also missing)" else "")
}

mod_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::uiOutput(ns("editor")), shiny::tableOutput(ns("rules")),
    shiny::uiOutput(ns("value_ui")))
}

mod_filters_server <- function(id, project, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    deleting <- shiny::reactiveVal(NULL)
    fields <- c(
      "editor_id", "filter", "new_name", "create", "delete_filter", "column", "type",
      "include_na", "add_rule", "remove_index", "remove_rule", "categories", "range_min",
      "range_max", "logical_value", "dates"
    )
    form_version <- function() paste(editor_revision(), revision(), sep = "_")
    form_inputs <- function() editor_inputs(input, active_layer(), form_version(), fields)
    form_input <- function(field) input[[editor_input_key(active_layer(), form_version(), field)]]

    # form_inputs() reads every dynamic field. Ignore each re-render's initial
    # actionButton value of zero, and remember counts to prevent replay.
    handled_actions <- shiny::reactiveVal(list())
    new_action <- function(field, value) {
      if (!isTRUE(value > 0)) return(FALSE)
      key <- paste(active_layer(), form_version(), field, value, sep = "\r")
      handled <- handled_actions()
      if (isTRUE(handled[[key]])) return(FALSE)
      handled[[key]] <- TRUE
      handled_actions(handled)
      TRUE
    }

    output$editor <- shiny::renderUI({
      id <- active_layer()
      ver <- form_version()
      ns <- function(field) session$ns(editor_input_key(id, ver, field))
      layer <- shiny::isolate(project$layers[[id]])
      if (is.null(layer)) return(shiny::helpText("Select a layer before creating filters."))
      dataset <- shiny::isolate(project$datasets[[layer$dataset]])
      definitions <- shiny::isolate(project$filters)
      definitions <- definitions[vapply(definitions,
        function(x) identical(x$dataset, dataset$id), logical(1))]
      ids <- names(definitions)
      if (is.null(ids)) ids <- character()
      filters <- stats::setNames(ids,
        vapply(definitions, function(x) x$name, character(1)))
      current <- definitions[[layer$filter]]
      rules <- if (is.null(current)) list() else current$rules
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::selectInput(ns("filter"), "Filter set for this layer",
          choices = c("(no filter)" = "", filters),
          selected = if (layer$filter %in% names(definitions)) layer$filter else ""),
        shiny::textInput(ns("new_name"), "New reusable filter name"),
        shiny::actionButton(ns("create"), "Create filter"),
        shiny::actionButton(ns("delete_filter"), "Delete selected filter", class = "btn-outline-danger"),
        shiny::hr(),
        shiny::helpText("Rules in a filter set are combined with AND. Filter sets can be reused by layers from the same dataset."),
        shiny::selectInput(ns("column"), "Column to filter", choices = field_names(dataset)),
        shiny::selectInput(ns("type"), "Rule type", choices = character()),
        shiny::checkboxInput(ns("include_na"), "Also include missing values", value = FALSE),
        shiny::actionButton(ns("add_rule"), "Add rule"),
        shiny::selectInput(ns("remove_index"), "Existing rule",
          choices = stats::setNames(as.character(seq_along(rules)), vapply(rules, filter_rule_label, character(1)))),
        shiny::actionButton(ns("remove_rule"), "Remove selected rule")
      )
    }) |> shiny::bindEvent(active_layer(), editor_revision(), revision())

    shiny::observeEvent(form_input("filter"), {
      input <- form_inputs()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || is.null(input$filter)) return()
      definition <- project$filters[[input$filter]]
      if (nzchar(input$filter) && (is.null(definition) || definition$dataset != layer$dataset)) return()
      layer$filter <- input$filter
      set_project_layer(project, layer)
    }, ignoreInit = TRUE)

    shiny::observeEvent(form_input("column"), {
      input <- form_inputs()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer)) return()
      dataset <- project$datasets[[layer$dataset]]
      if (is.null(dataset) || !input$column %in% field_names(dataset)) return()
      choices <- filter_type_choices(dataset$data[[input$column]])
      selected <- if (!is.null(input$type) && input$type %in% choices) input$type else unname(choices[[1]])
      shiny::updateSelectInput(session,
        editor_input_key(active_layer(), form_version(), "type"),
        choices = choices, selected = selected)
    })

    output$value_ui <- shiny::renderUI({
      id <- active_layer()
      ver <- form_version()
      ns <- function(field) session$ns(editor_input_key(id, ver, field))
      input <- editor_inputs(input, id, ver, c("editor_id", "column", "type"))
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || is.null(input$column) || is.null(input$type)) return(NULL)
      dataset <- project$datasets[[layer$dataset]]
      if (is.null(dataset) || !input$column %in% field_names(dataset)) return(NULL)
      values <- dataset$data[[input$column]]
      if (!input$type %in% filter_type_choices(values)) return(NULL)
      switch(input$type,
        categorical = shiny::selectizeInput(ns("categories"), "Include categories",
          choices = sort(unique(as.character(values[!is.na(values)]))), multiple = TRUE),
        numeric = {
          numbers <- suppressWarnings(as.numeric(as.character(values)))
          numbers <- numbers[is.finite(numbers)]
          shiny::tagList(
            shiny::numericInput(ns("range_min"), "Minimum", value = min(numbers)),
            shiny::numericInput(ns("range_max"), "Maximum", value = max(numbers)))
        },
        logical = shiny::selectInput(ns("logical_value"), "Value",
          choices = c("True" = "TRUE", "False" = "FALSE")),
        date = {
          dates <- as_filter_date(values)
          dates <- dates[!is.na(dates)]
          shiny::dateRangeInput(ns("dates"), "Date range", start = min(dates), end = max(dates),
            min = min(dates), max = max(dates))
        }
      )
    })

    output$rules <- shiny::renderTable({
      layer <- project$layers[[active_layer()]]
      if (is.null(layer) || is.null(project$filters[[layer$filter]])) return(NULL)
      rules <- project$filters[[layer$filter]]$rules
      if (!length(rules)) return(NULL)
      data.frame(Rule = seq_along(rules), Criterion = vapply(rules, filter_rule_label, character(1)))
    }, striped = TRUE, spacing = "s")

    shiny::observeEvent(form_inputs()$create, {
      input <- form_inputs()
      if (!new_action("create", input$create)) return()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer)) return()
      name <- if (is.null(input$new_name) || !nzchar(trimws(input$new_name))) {
        paste(layer$name, "filter")
      } else trimws(input$new_name)
      definition <- list(id = project_id(name, names(project$filters), "filter"),
        name = name, dataset = layer$dataset, rules = list())
      filters <- project$filters
      filters[[definition$id]] <- definition
      project$filters <- filters
      layer$filter <- definition$id
      set_project_layer(project, layer)
      revision(revision() + 1L)
    })

    shiny::observeEvent(form_inputs()$add_rule, {
      input <- form_inputs()
      if (!new_action("add_rule", input$add_rule)) return()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || is.null(input$column) || is.null(input$type)) return()
      dataset <- project$datasets[[layer$dataset]]
      definition <- project$filters[[layer$filter]]
      if (is.null(dataset) || is.null(definition) || !input$column %in% field_names(dataset) ||
          !input$type %in% filter_type_choices(dataset$data[[input$column]])) {
        shiny::showNotification("Create/select a filter and valid column first.", type = "warning")
        return()
      }
      rule <- list(column = input$column, type = input$type, include_na = isTRUE(input$include_na))
      if (input$type == "categorical") {
        rule$values <- if (is.null(input$categories)) character() else as.character(input$categories)
        if (!length(rule$values) && !rule$include_na) {
          shiny::showNotification("Select categories or include missing values.", type = "warning")
          return()
        }
      } else if (input$type == "numeric") {
        rule$min <- valid_number(input$range_min, NA_real_)
        rule$max <- valid_number(input$range_max, NA_real_)
        if (!is.finite(rule$min) || !is.finite(rule$max) || rule$min > rule$max) {
          shiny::showNotification("Enter a valid numeric range.", type = "warning")
          return()
        }
      } else if (input$type == "logical") {
        if (is.null(input$logical_value) || !input$logical_value %in% c("TRUE", "FALSE")) return()
        rule$value <- input$logical_value == "TRUE"
      } else {
        dates <- input$dates
        if (length(dates) != 2L || anyNA(dates) || dates[[1]] > dates[[2]]) {
          shiny::showNotification("Select a valid date range.", type = "warning")
          return()
        }
        rule$min <- as.character(as.Date(dates[[1]]))
        rule$max <- as.character(as.Date(dates[[2]]))
      }
      filters <- project$filters
      definition$rules[[length(definition$rules) + 1L]] <- rule
      filters[[definition$id]] <- definition
      project$filters <- filters
      revision(revision() + 1L)
    })

    shiny::observeEvent(form_inputs()$remove_rule, {
      input <- form_inputs()
      if (!new_action("remove_rule", input$remove_rule)) return()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer)) return()
      filters <- project$filters
      definition <- filters[[layer$filter]]
      index <- suppressWarnings(as.integer(input$remove_index))
      if (is.null(definition) || length(index) != 1L || is.na(index) || index < 1L || index > length(definition$rules)) return()
      definition$rules[[index]] <- NULL
      filters[[definition$id]] <- definition
      project$filters <- filters
      revision(revision() + 1L)
    })

    shiny::observeEvent(form_inputs()$delete_filter, {
      input <- form_inputs()
      if (!new_action("delete_filter", input$delete_filter)) return()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || !nzchar(layer$filter)) return()
      deleting(layer$filter)
      shiny::showModal(shiny::modalDialog("Delete this shared filter from every layer that uses it?",
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete"), "Delete", class = "btn-danger"))))
    })
    shiny::observeEvent(input$confirm_delete, {
      id <- deleting()
      shiny::removeModal()
      if (is.null(id) || !id %in% names(project$filters)) return()
      filters <- project$filters
      filters[[id]] <- NULL
      project$filters <- filters
      layers <- lapply(project$layers, function(layer) {
        if (identical(layer$filter, id)) layer$filter <- ""
        layer
      })
      project$layers <- layers
      deleting(NULL)
      revision(revision() + 1L)
    })
  })
}
