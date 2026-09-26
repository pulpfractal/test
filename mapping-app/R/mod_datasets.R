mod_datasets_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("files"), "Add CSV, GeoJSON or GeoPackage datasets",
      multiple = TRUE, accept = c(".csv", ".geojson", ".json", ".gpkg")),
    shiny::textInput(ns("upload_name"), "Friendly name (single file; optional)"),
    shiny::selectInput(ns("dataset"), "Uploaded datasets", choices = character()),
    shiny::uiOutput(ns("details")),
    shiny::tableOutput(ns("preview")),
    shiny::actionButton(ns("remove"), "Remove selected dataset", class = "btn-outline-danger")
  )
}

mod_datasets_server <- function(id, project, active_dataset, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    removing <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$files, {
      uploads <- input$files
      for (i in seq_len(nrow(uploads))) {
        path <- uploads$datapath[[i]]
        filename <- uploads$name[[i]]
        name <- if (nrow(uploads) == 1L && nzchar(trimws(input$upload_name))) {
          trimws(input$upload_name)
        } else tools::file_path_sans_ext(basename(filename))
        tryCatch({
          extension <- tolower(tools::file_ext(filename))
          data <- if (extension == "csv") {
            utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
          } else if (extension %in% c("geojson", "json", "gpkg")) {
            suppressWarnings(sf::st_read(path, quiet = TRUE))
          } else stop("Unsupported file type: ", filename, call. = FALSE)
          added <- add_project_dataset(project, data, name)
          active_dataset(added$dataset)
          active_layer(added$layer)
          revision(revision() + 1L)
          editor_revision(editor_revision() + 1L)
        }, error = function(e) {
          shiny::showNotification(paste("Could not load", filename, ":", conditionMessage(e)), type = "error")
        })
      }
    })

    shiny::observeEvent(project$datasets, {
      ids <- names(project$datasets)
      if (is.null(ids)) ids <- character()
      choices <- stats::setNames(ids,
        vapply(project$datasets, function(x) x$name, character(1)))
      selected <- active_dataset()
      if (!selected %in% names(project$datasets)) {
        selected <- if (length(choices)) unname(choices[[1]]) else ""
        active_dataset(selected)
      }
      shiny::updateSelectInput(session, "dataset", choices = choices, selected = selected)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$dataset, {
      if (input$dataset %in% names(project$datasets) && !identical(input$dataset, active_dataset())) {
        active_dataset(input$dataset)
      }
    })

    metadata_inputs <- function() {
      editor_inputs(input, active_dataset(), revision(),
        c("editor_id", "friendly", "id_col", "lat_col", "lon_col"))
    }

    output$details <- shiny::renderUI({
      id <- active_dataset()
      rev <- revision()
      ns <- function(field) session$ns(editor_input_key(id, rev, field))
      dataset <- shiny::isolate(project$datasets[[id]])
      if (is.null(dataset)) return(shiny::helpText("Upload a dataset to begin. Each upload also creates an editable default layer."))
      cols <- field_names(dataset)
      point_columns <- if (dataset$type == "point" && !inherits(dataset$data, "sf")) {
        shiny::tagList(
          shiny::selectInput(ns("lat_col"), "Latitude (WGS84)",
            choices = c("(not set)" = "", cols), selected = dataset$lat),
          shiny::selectInput(ns("lon_col"), "Longitude (WGS84)",
            choices = c("(not set)" = "", cols), selected = dataset$lon)
        )
      } else shiny::helpText("Spatial geometry is used for this dataset; its CRS is transformed to WGS84 on the map.")
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::textInput(ns("friendly"), "Dataset name", value = dataset$name),
        shiny::p("Type: ", dataset$type, if (inherits(dataset$data, "sf")) " (sf)" else " (table)",
          "; rows: ", nrow(dataset$data), "; internal ID: ", dataset$id),
        shiny::selectInput(ns("id_col"), "Record ID column", choices = c("(none)" = "", cols),
          selected = dataset$id_col),
        point_columns,
        shiny::tags$strong("First six rows")
      )
    }) |> shiny::bindEvent(active_dataset(), revision())

    shiny::observeEvent(metadata_inputs(), {
      input <- metadata_inputs()
      id <- input$editor_id
      if (is.null(id) || !identical(id, active_dataset())) return()
      dataset <- project$datasets[[id]]
      if (is.null(dataset) || is.null(input$friendly) || is.null(input$id_col)) return()
      updated <- dataset
      updated$name <- if (nzchar(trimws(input$friendly))) trimws(input$friendly) else dataset$name
      updated$id_col <- if (input$id_col %in% field_names(dataset)) input$id_col else ""
      if (dataset$type == "point" && !inherits(dataset$data, "sf")) {
        if (is.null(input$lat_col) || is.null(input$lon_col)) return()
        updated$lat <- if (input$lat_col %in% names(dataset$data)) input$lat_col else ""
        updated$lon <- if (input$lon_col %in% names(dataset$data)) input$lon_col else ""
      }
      if (!identical(updated, dataset)) {
        datasets <- project$datasets
        datasets[[id]] <- updated
        project$datasets <- datasets
      }
    }, ignoreInit = TRUE)

    output$preview <- shiny::renderTable({
      dataset <- project$datasets[[active_dataset()]]
      shiny::req(dataset)
      data <- if (inherits(dataset$data, "sf")) sf::st_drop_geometry(dataset$data) else dataset$data
      utils::head(data, 6)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    shiny::observeEvent(input$remove, {
      id <- active_dataset()
      if (!id %in% names(project$datasets)) return()
      removing(id)
      shiny::showModal(shiny::modalDialog(
        paste("Remove", project$datasets[[id]]$name, "and all its layers and filters?"),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_remove"), "Remove", class = "btn-danger"))
      ))
    })

    shiny::observeEvent(input$confirm_remove, {
      id <- removing()
      shiny::removeModal()
      if (is.null(id) || !id %in% names(project$datasets)) return()
      remove_project_dataset(project, id)
      removing(NULL)
      active_dataset("")
      revision(revision() + 1L)
      editor_revision(editor_revision() + 1L)
    })

    list(revision = shiny::reactive(revision()),
      touch = function() revision(revision() + 1L))
  })
}
