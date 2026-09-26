mod_basemap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("tiles"), "Basemap tiles",
      choices = c("OpenStreetMap", "CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery", "OpenTopoMap"), selected = "OpenStreetMap"),
    shiny::radioButtons(ns("extent"), "Initial extent",
      choices = c("Fit data" = "fit", "Use map center" = "center"), selected = "fit", inline = TRUE),
    shiny::conditionalPanel(sprintf("input['%s'] === 'center'", ns("extent")),
      shiny::numericInput(ns("center_lon"), "Center longitude", value = 0, min = -180, max = 180),
      shiny::numericInput(ns("center_lat"), "Center latitude", value = 0, min = -90, max = 90),
      shiny::numericInput(ns("zoom"), "Initial zoom", value = 4, min = 0, max = 22)
    ),
    shiny::numericInput(ns("min_zoom"), "Minimum zoom", value = 1, min = 0, max = 22),
    shiny::numericInput(ns("max_zoom"), "Maximum zoom", value = 19, min = 0, max = 22),
    shiny::checkboxInput(ns("zoom_control"), "Show zoom control", value = TRUE),
    shiny::textInput(ns("background"), "Map background colour", value = "#f4f4f4")
  )
}

mod_basemap_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    map_settings <- function() utils::modifyList(default_map(), if (is.null(project$map)) list() else project$map)

    shiny::observeEvent(TRUE, {
      settings <- map_settings()
      if (is.null(project$map)) project$map <- settings
      shiny::updateSelectInput(session, "tiles", selected = settings$tiles)
      shiny::updateRadioButtons(session, "extent", selected = settings$extent)
      shiny::updateNumericInput(session, "center_lon", value = valid_number(settings$center_lon, 0, -180, 180))
      shiny::updateNumericInput(session, "center_lat", value = valid_number(settings$center_lat, 0, -90, 90))
      shiny::updateNumericInput(session, "zoom", value = valid_number(settings$zoom, 4, 0, 22))
      shiny::updateNumericInput(session, "min_zoom", value = valid_number(settings$min_zoom, 1, 0, 22))
      shiny::updateNumericInput(session, "max_zoom", value = valid_number(settings$max_zoom, 19, 0, 22))
      shiny::updateCheckboxInput(session, "zoom_control", value = isTRUE(settings$zoom_control))
      shiny::updateTextInput(session, "background", value = settings$background)
    }, ignoreInit = FALSE, once = TRUE)

    shiny::observeEvent(list(input$tiles, input$extent, input$center_lon, input$center_lat,
      input$zoom, input$min_zoom, input$max_zoom, input$zoom_control, input$background), {
      if (any(vapply(list(input$tiles, input$extent, input$center_lon, input$center_lat,
        input$zoom, input$min_zoom, input$max_zoom, input$zoom_control, input$background), is.null, logical(1)))) return()
      old <- map_settings()
      updated <- old
      tile_choices <- c("OpenStreetMap", "CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery", "OpenTopoMap")
      updated$tiles <- if (input$tiles %in% tile_choices) input$tiles else old$tiles
      updated$extent <- if (input$extent %in% c("fit", "center")) input$extent else "fit"
      updated$center_lon <- valid_number(input$center_lon, old$center_lon, -180, 180)
      updated$center_lat <- valid_number(input$center_lat, old$center_lat, -90, 90)
      updated$zoom <- valid_number(input$zoom, old$zoom, 0, 22)
      updated$min_zoom <- valid_number(input$min_zoom, old$min_zoom, 0, 22)
      updated$max_zoom <- valid_number(input$max_zoom, old$max_zoom, 0, 22)
      if (updated$min_zoom > updated$max_zoom) {
        # Keep the permitted zoom range valid without silently changing min zoom.
        updated$max_zoom <- updated$min_zoom
      }
      updated$zoom <- valid_number(updated$zoom, old$zoom, updated$min_zoom, updated$max_zoom)
      updated$zoom_control <- isTRUE(input$zoom_control)
      updated$background <- if (is.null(input$background) || !nzchar(trimws(input$background))) old$background else trimws(input$background)
      if (!identical(project$map, updated)) project$map <- updated
    }, ignoreInit = TRUE)
  })
}
