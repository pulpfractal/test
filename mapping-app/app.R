options(shiny.maxRequestSize = 100 * 1024^2)

for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file, local = FALSE)
}

ui <- bslib::page_sidebar(
  title = "Leaflet Map Studio",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = bslib::sidebar(
    width = 490,
    open = "always",
    shiny::selectInput("active_layer", "Active Layer", choices = character()),
    bslib::navset_tab(
      bslib::nav_panel("Data", mod_datasets_ui("datasets"),
        bslib::accordion(bslib::accordion_panel("Azure Maps geocoding", mod_geocode_ui("geocode")))),
      bslib::nav_panel("Layers", mod_layers_ui("layers")),
      bslib::nav_panel("Markers", mod_markers_ui("markers")),
      bslib::nav_panel("Colours", mod_colours_ui("colours")),
      bslib::nav_panel("Polygons", mod_polygons_ui("polygons")),
      bslib::nav_panel("Popups", mod_popups_ui("popups")),
      bslib::nav_panel("Labels", mod_labels_ui("labels")),
      bslib::nav_panel("Legends", mod_legends_ui("legends")),
      bslib::nav_panel("Filters", mod_filters_ui("filters")),
      bslib::nav_panel("Widgets", mod_widgets_ui("widgets")),
      bslib::nav_panel("Map", mod_basemap_ui("basemap")),
      bslib::nav_panel("Export", mod_export_ui("export"))
    )
  ),
  bslib::card(
    bslib::card_header("Map preview"),
    leaflet::leafletOutput("map", height = "78vh"),
    full_screen = TRUE
  )
)

server <- function(input, output, session) {
  project <- shiny::reactiveValues(
    datasets = list(), layers = list(), map = default_map(),
    widgets = default_widgets(), filters = list()
  )
  active_dataset <- shiny::reactiveVal("")
  active_layer <- shiny::reactiveVal("")
  editor_revision <- shiny::reactiveVal(0L)

  datasets <- mod_datasets_server("datasets", project, active_dataset, active_layer, editor_revision)
  geocode <- mod_geocode_server("geocode", project, active_dataset,
    datasets$touch, editor_revision)
  mod_layers_server("layers", project, active_layer, editor_revision)
  mod_markers_server("markers", project, active_layer, editor_revision)
  mod_colours_server("colours", project, active_layer, editor_revision)
  mod_polygons_server("polygons", project, active_layer, editor_revision)
  mod_popups_server("popups", project, active_layer, editor_revision)
  mod_labels_server("labels", project, active_layer, editor_revision)
  mod_legends_server("legends", project, active_layer, editor_revision)
  mod_filters_server("filters", project, active_layer, editor_revision)
  mod_widgets_server("widgets", project)
  mod_basemap_server("basemap", project)
  mod_export_server("export", project, azure_key = geocode$key, helpers_dir = "R")

  shiny::observeEvent(project$layers, {
    layers <- project$layers
    ids <- names(layers)
    if (is.null(ids)) ids <- character()
    choices <- stats::setNames(ids, vapply(layers, function(layer) {
      paste(layer$name, "(", layer$type, ")")
    }, character(1)))
    selected <- active_layer()
    if (!selected %in% names(layers)) {
      selected <- if (length(layers)) names(layers)[[1]] else ""
      active_layer(selected)
    }
    shiny::updateSelectInput(session, "active_layer", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$active_layer, {
    if (input$active_layer %in% names(project$layers) &&
        !identical(input$active_layer, active_layer())) active_layer(input$active_layer)
  })

  output$map <- leaflet::renderLeaflet({
    config <- project_list(project)
    tryCatch(build_leaflet_map(config), error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "warning")
      create_base_map(config$map)
    })
  })
}

shiny::shinyApp(ui, server)
