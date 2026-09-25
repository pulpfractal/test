mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fileInput(ns("data_file"), "Upload point data (CSV)", accept = ".csv"),
    shiny::fileInput(ns("polygon_file"), "Upload polygons (GeoJSON or GPKG)", accept = c(".geojson", ".json", ".gpkg")),
    shiny::uiOutput(ns("column_inputs")),
    shiny::tableOutput(ns("preview"))
  )
}

mod_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    data_rv <- shiny::reactiveVal(NULL)
    polygons_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$data_file, {
      tryCatch({
        data_rv(utils::read.csv(input$data_file$datapath, check.names = FALSE, stringsAsFactors = FALSE))
      }, error = function(e) {
        shiny::showNotification(paste("Could not read CSV:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$polygon_file, {
      tryCatch({
        polygons_rv(sf::st_read(input$polygon_file$datapath, quiet = TRUE))
      }, error = function(e) {
        shiny::showNotification(paste("Could not read polygon file:", conditionMessage(e)), type = "error")
      })
    })

    output$column_inputs <- shiny::renderUI({
      shiny::req(data_rv())
      cols <- names(data_rv())
      shiny::tagList(
        shiny::selectInput(session$ns("id_col"), "Record ID column", choices = cols, selected = cols[[1]]),
        shiny::selectInput(session$ns("lat_col"), "Latitude column", choices = c("(not set)" = "", cols), selected = ""),
        shiny::selectInput(session$ns("lon_col"), "Longitude column", choices = c("(not set)" = "", cols), selected = "")
      )
    })

    output$preview <- shiny::renderTable({
      shiny::req(data_rv())
      utils::head(data_rv(), 6)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    list(
      data = shiny::reactive(data_rv()),
      polygons = shiny::reactive(polygons_rv()),
      columns = shiny::reactive(list(id = input$id_col, lat = input$lat_col, lon = input$lon_col))
    )
  })
}
