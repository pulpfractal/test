options(shiny.maxRequestSize = 100 * 1024^2)

for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file, local = FALSE)
}

ui <- bslib::page_sidebar(
  title = "Modular Mapping Studio",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = bslib::sidebar(
    width = 370,
    open = "always",
    bslib::accordion(
      bslib::accordion_panel("Upload data and boundaries", mod_upload_ui("upload")),
      bslib::accordion_panel("Azure Maps geocoding", mod_geocode_ui("geocode")),
      bslib::accordion_panel("Map design", mod_controls_ui("controls")),
      open = c("Upload data and boundaries")
    ),
    shiny::helpText("CSV coordinates should be decimal degrees (WGS84). Polygon input accepts GeoJSON or GPKG.")
  ),
  bslib::navset_card_tab(
    bslib::nav_panel("Map preview", leaflet::leafletOutput("map", height = "75vh")),
    bslib::nav_panel(
      "Generated code and exports",
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_header("Renderer script"),
          shiny::verbatimTextOutput("generated_code"),
          full_screen = TRUE
        ),
        bslib::card(
          bslib::card_header("Export"),
          shiny::p("The HTML export captures the current map. The project ZIP contains modular R code and the uploaded data needed to recreate it."),
          shiny::downloadButton("download_html", "Download standalone HTML", class = "btn-primary"),
          shiny::br(), shiny::br(),
          shiny::downloadButton("download_script", "Download renderer script"),
          shiny::br(), shiny::br(),
          shiny::downloadButton("download_bundle", "Download project ZIP"),
          shiny::hr(),
          shiny::p("Basemap tiles are fetched online. The app never includes the Azure Maps key in downloaded files."),
          full_screen = FALSE
        )
      )
    )
  )
)

server <- function(input, output, session) {
  upload <- mod_upload_server("upload")
  geocoded <- mod_geocode_server("geocode", upload$data, upload$columns)
  settings <- mod_controls_server("controls", geocoded$data)

  output$map <- leaflet::renderLeaflet({
    tryCatch(
      build_leaflet_map(geocoded$data(), upload$polygons(), settings(), geocoded$columns()),
      error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "warning")
        leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron")
      }
    )
  })

  output$generated_code <- shiny::renderText({
    shiny::req(geocoded$data())
    make_renderer_script(settings(), geocoded$columns(), settings()$custom_leaflet)
  })

  output$download_html <- shiny::downloadHandler(
    filename = function() "map.html",
    content = function(file) {
      render_map_html(geocoded$data(), upload$polygons(), settings(), geocoded$columns(), file, selfcontained = TRUE)
    }
  )

  output$download_script <- shiny::downloadHandler(
    filename = function() "render_map.R",
    content = function(file) {
      writeLines(make_renderer_script(settings(), geocoded$columns(), settings()$custom_leaflet), file, useBytes = TRUE)
    }
  )

  output$download_bundle <- shiny::downloadHandler(
    filename = function() "mapping-project.zip",
    content = function(file) {
      write_project_bundle(file, geocoded$data(), upload$polygons(), settings(), geocoded$columns(), settings()$custom_leaflet)
    }
  )
}

shiny::shinyApp(ui, server)
