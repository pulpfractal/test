mod_controls_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::accordion(
      bslib::accordion_panel(
        "Markers and colours",
        shiny::radioButtons(ns("marker_type"), "Marker type", choices = c("Circle" = "circle", "Standard pin" = "marker"), inline = TRUE),
        shiny::numericInput(ns("radius"), "Circle radius", value = 6, min = 1, max = 30),
        shiny::textInput(ns("marker_color"), "Single marker colour", value = "#2C7FB8"),
        shiny::radioButtons(ns("color_mode"), "Colour by", choices = c("Single colour" = "single", "Category" = "factor", "Number" = "numeric")),
        shiny::uiOutput(ns("color_column_ui")),
        shiny::textInput(ns("palette"), "Palette colours (comma-separated)", value = "#2C7FB8, #7FCDBB, #EDF8B1"),
        shiny::sliderInput(ns("opacity"), "Fill opacity", min = 0, max = 1, value = 0.8)
      ),
      bslib::accordion_panel(
        "Popups and labels",
        shiny::textInput(ns("popup_template"), "Popup HTML template", value = "<b>{id}</b>"),
        shiny::helpText("Use {column_name} placeholders. HTML markup is allowed; inserted data values are escaped."),
        shiny::selectInput(ns("label_col"), "Hover label column", choices = character()),
        shiny::checkboxInput(ns("cluster"), "Cluster overlapping points", value = FALSE)
      ),
      bslib::accordion_panel(
        "Map and polygons",
        shiny::selectInput(ns("tiles"), "Basemap", choices = c("OpenStreetMap", "CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery", "OpenTopoMap")),
        shiny::checkboxInput(ns("show_legend"), "Show colour legend", value = TRUE),
        shiny::textInput(ns("legend_title"), "Legend title", value = "Mapped values"),
        shiny::selectInput(ns("legend_position"), "Legend position", choices = c("bottomright", "bottomleft", "topright", "topleft")),
        shiny::textInput(ns("polygon_color"), "Polygon outline colour", value = "#E34A33"),
        shiny::textInput(ns("polygon_fill"), "Polygon fill colour", value = "#FC8D59"),
        shiny::sliderInput(ns("polygon_opacity"), "Polygon fill opacity", min = 0, max = 1, value = 0.2)
      )
    ),
    shiny::textAreaInput(ns("custom_leaflet"), "Advanced Leaflet R code (optional)", rows = 4,
      placeholder = "map <- map |> leaflet::addScaleBar()"),
    shiny::helpText("Advanced code is appended to the generated R renderer. It is not executed in the live preview.")
  )
}

mod_controls_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$color_column_ui <- shiny::renderUI({
      df <- data()
      cols <- if (is.null(df)) character() else names(df)
      shiny::selectInput(session$ns("color_col"), "Colour variable", choices = cols)
    })

    shiny::observeEvent(data(), {
      cols <- if (is.null(data())) character() else names(data())
      shiny::updateSelectInput(session, "label_col", choices = c("(none)" = "", cols), selected = "")
    }, ignoreInit = FALSE)

    shiny::reactive(list(
      marker_type = input$marker_type,
      radius = input$radius,
      marker_color = input$marker_color,
      color_mode = input$color_mode,
      color_col = input$color_col,
      palette = input$palette,
      opacity = input$opacity,
      popup_template = input$popup_template,
      label_col = input$label_col,
      cluster = input$cluster,
      tiles = input$tiles,
      show_legend = input$show_legend,
      legend_title = input$legend_title,
      legend_position = input$legend_position,
      polygon_color = input$polygon_color,
      polygon_fill = input$polygon_fill,
      polygon_opacity = input$polygon_opacity,
      custom_leaflet = input$custom_leaflet
    ))
  })
}
