mod_colours_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::uiOutput(ns("editor")), shiny::uiOutput(ns("palette_preview")))
}

mod_colours_server <- function(id, project, active_layer, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    modes <- c("Single colour" = "single", "Category" = "factor", "Continuous numeric" = "numeric",
      "Numeric bins" = "bin", "Quantiles" = "quantile", "Custom category mapping" = "custom")
    palettes <- c("Viridis", "Blues 3", "Reds 3", "Spectral", "Set 2", "Custom")
    inputs <- function() editor_inputs(input, active_layer(), editor_revision(), c(
      "editor_id", "mode", "single", "column", "palette", "n", "reverse",
      "custom_palette", "mapping", "na_colour"
    ))

    output$editor <- shiny::renderUI({
      id <- active_layer()
      rev <- editor_revision()
      ns <- function(field) session$ns(editor_input_key(id, rev, field))
      layer <- shiny::isolate(project$layers[[id]])
      if (is.null(layer)) return(shiny::helpText("Select a layer to edit its colours or polygon fill."))
      dataset <- shiny::isolate(project$datasets[[layer$dataset]])
      colour <- layer$colour
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput(ns("editor_id"), NULL, value = id)),
        shiny::selectInput(ns("mode"), "Colour mode", choices = modes, selected = colour$mode),
        shiny::textInput(ns("single"), if (layer$type == "polygon") "Single fill colour" else "Single colour",
          value = colour$single),
        shiny::selectInput(ns("column"), "Source column",
          choices = c("(none)" = "", field_names(dataset)), selected = colour$column),
        shiny::selectInput(ns("palette"), "Palette", choices = palettes, selected = colour$palette),
        shiny::numericInput(ns("n"), "Number of colours/bins", value = colour$n, min = 2, max = 24),
        shiny::checkboxInput(ns("reverse"), "Reverse palette", value = isTRUE(colour$reverse)),
        shiny::textAreaInput(ns("custom_palette"), "Custom palette (comma-separated colours)",
          value = colour$custom_palette, rows = 2),
        shiny::textAreaInput(ns("mapping"), "Custom categories (one Category = #hex per line)",
          value = colour$mapping, rows = 4),
        shiny::textInput(ns("na_colour"), "Missing/unmapped value colour", value = colour$na_colour),
        shiny::helpText("For polygons, the chosen colours control fill; for polylines, line colour. Custom category mappings are case-sensitive.")
      )
    }) |> shiny::bindEvent(active_layer(), editor_revision())

    output$palette_preview <- shiny::renderUI({
      input <- inputs()
      if (is.null(input$palette) || !nzchar(active_layer())) return(NULL)
      palette <- list(palette = input$palette, custom_palette = input$custom_palette,
        reverse = isTRUE(input$reverse))
      colours <- palette_colours(palette, valid_number(input$n, 5, 2, 24))
      shiny::tagList(shiny::tags$strong("Palette preview"),
        shiny::div(style = "display:flex; height:28px; margin-bottom:12px;",
          lapply(colours, function(colour) shiny::tags$span(title = colour,
            style = paste0("flex:1; background-color:", colour, ";")))))
    })

    shiny::observeEvent(inputs(), {
      input <- inputs()
      layer <- editor_layer(project, active_layer, input$editor_id)
      if (is.null(layer) || any(vapply(list(input$mode, input$single, input$column,
        input$palette, input$reverse, input$custom_palette, input$mapping, input$na_colour),
        is.null, logical(1)))) return()
      updated <- layer
      colour <- layer$colour
      colour$mode <- if (input$mode %in% modes) input$mode else "single"
      colour$single <- valid_colour(input$single, colour$single)
      colour$column <- if (input$column %in% field_names(project$datasets[[layer$dataset]])) input$column else ""
      colour$palette <- if (input$palette %in% palettes) input$palette else "Viridis"
      colour$n <- as.integer(valid_number(input$n, colour$n, 2, 24))
      colour$reverse <- isTRUE(input$reverse)
      colour$custom_palette <- input$custom_palette
      colour$mapping <- input$mapping
      colour$na_colour <- valid_colour(input$na_colour, colour$na_colour)
      updated$colour <- colour
      set_project_layer(project, updated)
    }, ignoreInit = TRUE)
  })
}
