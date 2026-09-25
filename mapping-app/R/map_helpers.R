`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) y else x

parse_palette <- function(value, fallback) {
  colors <- trimws(strsplit(value %||% "", ",", fixed = TRUE)[[1]])
  colors <- colors[nzchar(colors)]
  valid <- vapply(colors, function(color) !inherits(try(grDevices::col2rgb(color), silent = TRUE), "try-error"), logical(1))
  if (length(colors) < 2 || !all(valid)) fallback else colors
}

popup_html <- function(data, template, id_col) {
  if (is.null(template) || !nzchar(template)) template <- paste0("<b>{", id_col, "}</b>")
  vapply(seq_len(nrow(data)), function(i) {
    result <- template
    if (!is.null(id_col) && id_col %in% names(data)) {
      id_value <- data[[id_col]][i]
      id_value <- if (is.na(id_value)) "" else as.character(id_value)
      result <- gsub("{id}", as.character(htmltools::htmlEscape(id_value)), result, fixed = TRUE)
    }
    for (column in names(data)) {
      token <- paste0("{", column, "}")
      value <- data[[column]][i]
      value <- if (is.na(value)) "" else as.character(value)
      result <- gsub(token, as.character(htmltools::htmlEscape(value)), result, fixed = TRUE)
    }
    result
  }, character(1), USE.NAMES = FALSE)
}

build_leaflet_map <- function(data, polygons = NULL, settings, columns = list()) {
  validate <- function(condition, message) if (!condition) stop(message, call. = FALSE)
  validate(!is.null(data) && nrow(data) > 0, "Upload a non-empty point dataset first.")
  validate(!is.null(columns$lat) && nzchar(columns$lat) && columns$lat %in% names(data), "Choose a latitude column.")
  validate(!is.null(columns$lon) && nzchar(columns$lon) && columns$lon %in% names(data), "Choose a longitude column.")

  latitude <- suppressWarnings(as.numeric(data[[columns$lat]]))
  longitude <- suppressWarnings(as.numeric(data[[columns$lon]]))
  valid <- is.finite(latitude) & is.finite(longitude) & latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180
  validate(any(valid), "No rows have valid latitude and longitude values.")
  points <- data[valid, , drop = FALSE]
  points$.map_lat <- latitude[valid]
  points$.map_lon <- longitude[valid]

  map <- leaflet::leaflet(points) |>
    leaflet::addProviderTiles(settings$tiles %||% "OpenStreetMap")

  if (!is.null(polygons) && nrow(polygons) > 0) {
    if (is.na(sf::st_crs(polygons))) sf::st_crs(polygons) <- 4326
    polygons <- sf::st_transform(polygons, 4326)
    map <- map |>
      leaflet::addPolygons(
        data = polygons,
        color = settings$polygon_color %||% "#E34A33",
        fillColor = settings$polygon_fill %||% "#FC8D59",
        fillOpacity = settings$polygon_opacity %||% 0.2,
        weight = 2
      )
  }

  marker_type <- settings$marker_type %||% "circle"
  mode <- settings$color_mode %||% "single"
  pal <- NULL
  color_values <- settings$marker_color %||% "#2C7FB8"
  palette <- parse_palette(settings$palette, c("#2C7FB8", "#7FCDBB", "#EDF8B1"))
  color_col <- settings$color_col %||% ""

  if (mode %in% c("factor", "numeric") && color_col %in% names(points)) {
    values <- points[[color_col]]
    if (mode == "factor") {
      pal <- leaflet::colorFactor(palette, domain = as.character(values), na.color = "#BDBDBD")
      color_values <- pal(as.character(values))
    } else {
      numeric_values <- suppressWarnings(as.numeric(values))
      pal <- leaflet::colorNumeric(palette, domain = numeric_values, na.color = "#BDBDBD")
      color_values <- pal(numeric_values)
    }
  } else {
    color_values <- rep(settings$marker_color %||% "#2C7FB8", nrow(points))
  }

  popup <- popup_html(points, settings$popup_template, columns$id %||% names(points)[[1]])
  label_col <- settings$label_col %||% ""
  labels <- if (label_col %in% names(points)) lapply(as.character(points[[label_col]]), function(value) {
    htmltools::HTML(as.character(htmltools::htmlEscape(value)))
  }) else NULL

  if (marker_type == "marker") {
    marker_svg <- vapply(color_values, function(color) {
      svg <- paste0("<svg xmlns='http://www.w3.org/2000/svg' width='30' height='42' viewBox='0 0 30 42'><path d='M15 1C7.3 1 1 7.3 1 15c0 10 14 26 14 26s14-16 14-26C29 7.3 22.7 1 15 1z' fill='", color, "' stroke='#ffffff' stroke-width='2'/><circle cx='15' cy='15' r='5' fill='#ffffff'/></svg>")
      paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(svg, reserved = TRUE))
    }, character(1))
    icon <- leaflet::icons(iconUrl = marker_svg, iconWidth = 30, iconHeight = 42, iconAnchorX = 15, iconAnchorY = 42)
    map <- map |>
      leaflet::addMarkers(lng = ~.map_lon, lat = ~.map_lat, popup = popup, label = labels,
        icon = icon, clusterOptions = if (isTRUE(settings$cluster)) leaflet::markerClusterOptions() else NULL)
  } else {
    map <- map |>
      leaflet::addCircleMarkers(lng = ~.map_lon, lat = ~.map_lat, radius = settings$radius %||% 6,
        color = color_values, fillColor = color_values, fillOpacity = settings$opacity %||% 0.8,
        popup = popup, label = labels,
        clusterOptions = if (isTRUE(settings$cluster)) leaflet::markerClusterOptions() else NULL)
  }

  if (!is.null(pal) && isTRUE(settings$show_legend)) {
    map <- map |>
      leaflet::addLegend(pal = pal, values = points[[color_col]], title = settings$legend_title %||% color_col,
        position = settings$legend_position %||% "bottomright")
  }

  map |>
    leaflet::fitBounds(min(points$.map_lon), min(points$.map_lat), max(points$.map_lon), max(points$.map_lat))
}

render_map_html <- function(data, polygons, settings, columns, file, selfcontained = TRUE) {
  widget <- build_leaflet_map(data, polygons, settings, columns)
  htmlwidgets::saveWidget(widget, file = file, selfcontained = selfcontained)
  invisible(file)
}
