wgs84_data <- function(data) {
  if (!inherits(data, "sf")) stop("A spatial layer requires an sf dataset.", call. = FALSE)
  if (is.na(sf::st_crs(data))) sf::st_crs(data) <- 4326
  sf::st_transform(data, 4326)
}

prepare_layer_data <- function(dataset, layer, data = dataset$data) {
  if (is.null(data) || !nrow(data)) return(NULL)
  if (!layer$type %in% layer_types(dataset)) {
    stop("Layer ", layer$name, " is incompatible with dataset ", dataset$name, ".", call. = FALSE)
  }
  if (dataset$type != "point") {
    data <- wgs84_data(data)
    data <- data[!sf::st_is_empty(data), , drop = FALSE]
    return(if (nrow(data)) data else NULL)
  }
  if (inherits(data, "sf")) {
    data <- wgs84_data(data)
    data <- data[!sf::st_is_empty(data), , drop = FALSE]
    if (!nrow(data)) return(NULL)
    data <- suppressWarnings(sf::st_cast(data, "POINT", warn = FALSE))
    coordinates <- sf::st_coordinates(data)
    lon <- coordinates[, 1]
    lat <- coordinates[, 2]
  } else {
    if (is.null(dataset$lat) || is.null(dataset$lon) ||
        !dataset$lat %in% names(data) || !dataset$lon %in% names(data) ||
        identical(dataset$lat, dataset$lon)) return(NULL)
    lat <- suppressWarnings(as.numeric(as.character(data[[dataset$lat]])))
    lon <- suppressWarnings(as.numeric(as.character(data[[dataset$lon]])))
  }
  keep <- is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180
  if (!any(keep)) return(NULL)
  data <- data[keep, , drop = FALSE]
  data$.map_lat <- lat[keep]
  data$.map_lon <- lon[keep]
  data
}

# Unique Leaflet groups preserve independent show/hide even when logical groups match.
layer_group <- function(layer) {
  group <- if (is.null(layer$group) || !nzchar(layer$group)) layer$name else layer$group
  paste0(as.character(htmltools::htmlEscape(group)), " / ",
    as.character(htmltools::htmlEscape(layer$name)), " [", layer$id, "]")
}

search_titles <- function(data, dataset, layer) {
  cols <- names(map_records(data))
  column <- layer$label$column
  if (is.null(column) || !column %in% cols) column <- dataset$id_col
  if (is.null(column) || !column %in% cols) return(rep(dataset$name, nrow(data)))
  vapply(map_records(data)[[column]], popup_value, character(1), USE.NAMES = FALSE)
}

cluster_options <- function(layer) {
  if (!isTRUE(layer$marker$cluster)) return(NULL)
  leaflet::markerClusterOptions(
    maxClusterRadius = valid_number(layer$marker$cluster_radius, 80, 10, 300),
    spiderfyOnMaxZoom = isTRUE(layer$marker$spiderfy))
}

layer_ids <- function(layer, n) paste0(layer$id, ":", seq_len(n))

add_circle_layer <- function(map, dataset, layer, data = dataset$data) {
  points <- prepare_layer_data(dataset, layer, data)
  if (is.null(points)) return(map)
  colours <- map_colours(points, layer$colour)$colors
  marker <- layer$marker
  outline <- if (is.null(marker$stroke_color) || !nzchar(marker$stroke_color)) colours else
    valid_colour(marker$stroke_color, colours[[1]])
  leaflet::addCircleMarkers(map, data = points, lng = ~.map_lon, lat = ~.map_lat,
    layerId = layer_ids(layer, nrow(points)), group = layer_group(layer),
    radius = marker$radius, stroke = isTRUE(marker$stroke), color = outline,
    weight = marker$stroke_width, opacity = marker$stroke_opacity,
    fillColor = colours, fillOpacity = marker$fill_opacity,
    popup = popup_for_layer(points, layer, dataset),
    label = labels_for_layer(points, layer), labelOptions = label_options_for_layer(layer),
    options = leaflet::pathOptions(title = search_titles(points, dataset, layer)),
    clusterOptions = cluster_options(layer), clusterId = paste0("cluster_", layer$id))
}

pin_icons <- function(colours) {
  svg <- vapply(colours, function(colour) {
    rgb <- grDevices::col2rgb(valid_colour(colour, "#BDBDBD"))
    hex <- sprintf("#%02X%02X%02X", rgb[1, 1], rgb[2, 1], rgb[3, 1])
    image <- paste0("<svg xmlns='http://www.w3.org/2000/svg' width='30' height='42' viewBox='0 0 30 42'>",
      "<path d='M15 1C7.3 1 1 7.3 1 15c0 10 14 26 14 26s14-16 14-26C29 7.3 22.7 1 15 1z'",
      " fill='", hex, "' stroke='#ffffff' stroke-width='2'/><circle cx='15' cy='15' r='5' fill='#ffffff'/></svg>")
    paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(image, reserved = TRUE))
  }, character(1))
  leaflet::icons(iconUrl = svg, iconWidth = 30, iconHeight = 42, iconAnchorX = 15, iconAnchorY = 42)
}

add_marker_layer <- function(map, dataset, layer, data = dataset$data) {
  points <- prepare_layer_data(dataset, layer, data)
  if (is.null(points)) return(map)
  colours <- map_colours(points, layer$colour)$colors
  leaflet::addMarkers(map, data = points, lng = ~.map_lon, lat = ~.map_lat,
    layerId = layer_ids(layer, nrow(points)), group = layer_group(layer), icon = pin_icons(colours),
    popup = popup_for_layer(points, layer, dataset), label = labels_for_layer(points, layer),
    labelOptions = label_options_for_layer(layer),
    options = leaflet::markerOptions(title = search_titles(points, dataset, layer)),
    clusterOptions = cluster_options(layer), clusterId = paste0("cluster_", layer$id))
}

shape_highlight <- function(layer) {
  style <- layer$polygon
  if (!isTRUE(style$highlight)) return(NULL)
  leaflet::highlightOptions(weight = style$hover_weight, opacity = style$hover_opacity,
    fillOpacity = style$hover_opacity, bringToFront = isTRUE(style$bring_to_front),
    sendToBack = isTRUE(style$send_to_back))
}

add_polygon_layer <- function(map, dataset, layer, data = dataset$data) {
  polygons <- prepare_layer_data(dataset, layer, data)
  if (is.null(polygons)) return(map)
  style <- layer$polygon
  leaflet::addPolygons(map, data = polygons,
    layerId = layer_ids(layer, nrow(polygons)), group = layer_group(layer),
    color = valid_colour(style$outline, "#E34A33"), weight = style$width,
    opacity = style$opacity, dashArray = if (nzchar(style$dash)) style$dash else NULL,
    fillColor = map_colours(polygons, layer$colour)$colors, fillOpacity = style$fill_opacity,
    popup = popup_for_layer(polygons, layer, dataset),
    label = labels_for_layer(polygons, layer), labelOptions = label_options_for_layer(layer),
    options = leaflet::pathOptions(title = search_titles(polygons, dataset, layer)),
    highlightOptions = shape_highlight(layer))
}

add_polyline_layer <- function(map, dataset, layer, data = dataset$data) {
  lines <- prepare_layer_data(dataset, layer, data)
  if (is.null(lines)) return(map)
  style <- layer$polygon
  leaflet::addPolylines(map, data = lines,
    layerId = layer_ids(layer, nrow(lines)), group = layer_group(layer),
    color = map_colours(lines, layer$colour)$colors, weight = style$width,
    opacity = style$opacity, dashArray = if (nzchar(style$dash)) style$dash else NULL,
    popup = popup_for_layer(lines, layer, dataset),
    label = labels_for_layer(lines, layer), labelOptions = label_options_for_layer(layer),
    options = leaflet::pathOptions(title = search_titles(lines, dataset, layer)),
    highlightOptions = shape_highlight(layer))
}

add_map_layer <- function(map, dataset, layer, data = dataset$data) {
  switch(layer$type,
    circleMarker = add_circle_layer(map, dataset, layer, data),
    marker = add_marker_layer(map, dataset, layer, data),
    polygon = add_polygon_layer(map, dataset, layer, data),
    polyline = add_polyline_layer(map, dataset, layer, data),
    stop("Unsupported layer type: ", layer$type, call. = FALSE))
}
