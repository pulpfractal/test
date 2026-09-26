map_bounds <- function(entries) {
  visible <- Filter(function(entry) isTRUE(entry$layer$visible), entries)
  if (!length(visible)) return(NULL)
  boxes <- lapply(visible, function(entry) {
    if (entry$layer$type %in% c("circleMarker", "marker")) {
      c(min(entry$data$.map_lon), min(entry$data$.map_lat),
        max(entry$data$.map_lon), max(entry$data$.map_lat))
    } else as.numeric(sf::st_bbox(entry$data))
  })
  bounds <- c(min(vapply(boxes, `[[`, numeric(1), 1L)),
    min(vapply(boxes, `[[`, numeric(1), 2L)),
    max(vapply(boxes, `[[`, numeric(1), 3L)),
    max(vapply(boxes, `[[`, numeric(1), 4L)))
  if (all(is.finite(bounds))) bounds else NULL
}

create_base_map <- function(settings) {
  settings <- utils::modifyList(default_map(), if (is.null(settings)) list() else settings)
  min_zoom <- valid_number(settings$min_zoom, 1, 0, 22)
  max_zoom <- max(min_zoom, valid_number(settings$max_zoom, 19, 0, 22))
  providers <- c("OpenStreetMap", "CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery", "OpenTopoMap")
  tiles <- if (settings$tiles %in% providers) settings$tiles else "OpenStreetMap"
  leaflet::leaflet(options = leaflet::leafletOptions(
    minZoom = min_zoom, maxZoom = max_zoom, zoomControl = isTRUE(settings$zoom_control))) |>
    leaflet::addProviderTiles(tiles, group = tiles)
}

add_map_legends <- function(map, entries) {
  for (entry in entries) {
    layer <- entry$layer
    legend <- layer$legend
    if (!isTRUE(legend$enabled)) next
    result <- map_colours(entry$data, layer$colour)
    position <- if (legend$position %in% c("bottomright", "bottomleft", "topright", "topleft")) legend$position else "bottomright"
    title <- if (!is.null(legend$title) && nzchar(legend$title)) legend$title else layer$name
    title <- as.character(htmltools::htmlEscape(title))
    group <- if (identical(legend$behaviour, "always")) NULL else layer_group(layer)
    opacity <- valid_number(legend$opacity, 1, 0, 1)
    if (!is.null(result$pal)) {
      values <- result$values
      if (!isTRUE(legend$show_na)) values <- values[!is.na(values)]
      map <- leaflet::addLegend(map, pal = result$pal, values = values,
        na.label = if (isTRUE(legend$show_na)) "Missing" else "",
        title = title, position = position, opacity = opacity, group = group)
    } else if (identical(result$mode, "custom")) {
      mapping <- result$mapping
      if (!length(mapping)) next
      labels <- as.character(htmltools::htmlEscape(names(mapping)))
      colours <- unname(mapping)
      missing <- is.na(result$values) | !as.character(result$values) %in% names(mapping)
      if (isTRUE(legend$show_na) && any(missing)) {
        colours <- c(colours, result$na_colour)
        labels <- c(labels, "Missing")
      }
      map <- leaflet::addLegend(map, colors = colours, labels = labels,
        title = title, position = position, opacity = opacity, group = group)
    } else {
      map <- leaflet::addLegend(map, colors = result$colors[[1]], labels = layer$name,
        title = title, position = position, opacity = opacity, group = group)
    }
  }
  map
}

add_map_widgets <- function(map, project, entries) {
  widgets <- utils::modifyList(default_widgets(), if (is.null(project$widgets)) list() else project$widgets)
  settings <- utils::modifyList(default_map(), if (is.null(project$map)) list() else project$map)
  groups <- vapply(entries, function(entry) layer_group(entry$layer), character(1))
  if (isTRUE(widgets$layer_control)) {
    tiles <- if (settings$tiles %in% c("OpenStreetMap", "CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery", "OpenTopoMap")) settings$tiles else "OpenStreetMap"
    map <- leaflet::addLayersControl(map, baseGroups = tiles, overlayGroups = groups,
      options = leaflet::layersControlOptions(collapsed = FALSE))
  }
  if (isTRUE(widgets$scale_bar)) map <- leaflet::addScaleBar(map)
  if (isTRUE(widgets$minimap)) map <- leaflet::addMiniMap(map, tiles = settings$tiles, width = 150, height = 120)
  if (isTRUE(widgets$measure)) map <- leaflet::addMeasure(map)
  if (requireNamespace("leaflet.extras", quietly = TRUE)) {
    if (isTRUE(widgets$fullscreen)) map <- leaflet.extras::addFullscreenControl(map)
    if (isTRUE(widgets$search) && length(groups)) {
      map <- leaflet.extras::addSearchFeatures(map, targetGroups = groups,
        options = leaflet.extras::searchFeaturesOptions(propertyName = "title", openPopup = TRUE))
    }
  }
  map
}

# One JS hook is used for small Leaflet controls and the per-layer zoom policy.
decorate_map <- function(map, project, entries, bounds) {
  settings <- utils::modifyList(default_map(), if (is.null(project$map)) list() else project$map)
  widgets <- utils::modifyList(default_widgets(), if (is.null(project$widgets)) list() else project$widgets)
  zoom_layers <- lapply(entries, function(entry) list(
    group = layer_group(entry$layer), visible = isTRUE(entry$layer$visible),
    min = valid_number(entry$layer$marker$min_zoom, 0, 0, 22),
    max = valid_number(entry$layer$marker$max_zoom, 22, 0, 22)))
  zoom_layers <- Filter(function(x) x$min > 0 || x$max < 22, zoom_layers)
  code <- paste(c(
    "function(el, x, data) {",
    "  var map = this;",
    "  el.style.backgroundColor = data.background;",
    "  if (data.mouse) {",
    "    var coords = L.control({position: 'bottomleft'});",
    "    coords.onAdd = function() {",
    "      var box = L.DomUtil.create('div', 'leaflet-control leaflet-bar');",
    "      box.style.cssText = 'background:white;padding:4px 8px;font:12px sans-serif';",
    "      box.textContent = 'Move the cursor over the map';",
    "      map.on('mousemove', function(e) { box.textContent = 'Lon ' + e.latlng.lng.toFixed(5) + '  Lat ' + e.latlng.lat.toFixed(5); });",
    "      map.on('mouseout', function() { box.textContent = 'Move the cursor over the map'; });",
    "      return box;",
    "    };",
    "    coords.addTo(map);",
    "  }",
    "  if (data.reset) {",
    "    var reset = L.control({position: 'topleft'});",
    "    reset.onAdd = function() {",
    "      var box = L.DomUtil.create('div', 'leaflet-bar leaflet-control');",
    "      var button = L.DomUtil.create('a', '', box);",
    "      button.href = '#'; button.title = 'Reset initial view';",
    "      button.setAttribute('aria-label', 'Reset initial view'); button.textContent = '\u2302';",
    "      L.DomEvent.disableClickPropagation(box);",
    "      L.DomEvent.on(button, 'click', function(e) {",
    "        L.DomEvent.preventDefault(e);",
    "        if (data.fit && data.bounds) {",
    "          map.fitBounds([[data.bounds[1], data.bounds[0]], [data.bounds[3], data.bounds[2]]]);",
    "        } else { map.setView([data.center[1], data.center[0]], data.zoom); }",
    "      });",
    "      return box;",
    "    };",
    "    reset.addTo(map);",
    "  }",
    "  var limits = data.zoomLayers || [];",
    "  if (limits.length && map.layerManager && map.layerManager.getLayerGroup) {",
    "    var wanted = {}; var adjusting = false;",
    "    limits.forEach(function(rule) { wanted[rule.group] = rule.visible; });",
    "    function syncZoom() {",
    "      adjusting = true;",
    "      try {",
    "        limits.forEach(function(rule) {",
    "          var group = map.layerManager.getLayerGroup(rule.group);",
    "          if (!group) return;",
    "          var show = wanted[rule.group] && map.getZoom() >= rule.min && map.getZoom() <= rule.max;",
    "          if (show && !map.hasLayer(group)) map.addLayer(group);",
    "          if (!show && map.hasLayer(group)) map.removeLayer(group);",
    "        });",
    "      } finally { adjusting = false; }",
    "    }",
    "    map.on('overlayadd', function(e) { if (!adjusting && Object.prototype.hasOwnProperty.call(wanted, e.name)) { wanted[e.name] = true; syncZoom(); } });",
    "    map.on('overlayremove', function(e) { if (!adjusting && Object.prototype.hasOwnProperty.call(wanted, e.name)) { wanted[e.name] = false; syncZoom(); } });",
    "    map.on('zoomend', syncZoom); syncZoom();",
    "  }",
    "}"
  ), collapse = "\n")
  htmlwidgets::onRender(map, code, data = list(
    background = valid_colour(settings$background, "#f4f4f4"),
    mouse = isTRUE(widgets$mouse_coords), reset = isTRUE(widgets$reset_view),
    fit = identical(settings$extent, "fit"), bounds = unname(bounds),
    center = c(valid_number(settings$center_lon, 0, -180, 180),
      valid_number(settings$center_lat, 0, -90, 90)),
    zoom = valid_number(settings$zoom, 4, 0, 22), zoomLayers = zoom_layers))
}

build_leaflet_map <- function(project) {
  map <- create_base_map(project$map)
  entries <- list()
  for (layer in project$layers) {
    dataset <- project$datasets[[layer$dataset]]
    if (is.null(dataset)) next
    data <- prepare_layer_data(dataset, layer, layer_data(project, layer))
    if (is.null(data)) next
    map <- add_map_layer(map, dataset, layer, data)
    entries[[length(entries) + 1L]] <- list(dataset = dataset, layer = layer, data = data)
  }
  bounds <- map_bounds(entries)
  settings <- utils::modifyList(default_map(), if (is.null(project$map)) list() else project$map)
  if (identical(settings$extent, "fit") && !is.null(bounds)) {
    map <- leaflet::fitBounds(map, bounds[[1]], bounds[[2]], bounds[[3]], bounds[[4]])
  } else {
    map <- leaflet::setView(map,
      lng = valid_number(settings$center_lon, 0, -180, 180),
      lat = valid_number(settings$center_lat, 0, -90, 90),
      zoom = valid_number(settings$zoom, 4, 0, 22))
  }
  map <- add_map_legends(map, entries)
  map <- add_map_widgets(map, project, entries)
  for (entry in entries) {
    if (!isTRUE(entry$layer$visible)) map <- leaflet::hideGroup(map, layer_group(entry$layer))
  }
  decorate_map(map, project, entries, bounds)
}
