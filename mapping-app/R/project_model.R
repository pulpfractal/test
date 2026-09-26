# The project contains configuration and dataset records; layers only hold dataset IDs.
project_id <- function(name, existing = character(), fallback = "item") {
  base <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(name)))
  if (!nzchar(base)) base <- fallback
  make.unique(c(existing, base), sep = "_")[[length(existing) + 1L]]
}

field_names <- function(dataset) {
  if (is.null(dataset)) return(character())
  data <- dataset$data
  if (inherits(data, "sf")) names(sf::st_drop_geometry(data)) else names(data)
}

spatial_dataset_type <- function(data) {
  types <- unique(as.character(sf::st_geometry_type(data)))
  family <- ifelse(grepl("POINT$", types), "point",
    ifelse(grepl("POLYGON$", types), "polygon",
      ifelse(grepl("LINESTRING$", types), "polyline", "unsupported")))
  if (length(unique(family)) != 1L || family[[1]] == "unsupported") {
    stop("A spatial dataset must contain only points, polygons, or polylines of one geometry family.", call. = FALSE)
  }
  family[[1]]
}

new_dataset <- function(data, name, existing = character(), id = NULL) {
  if (!is.data.frame(data) || !nrow(data)) stop("Upload a dataset with at least one row.", call. = FALSE)
  spatial <- inherits(data, "sf")
  type <- if (spatial) spatial_dataset_type(data) else "point"
  cols <- if (spatial) names(sf::st_drop_geometry(data)) else names(data)
  guess <- function(candidates) {
    matching <- cols[tolower(cols) %in% candidates]
    if (length(matching)) matching[[1]] else ""
  }
  list(
    id = if (is.null(id)) project_id(name, existing, "dataset") else id,
    name = name,
    type = type,
    data = data,
    lat = if (spatial) "" else guess(c("latitude", "lat", "y")),
    lon = if (spatial) "" else guess(c("longitude", "lon", "lng", "long", "x")),
    id_col = if (length(cols)) cols[[1]] else ""
  )
}

layer_types <- function(dataset) {
  if (is.null(dataset)) return(character())
  switch(dataset$type, point = c("circleMarker", "marker"),
    polygon = "polygon", polyline = "polyline", character())
}

new_layer <- function(dataset, existing = character(), name = dataset$name,
                      type = layer_types(dataset)[[1]]) {
  if (!type %in% layer_types(dataset)) stop("Layer type is incompatible with the dataset geometry.", call. = FALSE)
  list(
    id = project_id(name, existing, "layer"), name = name,
    dataset = dataset$id, type = type, visible = TRUE, group = name,
    marker = list(radius = 6, stroke = TRUE, stroke_color = "", stroke_width = 2,
      stroke_opacity = 1, fill_opacity = 0.8, cluster = FALSE,
      cluster_radius = 80, spiderfy = TRUE, min_zoom = 0, max_zoom = 22),
    colour = list(mode = "single", single = if (type == "polygon") "#FC8D59" else "#2C7FB8", column = "",
      palette = "Viridis", n = 5, reverse = FALSE,
      custom_palette = "#2C7FB8, #7FCDBB, #EDF8B1", mapping = "",
      na_colour = "#BDBDBD"),
    polygon = list(fill_opacity = 0.2, outline = "#E34A33", width = 2,
      opacity = 1, dash = "", highlight = FALSE, hover_weight = 4,
      hover_opacity = 1, bring_to_front = TRUE, send_to_back = FALSE),
    popup = list(enabled = TRUE, mode = "html", title = "", fields = character(),
      labels = "", layout = "table", template = "<b>{id}</b>"),
    label = list(enabled = FALSE, column = "", permanent = FALSE, size = 12,
      style = "normal", direction = "auto", opacity = 1),
    legend = list(enabled = TRUE, title = "", position = "bottomright",
      opacity = 1, show_na = TRUE, behaviour = "auto"),
    filter = ""
  )
}

default_map <- function() {
  list(tiles = "OpenStreetMap", extent = "fit", center_lon = 0,
    center_lat = 0, zoom = 4, min_zoom = 1, max_zoom = 19,
    zoom_control = TRUE, background = "#f4f4f4")
}

default_widgets <- function() {
  list(layer_control = TRUE, search = FALSE, scale_bar = FALSE,
    fullscreen = FALSE, reset_view = FALSE, mouse_coords = FALSE,
    minimap = FALSE, measure = FALSE)
}

project_list <- function(project) {
  list(datasets = project$datasets, layers = project$layers, map = project$map,
    widgets = project$widgets, filters = project$filters)
}

add_project_dataset <- function(project, data, name, auto_layer = TRUE) {
  dataset <- new_dataset(data, name, names(project$datasets))
  datasets <- project$datasets
  datasets[[dataset$id]] <- dataset
  project$datasets <- datasets
  if (auto_layer) {
    layers <- project$layers
    layer <- new_layer(dataset, names(layers))
    layers[[layer$id]] <- layer
    project$layers <- layers
    return(list(dataset = dataset$id, layer = layer$id))
  }
  list(dataset = dataset$id, layer = NULL)
}

remove_project_dataset <- function(project, id) {
  datasets <- project$datasets
  datasets[[id]] <- NULL
  project$datasets <- datasets
  layers <- project$layers
  layers <- layers[!vapply(layers, function(layer) identical(layer$dataset, id), logical(1))]
  project$layers <- layers
  filters <- project$filters
  filters <- filters[!vapply(filters, function(filter) identical(filter$dataset, id), logical(1))]
  project$filters <- filters
  invisible(NULL)
}

set_project_layer <- function(project, layer) {
  layers <- project$layers
  if (!identical(layers[[layer$id]], layer)) {
    layers[[layer$id]] <- layer
    project$layers <- layers
  }
  invisible(layer)
}

editor_layer <- function(project, active_layer, editor_id) {
  if (is.null(editor_id) || !identical(editor_id, active_layer())) return(NULL)
  project$layers[[editor_id]]
}

# Versioned input IDs prevent the old layer's browser values from editing a new layer.
editor_input_key <- function(id, revision, field) {
  paste0("editor_", id, "_r", revision, "_", field)
}

editor_inputs <- function(input, id, revision, fields) {
  stats::setNames(lapply(fields, function(field) {
    input[[editor_input_key(id, revision, field)]]
  }), fields)
}

valid_number <- function(value, default, lower = -Inf, upper = Inf) {
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || !is.finite(number)) return(default)
  max(lower, min(upper, number))
}

as_filter_date <- function(values) {
  if (inherits(values, "Date")) return(values)
  if (inherits(values, "POSIXt")) return(as.Date(values))
  suppressWarnings(as.Date(as.character(values), format = "%Y-%m-%d"))
}

# Criteria are combined with AND, without changing the uploaded dataset.
filter_dataset <- function(data, rules) {
  for (rule in rules) {
    if (!rule$column %in% names(data)) {
      stop("Filter column is no longer available: ", rule$column, call. = FALSE)
    }
    values <- data[[rule$column]]
    missing <- is.na(values)
    keep <- switch(rule$type,
      categorical = as.character(values) %in% rule$values,
      numeric = {
        numbers <- suppressWarnings(as.numeric(values))
        missing <- is.na(numbers)
        !missing & numbers >= rule$min & numbers <= rule$max
      },
      logical = !missing & as.logical(values) == rule$value,
      date = {
        dates <- as_filter_date(values)
        missing <- is.na(dates)
        !missing & dates >= as.Date(rule$min) & dates <= as.Date(rule$max)
      },
      stop("Unsupported filter type: ", rule$type, call. = FALSE)
    )
    if (isTRUE(rule$include_na)) keep <- keep | missing
    keep[is.na(keep)] <- FALSE
    data <- data[keep, , drop = FALSE]
  }
  data
}

layer_data <- function(project, layer) {
  dataset <- project$datasets[[layer$dataset]]
  if (is.null(dataset)) return(NULL)
  data <- dataset$data
  filter_id <- layer$filter
  if (!is.null(filter_id) && length(filter_id) && nzchar(filter_id)) {
    definition <- project$filters[[filter_id]]
    if (!is.null(definition) && identical(definition$dataset, dataset$id)) {
      data <- filter_dataset(data, definition$rules)
    }
  }
  data
}
