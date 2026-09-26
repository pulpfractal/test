# Run with source("tests.R") from mapping-app, or source("mapping-app/tests.R") from its parent.
suppressMessages(local({
  app_dir <- if (file.exists("app.R") && dir.exists("R")) "." else "mapping-app"
  stopifnot(file.exists(file.path(app_dir, "app.R")))
  server_fun <- shiny::shinyAppDir(app_dir)$serverFuncSource()

  stores <- new_dataset(data.frame(id = c("s1", "s2"), latitude = c(-33, -34),
    longitude = c(151, 150), category = c("A", "B"), score = c(5, 5)), "Stores")
  another <- new_dataset(stores$data, "Stores", existing = stores$id)
  stopifnot(stores$id != another$id, stores$type == "point")
  points <- new_layer(stores)
  points$colour$mode <- "factor"
  points$colour$column <- "category"
  titles <- search_titles(prepare_layer_data(stores, points), stores, points)
  stopifnot(identical(titles, c("s1", "s2")), is.null(names(titles)))
  filtered_points <- new_layer(stores, existing = points$id, name = "Only A", type = "marker")
  filtered_points$filter <- "only_a"
  ring <- matrix(c(150, -35, 151, -35, 151, -34, 150, -34, 150, -35), ncol = 2, byrow = TRUE)
  boundary <- new_dataset(sf::st_sf(zone = "Zone A", geometry = sf::st_sfc(sf::st_polygon(list(ring)))), "Boundary")
  polygon <- new_layer(boundary)
  polygon$visible <- FALSE
  route <- new_dataset(sf::st_sf(name = "Route", geometry = sf::st_sfc(sf::st_linestring(ring[1:2, ]), crs = 4326)), "Route")
  polyline <- new_layer(route)
  project <- list(
    datasets = stats::setNames(list(stores, boundary, route), c(stores$id, boundary$id, route$id)),
    layers = stats::setNames(list(points, filtered_points, polygon, polyline),
      c(points$id, filtered_points$id, polygon$id, polyline$id)),
    map = default_map(),
    widgets = utils::modifyList(default_widgets(), list(scale_bar = TRUE, minimap = TRUE,
      measure = TRUE, mouse_coords = TRUE, reset_view = TRUE)),
    filters = list(only_a = list(id = "only_a", name = "Only A", dataset = stores$id,
      rules = list(list(column = "category", type = "categorical", values = "A", include_na = FALSE))))
  )
  stopifnot(is.null(filtered_points[["data"]]), nrow(layer_data(project, filtered_points)) == 1L,
    nrow(project$datasets[[stores$id]]$data) == 2L)
  methods <- vapply(build_leaflet_map(project)$x$calls, function(call) call$method, character(1))
  stopifnot(all(c("addCircleMarkers", "addMarkers", "addPolygons", "addPolylines",
    "addLayersControl", "addScaleBar", "addMiniMap", "addMeasure", "hideGroup") %in% methods))
  stopifnot(inherits(build_leaflet_map(list(datasets = list(), layers = list(),
    map = default_map(), widgets = default_widgets(), filters = list())), "leaflet"))
  stopifnot(!is.na(sf::st_crs(prepare_layer_data(boundary, polygon))))

  for (mode in c("numeric", "bin", "quantile")) {
    points$colour$mode <- mode
    points$colour$column <- "score"
    project$layers[[points$id]] <- points
    stopifnot(inherits(build_leaflet_map(project), "leaflet"))
  }
  points$colour$mode <- "custom"
  points$colour$column <- "category"
  points$colour$mapping <- "A = #ff0000\nB = #0000ff"
  stopifnot(identical(unname(map_colours(stores$data, points$colour)$colors), c("#ff0000", "#0000ff")))
  project$layers[[points$id]] <- points

  unsafe <- popup_html(data.frame(id = "<script>alert(1)</script>"), "<b>{id}</b>", "id")
  stopifnot(grepl("&lt;script&gt;", unsafe, fixed = TRUE), !grepl("<script>", unsafe, fixed = TRUE))
  points$popup$mode <- "simple"
  points$popup$title <- "id"
  points$popup$fields <- "category"
  points$popup$labels <- "category = Kind"
  stopifnot(grepl("<table>", popup_for_layer(stores$data, points, stores)[[1]], fixed = TRUE))
  rules <- list(list(column = "day", type = "date", min = "2026-09-24",
    max = "2026-09-26", include_na = TRUE))
  stopifnot(nrow(filter_dataset(data.frame(day = c("2026-09-25", "invalid", "2026-10-01")), rules)) == 2L)
  original <- data.frame(address = c("One Street", "Two Street"),
    latitude = c(-33, NA), longitude = c(151, NA))
  geocoded <- geocode_dataset_rows(original, "address", "latitude", "longitude", TRUE, "fixture-key",
    geocoder = function(addresses, key) lapply(addresses,
      function(address) c(latitude = -34, longitude = 150)))
  stopifnot(geocoded$attempted == 1L, geocoded$data$latitude[[2]] == -34,
    is.na(original$latitude[[2]]))

  html <- tempfile(fileext = ".html")
  render_map_html(project, html, selfcontained = TRUE)
  stopifnot(file.exists(html), file.info(html)$size > 1000)
  zipfile <- tempfile(fileext = ".zip")
  write_project_bundle(zipfile, project, helpers_dir = file.path(app_dir, "R"))
  entries <- utils::unzip(zipfile, list = TRUE)$Name
  stopifnot(all(c("project.rds", "render_map.R", "R/map_builder.R",
    paste0("data/", names(project$datasets), ".rds")) %in% entries))
  extracted <- tempfile("map-studio-unpacked-")
  dir.create(extracted)
  utils::unzip(zipfile, exdir = extracted)
  config <- readRDS(file.path(extracted, "project.rds"))
  stopifnot(is.null(config$datasets[[stores$id]]$data),
    inherits(readRDS(file.path(extracted, config$datasets[[boundary$id]]$file)), "sf"))
  stopifnot(inherits(try(make_renderer_script(project,
    custom_code = "map <- map # fixture-secret", secrets = "fixture-secret"), silent = TRUE), "try-error"))
  stopifnot(length(parse(file = file.path(extracted, "render_map.R"))) > 0)
  run_export <- function(path) {
    previous <- getwd()
    on.exit(setwd(previous), add = TRUE)
    setwd(path)
    sys.source("render_map.R", envir = new.env(parent = globalenv()))
    file.exists("map.html")
  }
  stopifnot(run_export(extracted))

  upload_file <- tempfile(fileext = ".csv")
  other_file <- tempfile(fileext = ".csv")
  utils::write.csv(stores$data, upload_file, row.names = FALSE)
  utils::write.csv(data.frame(id = "c1", latitude = -32, longitude = 152,
    category = "C", score = 6), other_file, row.names = FALSE)
  upload <- function(name, path) data.frame(name = name, datapath = path,
    size = file.info(path)$size, type = "text/csv")
  send_editor <- function(session, module, id, version, values) {
    names(values) <- paste0(module, "-", vapply(names(values),
      function(field) editor_input_key(id, version, field), character(1)))
    do.call(session$setInputs, values)
    session$flushReact()
  }
  suppressWarnings(shiny::testServer(server_fun, {
    session$setInputs(`datasets-upload_name` = "", `datasets-files` = upload("stores.csv", upload_file))
    session$flushReact()
    session$setInputs(`datasets-files` = upload("competitors.csv", other_file))
    session$flushReact()
    stopifnot(length(project$datasets) == 2L, length(project$layers) == 2L)
    ids <- names(project$layers)
    first <- ids[[1]]
    second <- ids[[2]]
    active_layer(first)
    initial <- list(editor_id = first, type = "circleMarker", radius = 6,
      stroke = TRUE, stroke_color = "", stroke_opacity = 1, fill_opacity = 0.8,
      cluster = FALSE, spiderfy = TRUE)
    send_editor(session, "markers", first, editor_revision(), initial)
    send_editor(session, "markers", first, editor_revision(), list(radius = 11))
    stopifnot(project$layers[[first]]$marker$radius == 11)
    active_layer(second)
    send_editor(session, "markers", second, editor_revision(), list(editor_id = second))
    stopifnot(project$layers[[second]]$marker$radius == 6)
    initial$editor_id <- second
    initial$radius <- 15
    send_editor(session, "markers", second, editor_revision(), initial)
    stopifnot(project$layers[[first]]$marker$radius == 11,
      project$layers[[second]]$marker$radius == 15)

    active_layer(first)
    version <- paste(editor_revision(), 0L, sep = "_")
    filter_initial <- list(editor_id = first, filter = "", new_name = "Only A",
      column = "category", type = "categorical", include_na = FALSE,
      create = 0, delete_filter = 0, add_rule = 0, remove_rule = 0, categories = "A")
    send_editor(session, "filters", first, version, filter_initial)
    send_editor(session, "filters", first, version, list(create = 1))
    filter_id <- project$layers[[first]]$filter
    stopifnot(nzchar(filter_id), length(project$filters) == 1L)
    version <- paste(editor_revision(), 1L, sep = "_")
    filter_initial$filter <- filter_id
    filter_initial$new_name <- ""
    filter_initial$create <- 0
    send_editor(session, "filters", first, version, filter_initial)
    send_editor(session, "filters", first, version, list(add_rule = 1))
    stopifnot(nrow(layer_data(project_list(project), project$layers[[first]])) == 1L,
      nrow(project$datasets[[project$layers[[first]]$dataset]]$data) == 2L)

    third <- new_layer(project$datasets[[project$layers[[first]]$dataset]],
      names(project$layers), "Third")
    existing <- project$layers
    existing[[third$id]] <- third
    project$layers <- existing
    active_layer(first)
    layer_form <- list(editor_id = first, name = project$layers[[first]]$name,
      source = project$layers[[first]]$dataset, type = project$layers[[first]]$type,
      visible = TRUE, group = project$layers[[first]]$group,
      duplicate = 0, up = 0, down = 0, delete = 0)
    send_editor(session, "layers", first, editor_revision(), layer_form)
    send_editor(session, "layers", first, editor_revision(), list(up = 1))
    stopifnot(identical(names(project$layers), c(second, first, third$id)))
    send_editor(session, "layers", first, editor_revision(), list(group = "New group"))
    stopifnot(identical(names(project$layers), c(second, first, third$id)))
    active_dataset(project$layers[[first]]$dataset)
    session$setInputs(`datasets-remove` = 1)
    session$flushReact()
    session$setInputs(`datasets-confirm_remove` = 1)
    session$flushReact()
    stopifnot(!first %in% names(project$layers), !third$id %in% names(project$layers),
      length(project$filters) == 0L)
  }))

  spatial <- boundary$data
  sf::st_crs(spatial) <- 4326
  geojson <- tempfile(fileext = ".geojson")
  gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(spatial, geojson, quiet = TRUE)
  sf::st_write(spatial, gpkg, quiet = TRUE)
  paths <- c(tempfile("shiny-upload-"), tempfile("shiny-upload-"))
  stopifnot(all(file.copy(c(geojson, gpkg), paths)))
  info <- data.frame(name = c("districts.geojson", "zones.gpkg"),
    datapath = paths, size = file.info(paths)$size, type = "application/octet-stream")
  shiny::testServer(server_fun, {
    session$setInputs(`datasets-upload_name` = "", `datasets-files` = info)
    session$flushReact()
    stopifnot(length(project$datasets) == 2L,
      all(vapply(project$datasets, function(ds) ds$type == "polygon", logical(1))),
      length(project$layers) == 2L)
  })
  invisible(TRUE)
}))
