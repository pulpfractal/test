mod_geocode_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(ns("dataset"), "Point dataset to geocode", choices = character()),
    shiny::passwordInput(ns("azure_key"), "Azure Maps subscription key", value = ""),
    shiny::helpText("Alternatively set AZURE_MAPS_SUBSCRIPTION_KEY in the app environment."),
    shiny::selectizeInput(ns("address_cols"), "Address columns (join in order)", choices = character(), multiple = TRUE,
      options = list(plugins = list("remove_button"))),
    shiny::checkboxInput(ns("only_missing"), "Geocode rows without coordinates only", value = TRUE),
    shiny::actionButton(ns("geocode"), "Geocode with Azure Maps", class = "btn-primary"),
    shiny::textOutput(ns("status"))
  )
}

azure_geocode_batch <- function(addresses, key, batch_size = 10000L, max_polls = 60L) {
  endpoint <- "https://atlas.microsoft.com/geocode:batch?api-version=2023-06-01"
  chunks <- split(seq_along(addresses), ceiling(seq_along(addresses) / batch_size))
  output <- vector("list", length(addresses))

  for (indices in chunks) {
    body <- list(batchItems = lapply(addresses[indices], function(address) list(query = address)))
    url <- paste0(endpoint, "&subscription-key=", utils::URLencode(key, reserved = TRUE))
    response <- httr2::request(url) |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)
    if (status >= 400) stop("Azure Maps returned HTTP ", status, ". Check the key, quota, and request.", call. = FALSE)

    if (status == 202) {
      headers <- httr2::resp_headers(response)
      status_url <- headers[["operation-location"]] %||% headers[["Operation-Location"]]
      if (is.null(status_url)) stop("Azure Maps accepted the batch but did not return an operation location.", call. = FALSE)
      complete <- FALSE
      for (poll in seq_len(max_polls)) {
        Sys.sleep(2)
        poll_url <- if (grepl("subscription-key=", status_url, fixed = TRUE)) status_url else paste0(status_url,
          if (grepl("\\?", status_url)) "&" else "?", "subscription-key=", utils::URLencode(key, reserved = TRUE))
        result_response <- httr2::request(poll_url) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()
        if (httr2::resp_status(result_response) >= 400) stop("Azure Maps batch polling failed with HTTP ", httr2::resp_status(result_response), ".", call. = FALSE)
        payload <- httr2::resp_body_json(result_response, simplifyVector = FALSE)
        state <- tolower(payload$status %||% "")
        if (state %in% c("succeeded", "completed", "complete")) {
          complete <- TRUE
          break
        }
        if (state %in% c("failed", "canceled", "cancelled")) stop("Azure Maps batch operation ", state, ".", call. = FALSE)
      }
      if (!complete) stop("Azure Maps batch operation did not finish before the polling limit.", call. = FALSE)
    } else {
      payload <- httr2::resp_body_json(response, simplifyVector = FALSE)
    }

    items <- payload$batchItems %||% list()
    for (j in seq_along(indices)) {
      item <- if (j <= length(items)) items[[j]] else list()
      results <- item$response$results %||% item$response$features %||% list()
      first <- if (length(results)) results[[1]] else list()
      position <- first$position %||% NULL
      if (is.null(position) && !is.null(first$geometry$coordinates)) {
        xy <- first$geometry$coordinates
        position <- list(longitude = xy[[1]], lat = xy[[2]])
      }
      output[[indices[[j]]]] <- if (!is.null(position)) {
        c(latitude = as.numeric(position$lat), longitude = as.numeric(position$lon %||% position$longitude))
      } else c(latitude = NA_real_, longitude = NA_real_)
    }
  }
  output
}

# A separate, testable data operation; the subscription key is never stored on a project.
geocode_dataset_rows <- function(data, address_cols, lat_col, lon_col, only_missing,
                                 key, geocoder = azure_geocode_batch) {
  if (!length(address_cols) || !all(address_cols %in% names(data))) {
    stop("Select at least one valid address column.", call. = FALSE)
  }
  valid_coords <- nzchar(lat_col) && nzchar(lon_col) &&
    lat_col %in% names(data) && lon_col %in% names(data) && lat_col != lon_col
  if (valid_coords) {
    lat <- suppressWarnings(as.numeric(data[[lat_col]]))
    lon <- suppressWarnings(as.numeric(data[[lon_col]]))
    good <- is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180
  } else good <- rep(FALSE, nrow(data))
  rows <- if (isTRUE(only_missing)) which(!good) else seq_len(nrow(data))
  if (!length(rows)) return(list(data = data, lat = lat_col, lon = lon_col, attempted = 0L, matched = 0L))

  addresses <- vapply(rows, function(i) {
    values <- vapply(address_cols, function(column) {
      value <- as.character(data[[column]][i])
      if (is.na(value)) "" else trimws(value)
    }, character(1))
    paste(values[nzchar(values)], collapse = ", ")
  }, character(1))
  keep <- nzchar(addresses)
  rows <- rows[keep]
  addresses <- addresses[keep]
  if (!length(rows)) return(list(data = data, lat = lat_col, lon = lon_col, attempted = 0L, matched = 0L))

  coordinates <- geocoder(addresses, key)
  if (length(coordinates) != length(rows)) stop("Azure Maps returned an incomplete batch.", call. = FALSE)
  if (!nzchar(lat_col) || !lat_col %in% names(data)) lat_col <- "latitude"
  if (!nzchar(lon_col) || !lon_col %in% names(data)) lon_col <- "longitude"
  if (identical(lat_col, lon_col)) stop("Latitude and longitude columns must differ.", call. = FALSE)
  if (!lat_col %in% names(data)) data[[lat_col]] <- rep(NA_real_, nrow(data))
  if (!lon_col %in% names(data)) data[[lon_col]] <- rep(NA_real_, nrow(data))
  data[[lat_col]][rows] <- vapply(coordinates, `[[`, numeric(1), "latitude")
  data[[lon_col]][rows] <- vapply(coordinates, `[[`, numeric(1), "longitude")
  list(data = data, lat = lat_col, lon = lon_col, attempted = length(rows),
    matched = sum(is.finite(vapply(coordinates, `[[`, numeric(1), "latitude"))))
}

mod_geocode_server <- function(id, project, active_dataset, touch_dataset, editor_revision) {
  shiny::moduleServer(id, function(input, output, session) {
    status_message <- shiny::reactiveVal("")

    shiny::observeEvent(project$datasets, {
      available <- project$datasets[vapply(project$datasets, function(dataset) {
        dataset$type == "point" && !inherits(dataset$data, "sf")
      }, logical(1))]
      ids <- names(available)
      if (is.null(ids)) ids <- character()
      choices <- stats::setNames(ids, vapply(available, function(x) x$name, character(1)))
      chosen <- input$dataset
      if (is.null(chosen) || !chosen %in% names(available)) {
        chosen <- if (active_dataset() %in% names(available)) active_dataset() else if (length(available)) names(available)[[1]] else ""
      }
      shiny::updateSelectInput(session, "dataset", choices = choices, selected = chosen)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$dataset, {
      dataset <- project$datasets[[input$dataset]]
      columns <- field_names(dataset)
      common <- c("address", "street", "city", "state", "province", "postal_code", "zip", "country")
      selected <- intersect(input$address_cols, columns)
      if (!length(selected)) selected <- columns[tolower(columns) %in% common]
      shiny::updateSelectizeInput(session, "address_cols", choices = columns, selected = selected, server = TRUE)
      status_message("")
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$geocode, {
      id <- input$dataset
      dataset <- project$datasets[[id]]
      if (is.null(dataset) || dataset$type != "point" || inherits(dataset$data, "sf")) {
        shiny::showNotification("Choose a tabular point dataset to geocode.", type = "warning")
        return()
      }
      key <- input$azure_key
      if (is.null(key) || !nzchar(key)) key <- Sys.getenv("AZURE_MAPS_SUBSCRIPTION_KEY")
      if (!nzchar(key)) {
        shiny::showNotification("Enter an Azure Maps key or set AZURE_MAPS_SUBSCRIPTION_KEY.", type = "warning")
        return()
      }
      shiny::withProgress(message = "Geocoding addresses with Azure Maps", value = 0, {
        tryCatch({
          result <- geocode_dataset_rows(dataset$data, input$address_cols,
            dataset$lat, dataset$lon, input$only_missing, key)
          if (result$attempted) {
            dataset$data <- result$data
            dataset$lat <- result$lat
            dataset$lon <- result$lon
            datasets <- project$datasets
            datasets[[id]] <- dataset
            project$datasets <- datasets
            touch_dataset()
            editor_revision(editor_revision() + 1L)
          }
          shiny::incProgress(1)
          status_message(if (result$attempted) {
            paste("Geocoded", result$matched, "of", result$attempted, "addresses in", dataset$name)
          } else "No selected records need geocoding (or none contain an address).")
          shiny::showNotification(status_message(),
            type = if (result$matched == result$attempted) "message" else "warning")
        }, error = function(e) {
          message <- conditionMessage(e)
          for (secret in unique(c(key, utils::URLencode(key, reserved = TRUE)))) {
            if (nzchar(secret)) message <- gsub(secret, "[REDACTED]", message, fixed = TRUE)
          }
          status_message(message)
          shiny::showNotification(message, type = "error", duration = NULL)
        })
      })
    })

    output$status <- shiny::renderText(status_message())
    list(key = shiny::reactive(input$azure_key))
  })
}
