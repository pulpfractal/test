mod_geocode_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
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
      item <- items[[j]] %||% list()
      results <- item$response$results %||% item$response$features %||% list()
      position <- results[[1]]$position %||% NULL
      if (is.null(position) && !is.null(results[[1]]$geometry$coordinates)) {
        xy <- results[[1]]$geometry$coordinates
        position <- list(longitude = xy[[1]], lat = xy[[2]])
      }
      output[[indices[[j]]]] <- if (!is.null(position)) {
        c(latitude = as.numeric(position$lat), longitude = as.numeric(position$lon %||% position$longitude))
      } else c(latitude = NA_real_, longitude = NA_real_)
    }
  }
  output
}

mod_geocode_server <- function(id, data, columns) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    status_message <- shiny::reactiveVal("")

    shiny::observeEvent(data(), {
      result(NULL)
      cols <- if (is.null(data())) character() else names(data())
      common <- c("address", "street", "city", "state", "province", "postal_code", "zip", "country")
      shiny::updateSelectizeInput(session, "address_cols", choices = cols, selected = cols[tolower(cols) %in% common], server = TRUE)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$geocode, {
      df <- shiny::isolate(data())
      if (is.null(df)) {
        shiny::showNotification("Upload a CSV before geocoding.", type = "warning")
        return()
      }
      address_cols <- input$address_cols
      if (length(address_cols) == 0) {
        shiny::showNotification("Select at least one address field.", type = "warning")
        return()
      }
      key <- input$azure_key
      if (!nzchar(key)) key <- Sys.getenv("AZURE_MAPS_SUBSCRIPTION_KEY")
      if (!nzchar(key)) {
        shiny::showNotification("Enter an Azure Maps key or set AZURE_MAPS_SUBSCRIPTION_KEY.", type = "warning")
        return()
      }

      current_columns <- columns()
      lat_col <- current_columns$lat %||% ""
      lon_col <- current_columns$lon %||% ""
      has_coords <- nzchar(lat_col) && nzchar(lon_col) && lat_col %in% names(df) && lon_col %in% names(df)
      rows <- if (isTRUE(input$only_missing) && has_coords) {
        which(!is.finite(suppressWarnings(as.numeric(df[[lat_col]]))) | !is.finite(suppressWarnings(as.numeric(df[[lon_col]]))))
      } else seq_len(nrow(df))
      if (!length(rows)) {
        status_message("All records already have coordinates.")
        return()
      }

      addresses <- vapply(rows, function(i) {
        values <- vapply(address_cols, function(column) {
          value <- as.character(df[[column]][i])
          if (is.na(value)) "" else trimws(value)
        }, character(1))
        paste(values[nzchar(values)], collapse = ", ")
      }, character(1))
      keep <- nzchar(trimws(addresses))
      rows <- rows[keep]
      addresses <- addresses[keep]
      if (!length(rows)) {
        status_message("No selected rows contain an address.")
        return()
      }

      shiny::withProgress(message = "Geocoding addresses with Azure Maps", value = 0, {
        tryCatch({
          coordinates <- azure_geocode_batch(addresses, key)
          if (!nzchar(lat_col) || !lat_col %in% names(df)) lat_col <- "latitude"
          if (!nzchar(lon_col) || !lon_col %in% names(df)) lon_col <- "longitude"
          df[[lat_col]] <- if (lat_col %in% names(df)) df[[lat_col]] else rep(NA_real_, nrow(df))
          df[[lon_col]] <- if (lon_col %in% names(df)) df[[lon_col]] else rep(NA_real_, nrow(df))
          df[[lat_col]][rows] <- vapply(coordinates, `[[`, numeric(1), "latitude")
          df[[lon_col]][rows] <- vapply(coordinates, `[[`, numeric(1), "longitude")
          result(list(data = df, columns = modifyList(current_columns, list(lat = lat_col, lon = lon_col))))
          matched <- sum(is.finite(vapply(coordinates, `[[`, numeric(1), "latitude")))
          status_message(paste0("Geocoded ", matched, " of ", length(rows), " addresses."))
          shiny::showNotification(paste0("Azure Maps returned coordinates for ", matched, " of ", length(rows), " addresses."),
            type = if (matched == length(rows)) "message" else "warning")
        }, error = function(e) {
          status_message(conditionMessage(e))
          shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
        })
      })
    })

    output$status <- shiny::renderText(status_message())
    list(
      data = shiny::reactive(if (is.null(result())) data() else result()$data),
      columns = shiny::reactive(if (is.null(result())) columns() else result()$columns)
    )
  })
}
