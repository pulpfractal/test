map_records <- function(data) {
  if (inherits(data, "sf")) sf::st_drop_geometry(data) else data
}

popup_value <- function(value) {
  if (!length(value) || all(is.na(value))) return("")
  as.character(htmltools::htmlEscape(paste(as.character(value), collapse = ", "), attribute = TRUE))
}

# Template markup is trusted; every substituted dataset value is escaped.
popup_html <- function(data, template, id_col) {
  records <- map_records(data)
  cols <- names(records)
  if (is.null(template) || !nzchar(template)) template <- "<b>{id}</b>"
  if (is.null(id_col) || !id_col %in% cols) id_col <- if (length(cols)) cols[[1]] else ""
  matches <- gregexpr("\\{[^{}]+\\}", template, perl = TRUE)[[1]]
  if (matches[[1]] < 0L) return(rep(template, nrow(records)))
  lengths <- attr(matches, "match.length")
  vapply(seq_len(nrow(records)), function(i) {
    result <- character()
    cursor <- 1L
    for (j in seq_along(matches)) {
      start <- matches[[j]]
      result <- c(result, substr(template, cursor, start - 1L))
      key <- substr(template, start + 1L, start + lengths[[j]] - 2L)
      col <- if (identical(key, "id")) id_col else key
      result <- c(result, if (col %in% cols) popup_value(records[[col]][i]) else
        substr(template, start, start + lengths[[j]] - 1L))
      cursor <- start + lengths[[j]]
    }
    paste0(c(result, substr(template, cursor, nchar(template))), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

popup_field_labels <- function(text, fields) {
  labels <- stats::setNames(fields, fields)
  if (is.null(text) || !nzchar(text)) return(labels)
  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (!nzchar(line)) next
    at <- regexpr("=", line, fixed = TRUE)[[1]]
    if (at > 1L) {
      key <- trimws(substr(line, 1L, at - 1L))
      if (key %in% fields) labels[[key]] <- trimws(substr(line, at + 1L, nchar(line)))
    } else if (i <= length(fields)) labels[[fields[[i]]]] <- line
  }
  labels
}

popup_for_layer <- function(data, layer, dataset) {
  config <- layer$popup
  if (!isTRUE(config$enabled)) return(NULL)
  records <- map_records(data)
  if (identical(config$mode, "html")) return(popup_html(records, config$template, dataset$id_col))
  fields <- intersect(config$fields, names(records))
  labels <- popup_field_labels(config$labels, fields)
  title_col <- if (config$title %in% names(records)) config$title else ""
  if (!length(fields) && !nzchar(title_col) && dataset$id_col %in% names(records)) title_col <- dataset$id_col
  layout <- if (config$layout %in% c("table", "stacked", "compact")) config$layout else "table"
  vapply(seq_len(nrow(records)), function(i) {
    title <- if (nzchar(title_col)) paste0("<strong>", popup_value(records[[title_col]][i]), "</strong>") else ""
    entries <- vapply(fields, function(col) {
      name <- popup_value(labels[[col]])
      value <- popup_value(records[[col]][i])
      switch(layout,
        table = paste0("<tr><th>", name, "</th><td>", value, "</td></tr>"),
        stacked = paste0("<div><strong>", name, "</strong><br>", value, "</div>"),
        compact = paste0("<div>", name, ": ", value, "</div>"))
    }, character(1))
    contents <- if (layout == "table" && length(entries)) paste0("<table>", paste(entries, collapse = ""), "</table>") else
      paste(entries, collapse = "")
    paste0(title, contents)
  }, character(1), USE.NAMES = FALSE)
}

labels_for_layer <- function(data, layer) {
  config <- layer$label
  records <- map_records(data)
  if (!isTRUE(config$enabled) || is.null(config$column) || !config$column %in% names(records)) return(NULL)
  lapply(records[[config$column]], function(value) htmltools::HTML(popup_value(value)))
}

label_options_for_layer <- function(layer) {
  config <- layer$label
  if (!isTRUE(config$enabled)) return(NULL)
  weight <- if (config$style %in% c("bold", "bolditalic")) "bold" else "normal"
  font_style <- if (config$style %in% c("italic", "bolditalic")) "italic" else "normal"
  leaflet::labelOptions(permanent = isTRUE(config$permanent),
    direction = config$direction, opacity = valid_number(config$opacity, 1, 0, 1),
    textsize = paste0(valid_number(config$size, 12, 1, 72), "px"),
    style = list("font-weight" = weight, "font-style" = font_style))
}
