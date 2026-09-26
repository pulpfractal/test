valid_colour <- function(value, fallback = "#BDBDBD") {
  if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value) ||
      inherits(try(grDevices::col2rgb(value), silent = TRUE), "try-error")) fallback else value
}

parse_palette <- function(value, fallback = c("#2C7FB8", "#7FCDBB", "#EDF8B1")) {
  if (is.null(value) || !length(value) || is.na(value[[1]])) return(fallback)
  colours <- trimws(strsplit(value[[1]], ",", fixed = TRUE)[[1]])
  colours <- colours[nzchar(colours)]
  if (length(colours) < 2L || any(vapply(colours, function(x) {
    inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
  }, logical(1)))) fallback else colours
}

palette_colours <- function(config, n = 5L) {
  n <- as.integer(valid_number(n, 5L, 1, 256))
  name <- config$palette
  if (is.null(name) || !name %in% c("Viridis", "Blues 3", "Reds 3", "Spectral", "Set 2", "Custom")) name <- "Viridis"
  if (name == "Custom") {
    colours <- grDevices::colorRampPalette(parse_palette(config$custom_palette))(n)
    if (isTRUE(config$reverse)) rev(colours) else colours
  } else grDevices::hcl.colors(n, palette = name, rev = isTRUE(config$reverse))
}

category_mapping <- function(value) {
  if (is.null(value) || !nzchar(value)) return(character())
  lines <- strsplit(value, "\n", fixed = TRUE)[[1]]
  result <- character()
  for (line in lines) {
    at <- regexpr("=", line, fixed = TRUE)[[1]]
    if (at < 2L) next
    category <- trimws(substr(line, 1L, at - 1L))
    colour <- trimws(substr(line, at + 1L, nchar(line)))
    if (nzchar(category) && identical(valid_colour(colour, ""), colour)) result[category] <- colour
  }
  result
}

map_colours <- function(data, config) {
  mode <- config$mode
  if (is.null(mode) || !mode %in% c("factor", "numeric", "bin", "quantile", "custom")) mode <- "single"
  column <- config$column
  single <- valid_colour(config$single, "#2C7FB8")
  na_colour <- valid_colour(config$na_colour, "#BDBDBD")
  empty <- list(colors = rep(single, nrow(data)), pal = NULL,
    values = NULL, mode = "single", mapping = character(), na_colour = na_colour)
  if (mode == "single" || is.null(column) || !column %in% names(data)) return(empty)
  values <- data[[column]]
  n <- as.integer(valid_number(config$n, 5, 2, 24))
  if (mode == "custom") {
    mapping <- category_mapping(config$mapping)
    colors <- unname(mapping[as.character(values)])
    colors[is.na(colors)] <- na_colour
    return(list(colors = colors, pal = NULL, values = values,
      mode = mode, mapping = mapping, na_colour = na_colour))
  }
  if (mode == "factor") {
    values <- as.character(values)
    nonmissing <- unique(values[!is.na(values)])
    if (!length(nonmissing)) return(empty)
    pal <- leaflet::colorFactor(palette_colours(config, max(n, length(nonmissing))),
      domain = values, na.color = na_colour)
  } else {
    values <- suppressWarnings(as.numeric(values))
    finite <- values[is.finite(values)]
    if (!length(finite)) return(empty)
    values[!is.finite(values)] <- NA_real_
    if (mode %in% c("bin", "quantile") && length(unique(finite)) < 2L) mode <- "numeric"
    if (mode == "quantile") {
      # Ties can make requested quantile breaks identical; reduce bins or use a gradient.
      candidates <- seq.int(n, 2L)
      valid <- vapply(candidates, function(bins) {
        cuts <- stats::quantile(finite, probs = seq(0, 1, length.out = bins + 1L))
        all(diff(cuts) > 0)
      }, logical(1))
      if (any(valid)) n <- candidates[[which(valid)[[1]]]] else mode <- "numeric"
    }
    palette <- palette_colours(config, n)
    pal <- switch(mode,
      numeric = leaflet::colorNumeric(palette, domain = values, na.color = na_colour),
      bin = leaflet::colorBin(palette, domain = values, bins = n, pretty = FALSE, na.color = na_colour),
      quantile = leaflet::colorQuantile(palette, domain = values, n = n, na.color = na_colour))
  }
  list(colors = pal(values), pal = pal, values = values,
    mode = mode, mapping = character(), na_colour = na_colour)
}
