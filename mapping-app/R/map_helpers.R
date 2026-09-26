# Shared entry point for preview and downloads. All map assembly lives in map_builder.R.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) y else x
}

render_map_html <- function(project, file, selfcontained = TRUE) {
  htmlwidgets::saveWidget(build_leaflet_map(project), file = file,
    selfcontained = selfcontained)
  invisible(file)
}
