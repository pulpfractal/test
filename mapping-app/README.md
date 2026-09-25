# Modular Mapping Studio

A Shiny application built with Shiny modules for uploading point data, geocoding with Azure Maps, styling Leaflet maps, and exporting a reproducible map project or standalone HTML.

## Run the app

From this directory in R:

```r
install.packages(c("shiny", "bslib", "leaflet", "htmlwidgets", "htmltools", "sf", "httr2", "zip"))
shiny::runApp()
```

The app expects a CSV with at least an identifier and decimal-degree coordinates, though coordinates may be added by geocoding. Select the latitude/longitude fields after upload. Optional polygon input accepts GeoJSON and GeoPackage (`.gpkg`) files. The app transforms uploaded polygons to WGS84 for display.

## Azure Maps batch geocoding

Provide an Azure Maps subscription key in the geocoding panel, or set `AZURE_MAPS_SUBSCRIPTION_KEY` before starting R. Choose one or more address fields in the order they should be joined. The app sends batches to the Azure Maps Geocoding Batch API (`2023-06-01`), waits for asynchronous jobs, and writes returned latitude/longitude values into the selected coordinate columns (or creates `latitude` and `longitude`). Geocoding is subject to Azure Maps access, quotas, and billing. The subscription key is not written to downloaded code, HTML, logs, or the export bundle.

## Map controls and exports

The preview supports standard or circle markers, marker clustering, single/category/numeric colours, a user-editable palette, popup HTML templates using `{column_name}` placeholders, hover labels, basemaps, legends, and polygon fill/outline styling. Inserted data values in popup templates are HTML-escaped; the template itself can contain HTML markup.

The export panel provides:

- **Standalone HTML**: a self-contained HTML widget file. Basemap tiles still require an internet connection.
- **Renderer script**: the R script used by the exported project; it expects the accompanying data and `R/map_helpers.R` files.
- **Project ZIP**: `render_map.R`, reusable map helper code, the uploaded CSV, and optional polygon GeoJSON. From the extracted directory run `Rscript render_map.R` to recreate `map.html`.

The advanced Leaflet R code field is appended to the exported renderer after `map` is created. For safety, this user-supplied code is not evaluated in the live Shiny preview. Review that code before running the exported script. Self-contained HTML packaging may also require Pandoc, depending on the local R/htmlwidgets installation.

## Modules

- `R/mod_upload.R`: point/polygon uploads, coordinate-column selection, and data preview.
- `R/mod_geocode.R`: Azure Maps batch geocoding and coordinate updates.
- `R/mod_controls.R`: marker, colour, popup, label, basemap, legend, and polygon controls.
- `R/map_helpers.R`: shared Leaflet map builder and HTML renderer.
- `R/mod_export.R`: generated renderer and reproducible ZIP bundle.
- `app.R`: app composition, preview, and download handlers.

## Notes

The app accepts CSV point data and GeoJSON/GeoPackage polygon files. It does not guess coordinate reference systems for point columns: point coordinates are assumed to be WGS84 decimal degrees. Polygon data without a CRS is assumed to be WGS84. For sensitive datasets, deploy the app in an appropriately secured environment and consider your Azure Maps data-processing requirements before geocoding.
