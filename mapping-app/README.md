# Leaflet Map Studio

A modular Shiny app for composing Leaflet maps from multiple CSV and `sf` datasets. Dataset records hold data and coordinate/ID metadata; layers reference those records and hold independent styling, visibility, group, popup, label, legend and filter settings. The preview and exports share the same map builder.

## Run

From the `mapping-app` directory in R:

```r
install.packages(c("shiny", "bslib", "leaflet", "htmlwidgets", "htmltools", "sf", "httr2", "zip"))
shiny::runApp()
```

Optional search and fullscreen buttons use `leaflet.extras` (`install.packages("leaflet.extras")`). The other map controls work with base `leaflet`. Spatial file support requires an `sf`/GDAL installation capable of reading your file formats. The app accepts uploads up to 100 MB per request.

Run `source("tests.R")` from this directory to exercise the project model, module state, spatial uploads and exports. The smoke suite makes no Azure requests and requires Pandoc to test self-contained HTML packaging.

## Make a map

1. In **Data**, add one or more CSV, GeoJSON or GeoPackage files. Each upload creates a default layer without replacing earlier datasets. Give each dataset a friendly name, inspect the first six rows, set its ID column and, for CSV points, choose WGS84 latitude/longitude columns. Common coordinate names are preselected when present. Spatial `sf` point, polygon and line geometries are recognized automatically; a GeoPackage upload reads its first spatial layer. Spatial data without a CRS is assumed to be WGS84 for display.
2. Choose an **Active Layer** at the top of the sidebar. In **Layers**, create additional layers from any uploaded dataset, rename, duplicate, delete, show/hide, change the source, assign a group, or move them up/down. Moving layers affects path drawing order (Leaflet's marker pane still displays markers above polygon paths). Removing a dataset also removes its layers and filters.
3. Use **Markers**, **Colours**, **Polygons**, **Popups**, **Labels** and **Legends** to style the active layer. Settings stay with that layer when you switch selections. Circle markers, standard pins, polygon fills and polylines can all be coloured separately. Polygon/line colours are set in **Colours**; opacity, outline, dash and hover styling are in **Polygons**. Colours support single, categorical, continuous numeric, bins, quantiles and `Category = #hex` custom mappings, with an on-screen palette preview. If a dataset value is missing or unmapped, the NA colour is used.
4. In **Filters**, create a named filter set for a dataset, add categorical, numeric, logical or date-range rules (combined with AND), and assign it to one or more layers of that dataset. Rules never alter the uploaded data. CSV date strings in `YYYY-MM-DD` format can be filtered as dates. In **Widgets**, toggle layer control, scale, minimap, measurement, reset view, coordinates and optional search/fullscreen. **Map** controls basemap and initial fit bounds or center/zoom.

The layer control gives each layer an independently toggleable entry displaying its group, name and ID. A legend may be attached to each layer; multiple legends can coexist. The search widget indexes a layer's label field when set, otherwise its ID column; enable search by installing `leaflet.extras`.

## Azure Maps geocoding

Open the Azure panel in **Data**, select a tabular point dataset and address fields, and enter a subscription key (or set the `AZURE_MAPS_SUBSCRIPTION_KEY` environment variable). Geocoding updates only that dataset's coordinates; its existing layers use the updated data. The **only missing** option avoids overwriting valid coordinates. The app uses the Azure Maps Geocoding Batch API `2023-06-01`, including polling for asynchronous jobs. Calls may use your quota and block a Shiny session while the batch finishes. Subscription keys are not placed in the project configuration, downloaded files or generated code.

## Exports

**Standalone HTML** saves a self-contained widget built by the same renderer as the preview. External basemap tiles still need internet access; HTML packaging may need Pandoc. Advanced Leaflet R code entered in **Export** is included only in the generated renderer, not run in the preview/HTML export. Do not put credentials in advanced code; exporting rejects the active Azure key if found there.

**Renderer script** is `render_map.R`; download the **project ZIP** for the required `project.rds`, `data/*.rds` and `R/` helper files. Run `Rscript render_map.R` from the extracted folder to rebuild `map.html`. `project.rds` stores dataset metadata, layers, map settings, widgets and filter configurations without duplicating the dataset contents. The `data/*.rds` files preserve CSV-derived column types and spatial `sf` geometries/CRSs. The ZIP is a reproducible renderer project, not a copy of the Shiny app. Neither Azure keys nor the geocoding module are included. If search/fullscreen was selected, install `leaflet.extras` in the renderer's R environment to reproduce those optional widgets.

## Code structure

`app.R` composes the twelve editor tabs and owns the project reactive state. `R/project_model.R` defines dataset/layer defaults, IDs and reusable filtering. The `R/mod_*.R` files edit configuration; `R/mod_geocode.R` retains the Azure batch client, and `R/mod_export.R` writes the ZIP/script. `R/map_builder.R` creates the base map, bounds, legends and controls; `R/map_layers.R` renders each layer type; `R/map_palettes.R` and `R/map_popups.R` share safe colour/popup logic. `R/map_helpers.R` saves the same widget to HTML. Rendering helpers are free of Shiny dependencies and are copied into exported projects.

Point coordinates must be decimal-degree WGS84. Invalid or out-of-range point coordinates are not drawn, but remain in the uploaded dataset. Upload and export data only in an appropriately secured environment, and review Azure Maps data-processing and billing requirements before geocoding.
