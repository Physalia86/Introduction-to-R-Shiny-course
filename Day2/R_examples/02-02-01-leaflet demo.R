

## Learn to use {leaflet} outside of Shiny

library(tidyverse)
library(terra)
library(sf)
library(leaflet)  #v2.2.2 (CRAN)
library(leafem)  #v0.2.3.9006 (GitHub)
library(viridis)

source('Day2/R_examples/leaflet_utils.R')



#################
### Load data ###
#################

# Simulated tracks
tracks <- read_csv("Day2/R_examples/data/Simulated tracks.csv")
tracks.sf <- tracks |>
  st_as_sf(coords = c('x','y'), crs = 4326)

head(tracks)
head(tracks.sf)


# Monthly SST (2021)
sst <- read_csv("Day2/R_examples/data/Monthly_SST_2021.csv")
sst.rast <- sst |>
  split(~month) |>
  purrr::map(~rast(.x[,c('x','y','sst')], type = "xyz", crs = "EPSG:4326")) |>
  rast()

# Offshore wind leases
wind <- st_read("Day2/R_examples/data/NE_Offshore_Wind.shp")
unique(wind$State)
wind$State <- gsub(pattern = "Massachussets", "Massachusetts", wind$State)  #fix typo






## Example 1: Create basemap

print(providers)  #many different basemap tiles available; doesn't include other WMS

# Ocean Basemap
leaflet() |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  setView(lng = -73, lat = 41.5, zoom = 6)

# Satellite Imagery Basemap
leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  setView(lng = -73, lat = 41.5, zoom = 6)

# OSM Basemap
leaflet() |>
  addProviderTiles(providers$OpenStreetMap) |>
  setView(lng = -73, lat = 41.5, zoom = 6)




## Example 2: Add spatial features

############
## Points ##
############

# Add points from data.frame object w/ default settings
leaflet(tracks) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addCircleMarkers(lng = ~x,
                   lat = ~y)


# Add points from sf object w/ default settings
leaflet(tracks.sf) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addCircleMarkers()


# Add points from data.frame object w/ customized size and opacity
leaflet(tracks) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 1,
                   opacity = 0.5)


# Add points from data.frame object w/ customized colors, labels, and legend
tracks.pal <- colorFactor("Dark2", factor(tracks$id))

leaflet(tracks) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 1,
                   opacity = 0.5,
                   color = ~tracks.pal(id),
                   label = ~paste0("ID: ", id)) |>
  addLegend(pal = tracks.pal,
            values = ~id,
            title = "ID")


# Add popups instead of labels (need to click instead of hover)
leaflet(tracks) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 1,
                   opacity = 0.5,
                   color = ~tracks.pal(id),
                   popup = ~paste0("ID: ", id,
                                   "<br> Long: ", x,
                                   "<br> Lat: ", y)) |>
  addLegend(pal = tracks.pal,
            values = ~id,
            title = "ID")




###########
## Lines ##
###########

tracks.pal2 <- RColorBrewer::brewer.pal(n_distinct(tracks$id), "Dark2")

# From data.frame object
new.map <- leaflet() |>
  addProviderTiles(providers$Esri.OceanBasemap)

for (i in 1:n_distinct(tracks$id)){
  new.map <- new.map |>
    addPolylines(data = tracks[tracks$id == unique(tracks$id)[i],],
                 lng = ~x,
                 lat = ~y,
                 color = tracks.pal2[i],
                 opacity = 0.75,
                 weight = 2,
                 label = ~paste0("ID: ", id))
}

new.map |>
  addLegend(pal = tracks.pal,
            values = tracks$id,
            title = "ID")


# From sf object
tracks.sf2 <- tracks.sf |>
  group_by(id) |>
  summarize(do_union = FALSE) |>
  st_cast("MULTILINESTRING")

leaflet(tracks.sf2) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addPolylines(color = ~tracks.pal(id),
               opacity = 0.75,
               weight = 2,
               label = ~paste0("ID: ", id)) |>
  addLegend(pal = tracks.pal,
            values = ~id,
            title = "ID")




##############
## Polygons ##
##############

# Add polygon sf object and customize settings
poly.pal <- colorFactor("Set3", factor(wind$State))

leaflet(wind) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addPolygons(color = ~poly.pal(State),
              fillOpacity = 1,
              stroke = FALSE,
              label = ~paste0("State: ", State)) |>
  addLegend(pal = poly.pal,
            values = ~State,
            title = "State",
            opacity = 1)




############
## Raster ##
############

# Add raster layer
leaflet() |>
  addProviderTiles(provider = providers$Esri.OceanBasemap) |>
  addRasterImage(x = sst.rast$Jan)


# Add legend to raster layer
rast.pal <- colorNumeric('magma',
                    domain = values(sst.rast$Jan),
                    na.color = "transparent")

leaflet() |>
  addProviderTiles(provider = providers$Esri.OceanBasemap) |>
  addRasterImage(x = sst.rast$Jan,
                 colors = rast.pal,
                 opacity = 0.8) |>
  addLegend(pal = rast.pal,
            values = values(sst.rast$Jan),
            title = "SST (\u00B0C)")


# Reversed legend (and palette) for standard ordering
leaflet() |>
  addProviderTiles(provider = providers$Esri.OceanBasemap) |>
  addRasterImage(x = sst.rast$Jan,
                 colors = rast.pal,
                 opacity = 0.8) |>
  addLegend_decreasing(pal = rast.pal,
                       values = values(sst.rast$Jan),
                       title = "SST (\u00B0C)",
                       decreasing = TRUE)






## Example 3: Add multiple basemaps, scale_bar, other widgets


# Define palette for all monthly SST values
sst.range <- range(as.vector(values(sst.rast)), na.rm = TRUE)
rast.pal2 <- colorNumeric('magma',
                          domain = sst.range,
                          na.color = "transparent")


# Add all layers w/ ability to hide, as well as raster querying and mouse coordinates

leaflet() |>
  addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Ocean Basemap",
                   options = providerTileOptions(zIndex = -10)) |>
  addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery",
                   options = providerTileOptions(zIndex = -10)) |>
  addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map",
                   options = providerTileOptions(zIndex = -10)) |>
  addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                   overlayGroups = c("Feb SST", "Aug SST", "Offshore Wind Leases", "Tracks"),
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE),
                   position = "bottomleft") |>
  addRasterImage(x = sst.rast$Feb,
                 colors = rast.pal2,
                 opacity = 1,
                 group = "Feb SST") |>
  addImageQuery(sst.rast$Feb, group = "Feb SST") |>  #add raster query
  addRasterImage(x = sst.rast$Aug,
                 colors = rast.pal2,
                 opacity = 1,
                 group = "Aug SST") |>
  addImageQuery(sst.rast$Aug, group = "Aug SST") |>  #add raster query
  addLegend_decreasing(pal = rast.pal2,
                       values = as.vector(values(sst.rast)),
                       title = "SST (\u00B0C)",
                       decreasing = TRUE) |>
  addPolygons(data = wind,
              color = ~poly.pal(State),
              fillOpacity = 1,
              stroke = FALSE,
              label = ~paste0("State: ", State),
              group = "Offshore Wind Leases") |>
  addLegend(pal = poly.pal,
            values = wind$State,
            title = "State",
            opacity = 1) |>
  addPolylines(data = tracks.sf2,
               color = ~tracks.pal(id),
               opacity = 0.75,
               weight = 2,
               label = ~paste0("ID: ", id),
               group = "Tracks") |>
  addLegend(pal = tracks.pal,
            values = tracks.sf2$id,
            title = "ID",
            position = "topleft") |>
  addScaleBar(position = "bottomright") |>
  addMeasure(position = "topleft",
             primaryLengthUnit = "kilometers",
             primaryAreaUnit = "hectares",
             activeColor = "#3D535D",
             completedColor = "#7D4479") |>
  addMouseCoordinates()


