

### Day 2, Section 2, Exercise 1

# Using the below example:
#  1. Convert app to a sidebar layout from the fluidPage with absolute panel
#  2. Change the linewidth of the tracks to a value of 4
#  3. Change the color palette variable from 'State' to 'Shape_Area', use the 'viridis' palette from {viridis}, and updated the legend title while showing it in decreasing order

library(dplyr)
library(readr)
library(purrr)
library(terra)
library(sf)
library(leaflet)  #v2.2.2 (CRAN)
library(leafem)  #v0.2.3.9006 (GitHub)
library(viridis)
library(shiny)
library(shinyWidgets)

source("../R_examples/leaflet_utils.R")  #for function addLegend_decreasing



#################
### Load data ###
#################

# Simulated tracks
tracks <- read_csv("../R_examples/data/Simulated tracks.csv")
tracks.sf <- tracks |>
  st_as_sf(coords = c('x','y'), crs = 4326)
tracks.sf2 <- tracks.sf |>
  group_by(id) |>
  summarize(do_union = FALSE) |>
  st_cast("MULTILINESTRING")

# Monthly SST (2021)
sst <- read_csv("../R_examples/data/Monthly_SST_2021.csv")
sst.rast <- sst |>
  split(~month) |>
  purrr::map(~rast(.x[,c('x','y','sst')], type = "xyz", crs = "EPSG:4326")) |>
  rast()

# Offshore wind leases
wind <- st_read("../R_examples/data/NE_Offshore_Wind.shp")
wind$State <- gsub(pattern = "Massachussets", "Massachusetts", wind$State)  #fix typo


# Define color palettes
tracks.pal <- colorFactor("Dark2", factor(tracks$id))
poly.pal <- colorFactor("Set3", factor(wind$State))

sst.range <- range(as.vector(values(sst.rast)), na.rm = TRUE)
rast.pal2 <- colorNumeric('magma',
                          domain = sst.range,
                          na.color = "transparent")


##########
### UI ###
##########

ui <- fluidPage(title = "Animal Movement, Offshore Wind Development, and SST",
                
                leafletOutput("mymap", width = "100%", height = "850px"),
                
                absolutePanel(class = "panel panel-default",
                              top = 300,
                              left = 25,
                              width = 250,
                              fixed = TRUE,
                              draggable = TRUE,
                              height = "auto",
                              
                              h3("Choose which layers to map"),
                              pickerInput(inputId = "tracks",
                                          label = "Select tracks",
                                          choices = unique(tracks$id),
                                          selected = unique(tracks$id),
                                          multiple = TRUE),
                              pickerInput(inputId = "polygons",
                                          label = "Select polygons by state",
                                          choices = unique(wind$State),
                                          selected = unique(wind$State),
                                          multiple = TRUE),
                              selectInput(inputId = "raster",
                                          label = "Select month of SST",
                                          choices = month.abb,
                                          selected = month.abb[1])
                              
                )  #close absolutePanel
                
)  #close fluidPage





##############
### Server ###
##############

server <- function(input, output, session) {
  
  #Create reactive objects based on selected input
  tracks.out <- reactive({
    tracks.sf2 |>
      filter(id %in% input$tracks)
  })
  
  wind.out <- reactive({
    wind |>
      filter(State %in% input$polygons)
  })
  
  sst.out <- reactive({
    sst.rast[[which(names(sst.rast) == input$raster)]]
  })
  
  
  
  
  ## Create map w/ non-reactive components
  output$mymap <- renderLeaflet({
    
    
    ## Static Leaflet basemap and widgets
    leaflet() |>
      setView(lng = -73, lat = 41.5, zoom = 6) |>
      addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Ocean Basemap",
                       options = providerTileOptions(zIndex = -10)) |>
      addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery",
                       options = providerTileOptions(zIndex = -10)) |>
      addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map",
                       options = providerTileOptions(zIndex = -10)) |>
      addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                       overlayGroups = c("SST", "Offshore Wind Leases", "Tracks"),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE),
                       position = "bottomleft") |>
      addScaleBar(position = "bottomright") |>
      addMeasure(position = "topleft",
                 primaryLengthUnit = "kilometers",
                 primaryAreaUnit = "hectares",
                 activeColor = "#3D535D",
                 completedColor = "#7D4479") |>
      addMouseCoordinates()
    
  })  #close renderLeaflet
  
  
  
  ## Add reactive elements to Leaflet map
  observe({
    
    leafletProxy(mapId = "mymap") |>
      clearMarkers() |>
      clearShapes() |>
      clearImages() |>
      clearControls() |>
      addRasterImage(x = sst.out(),
                     colors = rast.pal2,
                     opacity = 1,
                     group = "SST") |>
      addImageQuery(sst.out(), group = "SST") |>  #add raster  query
      addLegend_decreasing(pal = rast.pal2,
                           values = as.vector(values(sst.rast)),
                           title = "SST (\u00B0C)",
                           decreasing = TRUE) |>
      addPolygons(data = wind.out(),
                  color = ~poly.pal(State),
                  fillOpacity = 1,
                  stroke = FALSE,
                  label = ~paste0("State: ", State),
                  group = "Offshore Wind Leases") |>
      addLegend(pal = poly.pal,
                values = wind.out()$State,
                title = "State",
                opacity = 1) |>
      addPolylines(data = tracks.out(),
                   color = ~tracks.pal(id),
                   opacity = 0.75,
                   weight = 2,
                   label = ~paste0("ID: ", id),
                   group = "Tracks") |>
      addLegend(pal = tracks.pal,
                values = tracks.out()$id,
                title = "ID",
                position = "topleft")
    
  })  #close observe
  
}  #close server function





###############
### Run app ###
###############

shinyApp(ui = ui, server = server)
