

### Day 2, Section 3, Exercise 1

# Using the simulated data below:
#  1. Create a navbar page with title 'Freshwater Species Occurrence' and that uses the 'yeti' Bootswatch theme
#  2. Create a page using nav_panel entitled "Data summary" that uses a sidebar layout, where the sidebar has the title "Controls"
#  3. In the sidebar, add dropdown inputs for species and year of `occ_dat` where only a single species can be selected at a time, but multiple years can be selected
#  4. Add a leaflet map to the main panel of this page, where the two input widgets control which circleMarkers are displayed (by year and species); circleMarkers should be colored by species using the 'Dark2' palette; add a legend

library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(bslib)
library(rnaturalearth)
# install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
library(rnaturalearthhires)
library(sf)


##########################################
### Simulate sample data and locations ###
##########################################

# Store simple spatial layer for sampling locations
ca <- ne_states(country = "Canada", returnclass = 'sf') |> 
  dplyr::select(name) |> 
  rename(province = name)

# Generate 1000 samples across Canada
set.seed(123)
n_samples <- 1000

# Define data/metadata of spp occurrence
occ_dat <- data.frame(year = sample(2020:2024, size = n_samples, replace = TRUE),
                      spp = sample(LETTERS[1:8], size = n_samples, replace = TRUE),
                      obs = rbinom(n = n_samples, size = 1, prob = 0.4),
                      geometry = st_sample(ca, n_samples)) |> 
  st_sf()

# Join with `ca` to include provinces in occ_dat
occ_dat <- st_intersection(occ_dat, ca)




##########
### UI ###
##########

ui <- page_navbar(
  title = "Freshwater Species Occurrence",
  theme = bs_theme(bootswatch = "yeti"),
  
  nav_panel(
    title = "Data summary",
    
    layout_sidebar(
      # Sidebar content
      sidebar = sidebar(
        title = "Controls",
        selectInput("spp",
                    strong("Select a species:"),
                    choices = sort(unique(occ_dat$spp)),
                    selected = 'A',
                    multiple = FALSE),
        selectInput("years",
                    strong("Select year(s):"),
                    choices = 2020:2024,
                    selected = 2020:2024,
                    multiple = TRUE)
      ),
      
      leafletOutput("map")
    )
  )
)





##############
### Server ###
##############

server <- function(input, output, session) {
  
  ### Map ###
  
  # Create color palette for spp
  spp_pal <- colorFactor("Dark2", domain = factor(sort(unique(occ_dat$spp))))
  
  # Create reactive object by species and year(s) selected
  occ_filt <- reactive({
    occ_dat |> 
      filter(spp == input$spp,
             year %in% input$years)
  })
  
  
  # Create leaflet basemap
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = mean(st_coordinates(occ_dat)[,1]), 
              lat = mean(st_coordinates(occ_dat)[,2]), 
              zoom = 3)
  })
  
  
  # Render reactive map
  observe({
    
    req(occ_filt())  #to make sure data available before updating reactive map components
    
    # Add reactive elements
    leafletProxy("map", data = occ_filt()) |>
      clearMarkers() |>
      clearControls() |> 
      addCircleMarkers(radius = 5,
                       weight = 2,
                       opacity = 0.7,
                       color = ~spp_pal(spp),
                       fillColor = ~spp_pal(spp),
                       stroke = TRUE) |>
      addLegend(pal = spp_pal,
                values = ~spp,
                title = "Species")
  })
  
}





###############
### Run app ###
###############

shinyApp(ui = ui, server = server)
