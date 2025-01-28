

### Day 2, Section 3, Exercise 2

# Copying the code you wrote in Section 3, Exercise 1:
#  1. For circleMarkers on leaflet map, points that have obs == 1 should be filled, whereas points with obs == 0 should be open (i.e., use color on the stroke of the circleMarker)
#  2. Create labels for the circleMarkers that includes values for species, year, and occurrence
#  3. Place the leaflet map w/in a 'card', that also includes a footer: "Open circles denote sampling locations where the selected species was not detected, whereas filled circles denote detected species occurrence."; also allow the card to be expanded to full-screen
#  4. Add another page to the app layout, where the first paragraph describes basic metrics summarizing the simulated data and what these data represent, while a second paragraph describes the content included on the 'Data summary' page

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
  
  ### About page
  nav_panel(
    title = "About",
    
    # Title for text body
    h1("About these data"),
    
    # First paragraph of text
    p("Data were simulated to represent 1000 samples of occurrence measurements for 8 aquatic species across Candada from 2020 - 2024. This simulation was performed to resemble data that may be obtained from sampling to evaluate the occupancy of species, using methods such as eDNA sampling or visual surveys."),
    
    # Second paragraph of text
    p("This simulated dataset is visualized on the other page, including an interactive map, data table, and summary figures that show trends in detection rates of each species over time, as well as proportion of occurrences per province.")
  ),
  
  
  ### Content page
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
      
      card(
        leafletOutput("map"),
        card_footer(
          em("Open circles denote sampling locations where the selected species was not detected, whereas filled circles denote detected species occurrence.")
          ),
        full_screen = TRUE
      )
      
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
  outputOptions(output, "map", suspendWhenHidden = FALSE)  #need for displaying points for map on load
  
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
                       stroke = TRUE,
                       fillOpacity = ~obs * 0.7,
                       
                       label = ~paste0(strong("Species: "), spp, br(),
                                       strong("Year: "), year, br(),
                                       strong("Occurrence: "), obs) |> 
                         lapply(HTML)) |>
      addLegend(pal = spp_pal,
                values = ~spp,
                title = "Species")
  })
  
}





###############
### Run app ###
###############

shinyApp(ui = ui, server = server)
