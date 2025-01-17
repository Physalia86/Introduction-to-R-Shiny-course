

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

ui <- page_navbar()





##############
### Server ###
##############

server <- function(input, output, session) {
  
}





###############
### Run app ###
###############

shinyApp(ui = ui, server = server)
