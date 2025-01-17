

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
