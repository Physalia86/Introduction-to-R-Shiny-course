

### Day 2, Section 3, Exercise 3

# Copying the code you wrote in Section 3, Exercise 2:
#  1. Change the layout of the 'Data summary' page, where it consists of two rows: the card containing the map should be on the top row and cover the entire width, whereas the bottom row is comprised of 2 cards of equal width
#  2. For the card on the left of the bottom row, place a table containing all of the content from `occ_dat` after removing the 'geometry' column; give this card the title "All data" and allow to expand to full screen
#  3. For the card on the right of the bottom row, use a navset_card_tab() to store 2 tabs (labelled "Time" and "Province"); the tab stored under "Time" should contain a plot showing the time series of the proportion of detected occurrences per year for the selected species; the content stored under the "Province" tab should contain a chloropleth map per selected species (made using ggplot2 and the geom_sf() function) that summarizes the species occurrence across years by province (hint: use do_union = FALSE as argument when summarizing by province)


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
