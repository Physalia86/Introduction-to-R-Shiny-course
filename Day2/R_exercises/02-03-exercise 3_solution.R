

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
library(DT)
library(ggplot2)


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
      
      layout_columns(col_widths = c(12, 6, 6),
                     row_heights = c(3, 2),
                     
      # Map
      card(
        leafletOutput("map"),
        card_footer(em("Open circles denote sampling locations where the selected species was not detected, whereas filled circles denote detected species occurrence.")),
        full_screen = TRUE
      ),
      
      # Table
      card(card_header(strong("All data")),
           DT::dataTableOutput("tbl"),
           full_screen = TRUE),
      
      # Plots
      navset_card_tab(
        nav_panel(title = strong("Time"),
                  plotOutput("time_trend")),
        nav_panel(title = strong("Province"),
                  plotOutput("space_trend")),
        full_screen = TRUE
      )
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
  
  
  
  ### Summary table ###
  output$tbl <- DT::renderDataTable(
    datatable(
      data = occ_dat |>
        st_drop_geometry()
    )
  )
  
  
  
  ### Summary plots ###
  
  # Create reactive object that summarizes data by species over time
  occ_dat_t <- reactive({
    occ_dat |>
      st_drop_geometry() |> 
      filter(spp == input$spp) |> 
      summarize(.by = year,
                prop_occ = sum(obs) / n())
  })
  
  output$time_trend <- renderPlot(
    ggplot(occ_dat_t(), aes(year, prop_occ)) +
      geom_line(linewidth = 0.75) +
      geom_point(size = 4, color = spp_pal(input$spp)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "Year", y = "Proportion Occurrence") +
      theme_bw(base_size = 14)
  )
  
  
  # Create reactive object that summarizes data by species and province
  occ_dat_s <- reactive({
    occ_dat |>
      filter(spp == input$spp) |> 
      group_by(province) |> 
      summarize(prop_occ = sum(obs) / n(),
                do_union = FALSE) |> 
      ungroup() |> 
      st_drop_geometry() |> 
      left_join(ca, by = 'province') |> 
      st_sf(crs = 4326)
  })
  
  output$space_trend <- renderPlot(
    ggplot() +
      geom_sf(data = ca) +
      geom_sf(data = occ_dat_s(), aes(fill = prop_occ)) +
      scale_fill_viridis_c("Proportion\noccurrence", option = "inferno") +
      theme_bw(base_size = 14)
  )
  
}





###############
### Run app ###
###############

shinyApp(ui = ui, server = server)
