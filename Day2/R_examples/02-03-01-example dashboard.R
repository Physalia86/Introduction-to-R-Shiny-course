

## Completed example Shiny app with spatial and other components
# install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
# install.packages(c("webshot2", "pagedown", "curl"))

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(bslib)
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(DT)
# Packages below are used for screenshot of Leaflet map
library(mapview)
library(webshot2)
# force the use of pagedown to install chrome on shinyapps.io (this is a workaround)
library(pagedown)
# force the use of curl because chromote needs it (see https://github.com/rstudio/chromote/issues/37)
library(curl)
##  troubleshooting for screenshots courtesy of https://forum.posit.co/t/how-to-properly-configure-google-chrome-on-shinyapps-io-because-of-webshot2/109020


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
  theme = bs_theme(bootswatch = 'yeti'),
  
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
                    multiple = TRUE),
        downloadButton("save_map", label = "Download map"),
        p('Please allow a few seconds after clicking the download button for your map to be ready.')
      ),
      
      
      # Main panel content
      layout_columns(
        col_widths = c(12, 6, 6),
        row_heights = c(3, 2),
        
        # Map
        card(leafletOutput("map"),
             card_footer(em('Open circles denote sampling locations where the selected species was not detected, whereas filled circles denote detected species occurrence.')),
             full_screen = TRUE),
        
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
                       
                       label = ~paste0("<b>Species:</b> ", spp,
                                       "<br> <b>Year:</b> ", year,
                                       "<br> <b>Occurrence:</b> ", obs) |> 
                         lapply(HTML),
                       labelOptions = labelOptions(style = list(
                                                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                     "font-size" = "12px",
                                                     "background-color" = "rgba(255,255,255,0.85)"
                                                   ))
                       ) |>
      addLegend(pal = spp_pal,
                values = ~spp,
                title = "Species")
  })
  
  
  
  # Store reactive map for download
  save_map <- reactive({
    leaflet(data = occ_filt()) |>
      addTiles() |>
      setView(lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom) |> 
      addCircleMarkers(radius = 5,
                       weight = 2,
                       opacity = 0.7,
                       color = ~spp_pal(spp),
                       fillColor = ~spp_pal(spp),
                       stroke = TRUE,
                       fillOpacity = ~obs * 0.7,
                       
                       label = ~paste0("<b>Species:</b> ", spp,
                                       "<br> <b>Year:</b> ", year,
                                       "<br> <b>Occurrence:</b> ", obs) |> 
                         lapply(HTML),
                       labelOptions = labelOptions(style = list(
                         "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                         "font-size" = "12px",
                         "background-color" = "rgba(255,255,255,0.6)"
                       ))
      ) |>
      addLegend(pal = spp_pal,
                values = ~spp,
                title = "Species")
  })
  
  
  # Export PNG of leaflet map when button is clicked
  output$save_map <- downloadHandler(
    filename = "map.png",
    content = function(file) {
      mapshot2(save_map(), file = file, 
              cliprect = "viewport",
              zoom = 2,
              selfcontained = FALSE)
    }
  )

  

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

shinyApp(ui, server)
