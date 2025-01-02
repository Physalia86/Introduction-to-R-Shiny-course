

## Shiny app example that explores more outputs

library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(shiny)
library(bslib)


#################
### Define UI ###
#################

ui <- page_sidebar(
  title = h1("Penguins dashboard"),
  
  # Define sidebar inputs
  sidebar = sidebar(
    title = strong("Histogram controls"),
    
    # Controls for both plots
    selectizeInput(inputId = "spp",
                   label = "Select a species",
                   choices = unique(penguins$species),
                   selected = "Adelie",
                   multiple = TRUE
    ),
    
    
    #create dropdown selection for var on x-axis
    varSelectInput(
      inputId = "var_x",
      label = "Select x-axis",
      data = penguins,
      selected = "bill_length_mm"
    ),
    
    #create dropdown selection for var on y-axis
    varSelectInput(
      inputId = "var_y",
      label = "Select y-axis",
      data = penguins,
      selected = "body_mass_g"
    ),
    
    
    # Artwork from Allison Horst
    tags$figure(
      img(src = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png",
          width = "100%"),
      tags$figcaption(em("Artwork by @allison_horst"))
    )
    
  ),
  
  # Main panel content
  h3("Exploration of Palmer penguins data"),
  
  plotOutput("biplot"),  #add figure
  
  a(href = "https://pallter.marine.rutgers.edu",  #add hyperlink
    img(src = "Palmer_LTER_logo.png",  #add image from local directory (in www/ folder)
        width = "50%",
        alt = "Palmer LTER logo",
        style = "display: block; margin-left: auto; margin-right: auto;")  #to align image to center
  )
  
)



#####################
### Define server ###
#####################

server <- function(input, output, session) {
  
  # Create reactive object for filtered penguins data
  penguins_filt <- reactive({
    penguins |> 
      filter(species %in% input$spp)
  })
  
  
  ### Output in app ###
  
  # Create biplot based on selected variables
  output$biplot <- renderPlot({
    ggplot(penguins_filt()) +
      geom_point(aes(!!input$var_x, !!input$var_y, color = species), size = 2, alpha = 0.75) +
      scale_color_brewer(palette = "Set1") +
      theme_bw(base_size = 20)
  })
  
  
  
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
