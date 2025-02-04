
### Day 1, Section 3, Exercise 2

# Using the simple example:
#  1. Create reactive value for the binwidth used in histogram (by species and variable)
#  2. Add binwidth to title of figure


library(palmerpenguins)
library(ggplot2)
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
    
    #create dropdown selection for numeric columns
    varSelectInput(
      inputId = "var",
      label = "Select variable",
      data = dplyr::select_if(penguins, is.numeric)
    ),
    
    radioButtons(inputId = "spp",
                 label = "Select a species",
                 choices = unique(penguins$species)
    ),
    
    #create slider input for histogram
    sliderInput("bins",
                "Number of bins",
                min = 3,
                max = 100,
                value = 30,
                step = 1),
    
    
    hr(),  #add horizontal line
    
    # Artwork from Allison Horst
    tags$figure(
      img(src = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png",
          width = "100%"),
      tags$figcaption(em("Artwork by @allison_horst"))
    )
  ),
  
  # Main panel content
  h3("Exploration of Palmer penguins data"),
  
  plotOutput("hist")
  
)



#####################
### Define server ###
#####################

server <- function(input, output, session) {
  
  # Create histogram based on selection from inputs
  output$hist <- renderPlot({
    ggplot(penguins[penguins$species == input$spp,]) +
      geom_histogram(aes(!!input$var), fill = "cadetblue", bins = input$bins) +
      theme_bw(base_size = 20)
  })
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
