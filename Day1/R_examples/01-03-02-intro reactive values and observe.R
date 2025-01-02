

## Intro to reactive values and using `observe` for side effects

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
    title = strong("Histogram controls"),  #make text bold
    
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
    
    radioButtons(inputId = "spp",
                 label = "Select a species",
                 choices = unique(penguins$species)
    ),
    
    #create slider input for histogram
    sliderInput("years",
                "Select years of interest",
                min = min(penguins$year),
                max = max(penguins$year),
                value = range(penguins$year),
                step = 1,
                sep = "")
  ),
  
  # Main panel content
  h3("Exploration of Palmer penguins data"),
  
  plotOutput("biplot")  #add figure
  
  
)



#####################
### Define server ###
#####################

server <- function(input, output, session) {
  
  # Create reactive object to filter based on multiple inputs
  penguins_filt <- reactive({
    penguins |> 
      filter(year >= input$years[1] & year <= input$years[2],
             species == input$spp)
  })
  
  
  # Create reactive value for sample size of dataset
  r <- reactiveValues(n = nrow(penguins))
  
  # Update reactiveValue for sample size based on penguins_filt
  observe({
    req(penguins_filt())  #ensure/require that reactive object is available before proceeding
    r$n <- nrow(penguins_filt())  #update reactive value
  })
  
  
  
  # Create biplot based on selected variables
  output$biplot <- renderPlot({
    ggplot(penguins_filt()) +  #reactive objects need empty parentheses to work
      geom_point(aes(!!input$var_x, !!input$var_y, color = species), size = 2, alpha = 0.75) +
      scale_color_brewer(palette = "Set1") +
      labs(title = paste("N =", r$n)) +  #reactive value used in title
      theme_bw(base_size = 20)
  })
  
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
