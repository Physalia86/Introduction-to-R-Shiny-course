

## Shiny app example that explores input widgets

library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(shiny)
library(bslib)


#################
### Define UI ###
#################

ui <- page_sidebar(
  title = h1("Penguins dashboard"),  #Use H1 header
  
  # Define sidebar inputs
  sidebar = sidebar(
    title = strong("Histogram controls"),  #make text bold
    
    # Controls for both plots
    checkboxGroupInput(inputId = "spp",
                       label = "Select a species",
                       choices = unique(penguins$species),
                       selected = "Adelie"
    ),
    
    # Add action button to update filter
    actionButton(
      inputId = "update_filt",
      label = "Update",
      class = "btn-primary"
    ),
    
    hr(),  #add horizontal line
    
    # Controls for top plot
    em("Controls for histogram"),
    
    #create dropdown selection for numeric columns
    varSelectInput(
      inputId = "var",
      label = "Select variable",
      data = dplyr::select_if(penguins, is.numeric)
    ),
    
    #create slider input for histogram
    sliderInput("bins",
                "Number of bins",  #make text look like monospace code
                min = 3,
                max = 100,
                value = 30,
                step = 1),
    
    
    
    hr(),  #add horizontal line
    
    
    # Controls for bottom plot
    em("Controls for biplot"),
    
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
    )
    
  ),
  
  # Main panel content
  h3("Exploration of Palmer penguins data"),
  
  plotOutput("hist"),
  plotOutput("biplot")
  
)



#####################
### Define server ###
#####################

server <- function(input, output, session) {
  
  # Create reactive object
  penguins_filt <- eventReactive(input$update_filt, {
    penguins |> 
      filter(species %in% input$spp)
  })
  
  # Create histogram based on selection from inputs
  output$hist <- renderPlot({
    ggplot(penguins_filt()) +
      geom_histogram(aes(!!input$var, fill = species), bins = input$bins) +
      scale_fill_brewer(palette = "Set1") +
      theme_bw(base_size = 20)
  })
  
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
