

## Shiny app example that explores more input widgets

library(palmerpenguins)
library(ggplot2)
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
    
    # Add button to download figure
    downloadButton(outputId = "save_fig",
                   label = "Download figure"),
    
    # Add download link for full dataset
    downloadLink(outputId = "save_data",
                 label = "Download full penguins dataset")
    
  ),
  
  # Main panel content
  h3("Exploration of Palmer penguins data"),
  
  plotOutput("biplot")
  
)



#####################
### Define server ###
#####################

server <- function(input, output, session) {
  
  # Create reactive object for ggplot fig
  fig <- reactive({
    ggplot(penguins[penguins$species %in% input$spp,]) +
      geom_point(aes(!!input$var_x, !!input$var_y, color = species), size = 2, alpha = 0.75) +
      scale_color_brewer(palette = "Set1") +
      theme_bw(base_size = 20)
  })
  
  # Create biplot based on selected variables
  output$biplot <- renderPlot({
    fig()
  })
  
  # Code to export figure upon clicking download button
  output$save_fig <- downloadHandler(
    filename = "example_fig.png",
    content = function(file) {
      ggsave(file, plot = fig(), device = "png", width = 6, height = 4, units = "in")
    }
  )
  
  # Code to export data upon clicking download link
  output$save_data <- downloadHandler(
    filename = "penguins.csv",
    content = function(file) {
      readr::write_csv(penguins, file)
    }
  )
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
