

### Day 1, Section 4, Exercise 1

# Using the below example:
#  1. Change the input widget from species to Checkbox Group
#  2. Add a radio button widget that also filters the penguins dataset by year (in addition to species)

library(palmerpenguins)
library(ggplot2)
library(shiny)
library(bslib)
library(DT)


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
    
    # Add button to download figure
    downloadButton(outputId = "save_fig",
                   label = "Download figure"),
    
    # Add download link for full dataset
    downloadLink(outputId = "save_data",
                 label = "Download brushed penguins dataset")
    
  ),
  
  # Main panel content
  h3("Exploration of Palmer penguins data"),
  
  plotOutput("biplot", brush = "plot_brush"),
  dataTableOutput("tbl")
  
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
  
  # Create reactive object for ggplot fig
  fig <- reactive({
    ggplot(penguins_filt()) +
      geom_point(aes(!!input$var_x, !!input$var_y, color = species), size = 2, alpha = 0.75) +
      scale_color_brewer(palette = "Set1") +
      theme_bw(base_size = 20)
  })
  
  # Create biplot based on selected variables
  output$biplot <- renderPlot({
    fig()
  })
  
  # Create table from brushed points on plot
  output$tbl <- renderDataTable(
    brushedPoints(penguins_filt(), input$plot_brush),
    options = list(pageLength = 5)
  )
  
  
  ### Output to download ###
  
  # Code to export figure upon clicking download button
  output$save_fig <- downloadHandler(
    filename = "example_fig.png",
    content = function(file) {
      ggsave(file, plot = fig(), device = "png", width = 6, height = 4, units = "in")
    }
  )
  
  # Code to export data upon clicking download link
  output$save_data <- downloadHandler(
    filename = "penguins_brushed.csv",
    content = function(file) {
      readr::write_csv(brushedPoints(penguins_filt(), input$plot_brush),
                       file)
    }
  )
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
