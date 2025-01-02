

## Shiny app example that explores more outputs

library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(shiny)
library(bslib)
library(bsicons)
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
    
    # Add image w/ hyperlink
    a(href = "https://pallter.marine.rutgers.edu",  #add hyperlink
      img(src = "Palmer_LTER_logo.png",  #add image from local directory (in www/ folder)
          width = "80%",
          alt = "Palmer LTER logo",
          style = "display: block; margin-left: auto; margin-right: auto;")  #to align image to center
    ),
    
    # Add reactive text that summarizes 'species' selection
    htmlOutput("txt")
    
  ),
  
  # Main panel content
  layout_columns(
    value_box(title = "Total sample size",
              value = nrow(penguins),
              showcase = icon('kiwi-bird'),
              theme = "bg-gradient-indigo-purple"),
    
    value_box(title = "Islands sampled",
              value = n_distinct(penguins$island),
              showcase = bsicons::bs_icon('geo-alt-fill'),
              theme = "bg-gradient-orange-green"),
    
    value_box(title = "Latest sampling year",
              value = max(penguins$year),
              showcase = icon('calendar'),
              theme = "bg-gradient-yellow-red")
  ),
  
  
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
  
  # Generate text for sidebar based on penguins_filt
  output$txt <- renderText({
    paste(strong(n_distinct(penguins_filt()$species)),  #bold reactive component
          "species were selected, which includes",
          strong(paste(unique(penguins_filt()$species), collapse = ", "))  #bold reactive component
          )
  })
  
  
  ### Output in app ###
  
  # Create biplot based on selected variables
  output$biplot <- renderPlot({
    ggplot(penguins_filt()) +
      geom_point(aes(!!input$var_x, !!input$var_y, color = species), size = 2, alpha = 0.75) +
      scale_color_brewer(palette = "Set1") +
      theme_bw(base_size = 20)
  })
  
  # Create table from brushed points on plot
  output$tbl <- renderDataTable(
    brushedPoints(penguins_filt(), input$plot_brush),
    options = list(pageLength = 5)
  )
  
  
  
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
