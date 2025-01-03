

## Shiny app example for basic fillable layout

library(palmerpenguins)
library(ggplot2)
library(shiny)
library(bslib)


#################
### Define UI ###
#################

ui <- page_fillable(
  h1("Penguins dashboard"),
  
  ### Input widgets ###
  
  #create dropdown selection for var on x-axis
  layout_columns(
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
  
  br(),  #add some white space
  
  ### Outputs ###
  plotOutput("adelie_plot"),
  plotOutput("chinstrap_plot"),
  plotOutput("gentoo_plot")
)



#####################
### Define server ###
#####################

server <- function(input, output, session) {
  
  # Create biplots based on selected variables
  output$adelie_plot <- renderPlot({
    ggplot(penguins[penguins$species == 'Adelie',]) +
      geom_point(aes(!!input$var_x, !!input$var_y), color = "cadetblue", size = 4, alpha = 0.75) +
      theme_bw(base_size = 20)
  })
  
  output$chinstrap_plot <- renderPlot({
    ggplot(penguins[penguins$species == 'Chinstrap',]) +
      geom_point(aes(!!input$var_x, !!input$var_y), color = "goldenrod", size = 4, alpha = 0.75) +
      theme_bw(base_size = 20)
  })
  
  output$gentoo_plot <- renderPlot({
    ggplot(penguins[penguins$species == 'Gentoo',]) +
      geom_point(aes(!!input$var_x, !!input$var_y), color = "firebrick", size = 4, alpha = 0.75) +
      theme_bw(base_size = 20)
  })
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
