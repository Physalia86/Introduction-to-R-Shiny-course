

## Shiny app example for multi-panel sidebar layout

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
    title = strong("Plot controls"),
    
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
          style = "display: block; margin-left: auto; margin-right: auto;")  #to align image to center
    )
    
  ),
  
  ### Main panel content ###
  
  navset_tab(
    # Page for Adelie
    nav_panel(title = "Adelie",
              h3("Exploration of Adelie data"),
              plotOutput("adelie_plot")
    ),
    # Page for Chinstrap
    nav_panel(title = "Chinstrap",
              h3("Exploration of Chinstrap data"),
              plotOutput("chinstrap_plot")
    ),
    # Page for Gentoo
    nav_panel(title = "Gentoo",
              h3("Exploration of Gentoo data"),
              plotOutput("gentoo_plot")
    ),
    nav_spacer(),  #shift tabs to be right-aligned
    nav_menu(
      title = "Links",
      nav_item(a(shiny::icon('kiwi-bird'), "palmerpenguins", href = "https://allisonhorst.github.io/palmerpenguins/")),
      nav_item(a(shiny::icon("globe"), "Palmer LTER", href = "https://pallter.marine.rutgers.edu"))
    )
  )
  
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
