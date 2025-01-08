

## Shiny example for defining custom theme

library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(shiny)
library(bslib)
library(DT)
library(thematic)
library(showtext)


#################
### Define UI ###
#################

# thematic_shiny(font = 'auto')

ui <- page_navbar(
  title = "Penguins dashboard",
  theme = bs_theme(bg = "#101010",
                   fg = "#FFF",
                   primary = "#E69F00",
                   secondary = "#0072B2",
                   success = "#009E73",
                   base_font = font_google("Inter"),
                   code_font = font_google("JetBrains Mono")),
  
  # First page
  nav_panel(
    title = "About",
    
    # Title for text body
    h1("Palmer penguins"),
    
    # First paragraph of text
    p("Data were collected and made available by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER, a member of the Long Term Ecological Research Network."),
    
    # Second paragraph of text
    p("The {palmerpenguins} package contains two datasets. Both datasets contain data for 344 penguins. There are 3 different species of penguins in this dataset, collected from 3 islands in the Palmer Archipelago, Antarctica. More info can be found at this link:"),
    
    # Link to {palmerpenguins} package website
    a("https://allisonhorst.github.io/palmerpenguins/", href = "https://allisonhorst.github.io/palmerpenguins/"),
    
    # Artwork from Allison Horst
    img(src = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png")
  ),
  
  
  # Second page
  nav_panel(
    title = "Data Exploration",
    
    # Content for page
    layout_sidebar(
      # Define sidebar inputs
      sidebar = sidebar(
        title = "Inputs",
        
        # Filter data by species
        selectInput(
          inputId = "species",
          label = "Filter by species",
          choices = unique(penguins$species)
        ),
        
        #create dropdown selection for numeric columns
        varSelectInput(
          inputId = "var",
          label = "Select variable",
          data = dplyr::select_if(penguins, is.numeric)
        ),
      ),  #close sidebar
      
      # Main panel content
      layout_columns(
        col_widths = c(12, 8, 4),
        row_heights = c(2, 3),
        
        # Density plot
        card(
          card_header("Density Plot"),
          plotOutput("dens"),
          full_screen = TRUE
        ),
        
        # Table
        card(
          card_header("Data Table"),
          DT::dataTableOutput("tbl"),
          full_screen = TRUE
        ),
        
        # Text
        card(
          card_header("Summary"),
          verbatimTextOutput("txt"),
          full_screen = TRUE
        )
      )
    )
  )
)




#####################
### Define server ###
#####################

server <- function(input, output, session) {
  
  
  # Create reactive object
  penguins_react <- reactive({
    penguins |> 
      filter(species == input$species)
  })
  
  # Create density plot based on selection from inputs
  output$dens <- renderPlot({
    ggplot(penguins_react()) +
      geom_density(aes(!!input$var, fill = island), alpha = 0.6)
  })
  
  
  # Create interactive table via {DT}
  output$tbl <- DT::renderDataTable(
    datatable(
      data = penguins_react()
    )
  )
  
  
  # Summarize penguins dataset for text
  output$txt <- renderPrint({
    penguins |> 
      group_by(species, island) |> 
      count()
  })
  
  
  
}



###############
### Run app ###
###############

shinyApp(ui, server)

