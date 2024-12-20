

## Simple Shiny app example with HTML tags for text styling

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
    
    #create dropdown selection for numeric columns
    varSelectInput(
      inputId = "var",
      label = "Select variable",
      data = dplyr::select_if(penguins, is.numeric)
    ),
    
    radioButtons(inputId = "spp",
                label = em("Select a species"),  #make text italic
                choices = unique(penguins$species)
                ),
    
    #create slider input for histogram
    sliderInput("bins",
                code("Number of bins"),  #make text look like monospace code
                min = 3,
                max = 100,
                value = 30,
                step = 1),
    
    
    hr(),  #add horizontal line
    
    # Brief description of dataset (as paragraph)
    p("Data were collected and made available by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER, a member of the Long Term Ecological Research Network."),
    
    p("The", code("{palmerpenguins}"), "package contains two datasets. Both datasets contain data for 344 penguins. There are 3 different species of penguins in this dataset, collected from 3 islands in the Palmer Archipelago, Antarctica. More info can be found at this link:"),
    
    # Link to {palmerpenguins} package website
    a("{palmerpenguins}", href = "https://allisonhorst.github.io/palmerpenguins/"),
    
    # Artwork from Allison Horst
    img(src = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png")
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
