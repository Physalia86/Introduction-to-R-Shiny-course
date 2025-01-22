
### Day 1, Section 1, Exercise 2

# Using the simple example:
#  1. Fill in the blank on Line 26 for the missing inputId
#  2. Fill in the blank on Line 46 for the missing output object
#  3. Change the slider values to have a minimum of 2, maximum of 10, and a default starting value of 5


library(palmerpenguins)
library(ggplot2)
library(shiny)
library(bslib)


# Define UI
ui <- page_sidebar(
  title = "Penguins dashboard",
  
  # Define sidebar inputs
  sidebar = sidebar(
    title = "Histogram controls",
    
    #create dropdown selection for numeric columns
    varSelectInput(
      inputId = _____,
      label = "Select variable",
      data = dplyr::select_if(penguins, is.numeric)
    ),
    
    #create slider input for histogram
    sliderInput("bins", "Number of bins", min = 3, max = 100, value = 30, step = 1)
  ),  #close sidebar
  
  # Main panel content
  plotOutput("figure")
  
)  #close page_sidebar



# Define server
server <- function(input, output, session) {
  
  # Create histogram based on selection from inputs
  output$_____ <- renderPlot({
    ggplot(penguins) +
      geom_histogram(aes(!!input$variable), color = "black", fill = "cadetblue",
                     bins = input$bins) +
      theme_bw(base_size = 20)
  })
  
}


# Run app
shinyApp(ui, server)
