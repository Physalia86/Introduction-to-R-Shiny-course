
# Skeleton of simple standard Shiny app

library(shiny)

# Define user interface (UI)
ui <- fluidPage(
  titlePanel("My app"),
  sidebarLayout(
    sidebarPanel = sidebarPanel("Inputs"),
    mainPanel = mainPanel("Main content area for outputs")
  )
  
  # Add content here for what will be shown in your app
)

# Define server
server <- function(input, output, session) {
  
  # Add code here for doing computations and producing output
}


# Run app
shinyApp(ui, server)
