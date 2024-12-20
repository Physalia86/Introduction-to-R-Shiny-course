
# Skeleton of simple Shiny app using {bslib} for UI

library(shiny)
library(bslib)

# Define user interface (UI)
ui <- page_sidebar(
  title = "My app",
  sidebar = sidebar(
    title = "Inputs"
    ),
  "Main content area for outputs"
)

# Define server
server <- function(input, output, session) {
  
  # Add code here for doing computations and producing output
}


# Run app
shinyApp(ui, server)