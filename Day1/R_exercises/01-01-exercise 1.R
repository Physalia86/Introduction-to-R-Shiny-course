
### Day 1, Section 1, Exercise 1

# Using the {bslib} Shiny app skeleton:
#  1. Change title to "Skeleton Shiny app"
#  2. Add a brief sentence to place under the sidebar title
#  3. Change the text shown for the main panel


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
