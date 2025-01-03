

## Dashboard sidebar layout example (w/o Shiny features)

library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(shiny)
library(bslib)
library(bsicons)
library(plotly)

### Pre-process data

# Create plot
p <- ggplot(data = penguins) +
  geom_point(aes(body_mass_g, flipper_length_mm, color = species), size = 2, alpha = 0.75) +
  scale_color_brewer(palette = 'Dark2') +
  theme_bw(base_size = 20)


#################
### Define UI ###
#################

page_sidebar(
  title = h1("Penguins dashboard"),
  
  # Define sidebar inputs
  sidebar = sidebar(
    title = strong("About the data"),
    
    # Controls for both plots
    p("Data were collected and made available by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER, a member of the Long Term Ecological Research Network."),
    
    p("The", code("{palmerpenguins}"), "package contains two datasets. Both datasets contain data for 344 penguins. There are 3 different species of penguins in this dataset, collected from 3 islands in the Palmer Archipelago, Antarctica. More info can be found at this link:"),
    
    # Link to {palmerpenguins} package website
    a("{palmerpenguins}", href = "https://allisonhorst.github.io/palmerpenguins/")
    
  ),
  
  # Main panel content
  layout_columns(
    value_box(title = "Total sample size",
              value = nrow(penguins),
              showcase = shiny::icon('kiwi-bird'),
              theme = "bg-gradient-indigo-purple"),

    value_box(title = "Islands sampled",
              value = n_distinct(penguins$island),
              showcase = bsicons::bs_icon('geo-alt-fill'),
              theme = "bg-gradient-orange-green"),

    value_box(title = "Latest sampling year",
              value = max(penguins$year),
              showcase = shiny::icon('calendar'),
              theme = "bg-gradient-yellow-red")
  ),
  
  
  h3("Exploration of Palmer penguins data"),
  
  # Plot
  plotly::ggplotly(p)
  
)


