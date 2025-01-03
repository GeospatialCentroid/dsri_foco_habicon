library(shiny)
library(leaflet)
library(bslib)
library(dplyr)
library(sf)
library(tidyverse)
library(terra)

# Read in data --------------------------------------------
# patch priority
patch_maps <- map(
  list.files(
    "data/output_habicon/",
    pattern = "patch_priority.tif",
    full.names = TRUE
  ),
  terra::rast
)

# corridor priority
corr_maps <- map(
  list.files(
    "data/output_habicon/",
    pattern = "corridor_priority.tif",
    full.names = TRUE
  ),
  terra::rast
)

# richness maps
patch_all <- terra::rast("data/output_habicon/patch_priority_all.tif")
corr_all <- terra::rast('data/output_habicon/corridor_priority_all.tif')

# census data
load("data/acs_Larimer_2022.RData")


# UI ------------------------------------------------------
ui <- navbarPage(
  title = "Fort Collins Conservation Decision Support Tool",
  theme = bs_theme(bootswatch = "litera"),
  
  tabPanel(
    title = "Interactive Map",
    page_sidebar(
      #title = "Fort Collins Conservation Decision Support Tool",
      theme = bs_theme(bootswatch = "litera"),
      fillable = TRUE,
      
      sidebar = sidebar(
        fixed = TRUE,
        h4("Data Layers"),
        checkboxGroupInput(
          "species_select",
          "Species Habitat:",
          choices = unique(habitat_points$species),
          selected = unique(habitat_points$species)[1]
        ),
        
        sliderInput(
          "priority_threshold",
          "Minimum Habitat Priority Score:",
          min = 1,
          max = 10,
          value = 5
        ),
        
        checkboxGroupInput(
          "socio_layers",
          "Socioeconomic Layers:",
          choices = c("Median Income", "Population Density"),
          selected = "Median Income"
        ),
        
        hr(),
        
        helpText(
          "This tool helps identify areas where conservation priorities
             overlap with different socioeconomic factors in Fort Collins."
        )
      ),
      
      div(
        #style = "height: calc(100vh - 60px); overflow-y: auto;",
        card(
          full_screen = TRUE,
          height = "500px",
          #card_header("Interactive Map"),
          leafletOutput("map", height = "100%")
        ),
        
        card(
          height = "400px",
          card_header("Area Statistics"),
          div(style = "padding: 15px;", textOutput("stats_text")),
          div(style = "height: 300px; padding: 15px;", plotOutput("priority_plot", height = "100%"))
        )
      )
    )
  ),
  tabPanel(title = "About"),
  
  tabPanel(title = "Methods")
)


# SERVER --------------------------------------------

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_habitat <- reactive({
    habitat_points %>%
      filter(species %in% input$species_select,
             priority_score >= input$priority_threshold)
  })
  
  # Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -105.05, lat = 40.57, zoom = 12)
  })
  
  # Update map layers
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
    
    # Add habitat points
    if (nrow(filtered_habitat()) > 0) {
      leafletProxy("map") %>%
        addCircleMarkers(
          data = filtered_habitat(),
          lng = ~lon, lat = ~lat,
          radius = ~priority_score * 2,
          color = "green",
          fillOpacity = 0.7,
          popup = ~paste("Species:", species, "<br>Priority:", priority_score)
        )
    }
    
    # Add socioeconomic layers
    if ("Median Income" %in% input$socio_layers) {
      leafletProxy("map") %>%
        addCircles(
          data = neighborhoods,
          lng = ~lon, lat = ~lat,
          radius = 500,
          color = "blue",
          fillOpacity = 0.3,
          popup = ~paste("Neighborhood:", neighborhood,
                         "<br>Median Income: $", format(median_income, big.mark=","))
        )
    }
    
    if ("Population Density" %in% input$socio_layers) {
      leafletProxy("map") %>%
        addCircles(
          data = neighborhoods,
          lng = ~lon, lat = ~lat,
          radius = ~sqrt(population_density) * 10,
          color = "red",
          fillOpacity = 0.3,
          popup = ~paste("Neighborhood:", neighborhood,
                         "<br>Population Density:", population_density, "per sq km")
        )
    }
  })
  
  # Statistics output
  output$stats_text <- renderText({
    n_priorities <- nrow(filtered_habitat())
    paste("Number of priority habitat areas shown:", n_priorities,
          "\nAverage priority score:", 
          round(mean(filtered_habitat()$priority_score), 1))
  })
  
  # Priority distribution plot
  output$priority_plot <- renderPlot({
    ggplot(filtered_habitat(), aes(x = priority_score)) +
      geom_histogram(binwidth = 1, fill = "forestgreen", color = "white") +
      theme_minimal() +
      labs(title = "Distribution of Habitat Priority Scores",
           x = "Priority Score",
           y = "Count")
  })
}

shinyApp(ui, server)