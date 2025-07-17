library(shiny)
library(leaflet)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)

# Create sample data for demonstration
sample_species <- data.frame(
  common_name = c("Red-winged Blackbird", "Western Kingbird", "Yellow Warbler", "Painted Lady"),
  scientific_name = c("Agelaius phoeniceus", "Tyrannus verticalis", "Setophaga petechia", "Vanessa cardui"),
  stringsAsFactors = FALSE
)

# Sample model results
sample_results <- list(
  metadata = data.frame(
    name = c("Model type", "Features", "Regularization", "AUC", "CBI"),
    value = c("MaxEnt", "Linear, Quadratic", "1.5", "0.85", "0.72")
  ),
  variable_importance = data.frame(
    Variable = c("Temperature", "Precipitation", "Elevation", "Land Cover", "Distance to Water"),
    Permutation_importance = c(0.45, 0.23, 0.18, 0.12, 0.08),
    sd = c(0.05, 0.03, 0.02, 0.02, 0.01)
  )
)

# Sample summary statistics
sample_stats <- data.frame(
  species = c("Agelaius phoeniceus", "Tyrannus verticalis", "Setophaga petechia", "Vanessa cardui"),
  total_patches = c(125, 98, 87, 156),
  total_area = c(45.6, 32.1, 28.9, 67.3),
  connectivity_index = c(0.72, 0.68, 0.65, 0.78)
)

# UI
ui <- navbarPage(
  id = "nav",
  title = HTML("Fort Collins Decision Support Tool <em>(Demo Version)</em>"),
  theme = bs_theme(bootswatch = "litera"),
  
  # Map Tab
  tabPanel(
    title = "Interactive Map",
    page_sidebar(
      theme = bs_theme(bootswatch = "litera"),
      fillable = TRUE,
      sidebar = sidebar(
        title = "Data Layers",
        width = 300,
        
        accordion(
          open = TRUE,
          accordion_panel(
            "Species Layers",
            radioButtons(
              "species",
              "Species:",
              choiceNames = sample_species$common_name,
              choiceValues = sample_species$scientific_name,
              selected = sample_species$scientific_name[1]
            ),
            checkboxGroupInput(
              "map_type",
              "Priority Maps:",
              choices = c("Patches", "Corridors"),
              selected = "Patches"
            ),
            radioButtons(
              "patch_variable",
              "Choose Patch Priority Metric",
              choiceNames = c("Quality-Weighted Area", "Betweenness Centrality", "Equivalent Connectivity"),
              choiceValues = c("qwa", "btwn", "dECA"),
              selected = "qwa"
            )
          )
        ),
        
        accordion(
          open = FALSE,
          accordion_panel(
            "Socioeconomic Layers",
            checkboxInput("show_blocks", "Show Census Blocks", value = FALSE),
            radioButtons(
              "socio_layers",
              "Variables:",
              choices = c(
                "Population Density" = "population_density",
                "Median Income" = "median_income",
                "Housing Burden" = "housing_burden"
              )
            )
          )
        ),
        
        hr(),
        helpText("Demo version - full functionality requires external data files")
      ),
      
      # Main content with map and summary stats
      layout_columns(
        col_widths = c(8, 4),
        
        # Map panel
        card(
          full_screen = TRUE,
          height = "600px",
          card_header("Species Priority Map"),
          leafletOutput("map", height = "100%")
        ),
        
        # Summary statistics panel
        card(
          height = "600px",
          card_header("Summary Statistics"),
          card_body(
            h5("Species Overview"),
            p("This panel displays key metrics for the selected species habitat and connectivity analysis."),
            
            hr(),
            
            h5("Quick Stats"),
            div(
              style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                strong("Total Patches:"),
                span(textOutput("stat_patches", inline = TRUE), style = "font-size: 1.2em; color: #0066cc;")
              ),
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                strong("Total Area (km²):"),
                span(textOutput("stat_area", inline = TRUE), style = "font-size: 1.2em; color: #009933;")
              ),
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                strong("Connectivity Index:"),
                span(textOutput("stat_connectivity", inline = TRUE), style = "font-size: 1.2em; color: #ff6600;")
              )
            ),
            
            hr(),
            
            h5("Analysis Notes"),
            div(
              style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; font-size: 0.9em;",
              p("• Patches represent suitable habitat areas based on MaxEnt models"),
              p("• Connectivity index measures landscape permeability for species movement"),
              p("• Values update dynamically based on selected species and analysis parameters")
            )
          )
        )
      )
    )
  ),
  
  # About Tab
  tabPanel(
    title = "About",
    card(
      card_header(h3("Project Overview")),
      card_body(
        p("This is a demo version of the Fort Collins Conservation Decision Support Tool."),
        p("The full application integrates species habitat suitability models, connectivity analysis, 
          and socioeconomic data to support conservation planning decisions."),
        p(strong("Key Features:")),
        tags$ul(
          tags$li("Species-specific habitat priority mapping"),
          tags$li("Connectivity corridor analysis"),
          tags$li("Socioeconomic data integration"),
          tags$li("Model performance evaluation")
        )
      )
    ),
    
    card(
      card_header(h3("Research Team")),
      card_body(
        p(strong("Principal Investigator:"), "Caitlin Mothes, PhD, Geospatial Centroid"),
        p(strong("Co-Investigators:"), "Sara Bombaci, Liba Pejchar"),
        p(strong("Funding:"), "CSU Data Science Research Institute DISCOVER Seed Grant")
      )
    )
  ),
  
  # Methods Tab
  tabPanel(
    title = "Methods",
    card(
      card_header(h4("Analytical Pipeline")),
      card_body(
        h5("1. Species Distribution Models"),
        p("MaxEnt models predict habitat suitability across the landscape using environmental predictors."),
        
        h5("2. Connectivity Analysis"),
        p("Omniscape models map movement corridors based on habitat resistance surfaces."),
        
        h5("3. Priority Mapping"),
        p("The 'habicon' package integrates suitability and connectivity to identify priority areas."),
        
        h5("4. Socioeconomic Integration"),
        p("Census data overlays enable equitable conservation planning decisions.")
      )
    )
  ),
  
  # Results Tab
  tabPanel(
    title = "Model Results",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "demo_species",
          "Select Species:",
          choices = setNames(sample_species$scientific_name, sample_species$common_name)
        )
      ),
      mainPanel(
        h4("Model Performance"),
        p("This demo shows sample model evaluation results."),
        
        h5("Model Metadata"),
        DTOutput("meta_table"),
        
        h5("Variable Importance"),
        plotOutput("varimp_plot"),
        
        div(
          style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
          strong("Note:"), " This is a demonstration version with sample data. 
          The full application requires external data files including species occurrence data, 
          environmental predictors, and census data."
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -105.05, lat = 40.57, zoom = 12) %>%
      addMarkers(
        lng = -105.05, lat = 40.57,
        popup = "Fort Collins, CO<br/>Demo location"
      )
  })
  
  # Update map based on selections
  observe({
    species_name <- sample_species$common_name[sample_species$scientific_name == input$species]
    
    leafletProxy("map") %>%
      clearPopups() %>%
      addPopups(
        lng = -105.05, lat = 40.57,
        popup = paste("Selected Species:", species_name, "<br/>",
                      "Priority Type:", paste(input$map_type, collapse = ", "), "<br/>",
                      "Metric:", input$patch_variable)
      )
  })
  
  # Summary statistics outputs
  output$stat_patches <- renderText({
    selected_stats <- sample_stats[sample_stats$species == input$species, ]
    as.character(selected_stats$total_patches)
  })
  
  output$stat_area <- renderText({
    selected_stats <- sample_stats[sample_stats$species == input$species, ]
    paste(round(selected_stats$total_area, 1))
  })
  
  output$stat_connectivity <- renderText({
    selected_stats <- sample_stats[sample_stats$species == input$species, ]
    as.character(round(selected_stats$connectivity_index, 2))
  })
  
  # Model results table
  output$meta_table <- renderDT({
    datatable(
      sample_results$metadata,
      options = list(
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # Variable importance plot
  output$varimp_plot <- renderPlot({
    ggplot(sample_results$variable_importance, aes(
      x = reorder(Variable, Permutation_importance),
      y = Permutation_importance
    )) +
      geom_col(fill = "steelblue") +
      geom_errorbar(aes(ymin = Permutation_importance - sd, 
                        ymax = Permutation_importance + sd),
                    width = 0.2) +
      coord_flip() +
      labs(
        title = "Variable Importance (Sample Data)",
        x = "Environmental Variables",
        y = "Permutation Importance"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)
