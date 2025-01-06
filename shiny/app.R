library(shiny)
library(leaflet)
library(bslib)
library(dplyr)
library(sf)
library(tidyverse)
library(terra)
library(leaflegend)
library(shinyWidgets)

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
## names
names(patch_maps) <-  map(
  patch_maps,
  ~ tools::file_path_sans_ext(basename(sources(.x))) %>% str_remove("_patch_priority")
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
## names
names(corr_maps) <-  map(
  corr_maps,
  ~ tools::file_path_sans_ext(basename(sources(.x))) %>% str_remove("_corridor_priority")
)


# richness maps
patch_all <- terra::rast("data/output_habicon/patch_priority_all.tif")
corr_all <- terra::rast('data/output_habicon/corridor_priority_all.tif')

# richness palettes
joint_patch_pal <- colorNumeric(
  palette = "YlGn",
  domain = values(patch_all),
  na.color = "transparent"
)

joint_corr_pal <- colorNumeric(
  palette = "RdPu",
  domain = values(corr_all),
  na.color = "transparent"
)


# census data
load("data/acs_Larimer_2022.RData")

## create color palettes for each variable
pal_list <- list(
  population_density = colorNumeric("Greys", cleaned_acs$population_density, na.color = "transparent"),
  median_income = colorNumeric("Greens", cleaned_acs$median_income, na.color = "transparent"),
  housing_burden = colorNumeric("Blues", cleaned_acs$housing_burden, na.color = "transparent"),
  racial_minority = colorNumeric("Purples", cleaned_acs$racial_minority, na.color = "transparent"),
  low_income = colorNumeric("Reds", cleaned_acs$low_income, na.color = "transparent")
)

## nice labels for census variables
var_labels <- c(
  "Population Density",
  "Median Household Income",
  "Housing Burden",
  "Racial Minority",
  "Low Income"
)
names(var_labels) <- names(pal_list)


# vector of species names
species_names <- read_csv("data/species_names.csv") %>% 
  # remove yellow warbler (SDM errors)
  filter(!common_name %in% "Yellow Warbler")


# UI ------------------------------------------------------
ui <- navbarPage(
  id = "nav",
  title = "Fort Collins Conservation Decision Support Tool",
  theme = bs_theme(bootswatch = "litera"),
  
  tabPanel(
    title = "Interactive Map",
    page_sidebar(
      #title = "Fort Collins Conservation Decision Support Tool",
      theme = bs_theme(bootswatch = "litera"),
      fillable = TRUE,
      sidebar = sidebar(
        title = "Data Layers",
        width = 300,
        fixed = TRUE,
        accordion(
          open = FALSE,
          accordion_panel(
            "Species Layers",
            materialSwitch("model_type", "View Joint Species Maps", value = FALSE),
            # switchInput(
            #   inputId = "model_type",
            #   label = "View Maps By:",  # Add a label for context
            #   onLabel = "Single Species",            # Label when switched on
            #   offLabel = "All Species",            # Label when switched off
            #   value = TRUE              # Default value (off = "No")
            # ),
            conditionalPanel(
              "input.model_type == false",
              radioButtons(
                "species",
                "Species:",
                choiceNames = species_names$common_name,
                choiceValues = species_names$scientific_name,
                selected = species_names$scientific_name[1]
              ),
              checkboxGroupInput(
                "map_type",
                "Priority Maps:",
                choices = c("Patches", "Corridors"),
                selected = "Patches"
              ),
            ),
            conditionalPanel(
              "input.model_type == true",
              checkboxGroupInput(
                "richness_maps",
                "Joint Priority Maps:",
                choices = c("Patches", "Corridors"),
                selected = "Patches"
              )
            ),
            radioButtons(
              "patch_variable",
              "Choose Patch Priority Metric",
              choiceNames = c("Quality-Weighted Area", "Betweenness Centrality", "Equivalent Connectivity"),
              choiceValues = c("qwa", "btwn", "dECA"),
              selected = "qwa"
            ),
            helpText(
              "Species joint maps were calculated by normalizing and summing priority maps for all species."
            ),
          )
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            "Socioeconomic and Demographic Layers",
            input_switch("show_blocks", "Show Layers", value = FALSE),
            radioButtons(
              "socio_layers",
              "",
              choiceNames = c(
                "Population Density",
                "Median Household Income",
                "Housing Burden",
                "Racial Minority",
                "Low Income"
              ),
              choiceValues = c(
                "population_density",
                "median_income",
                "housing_burden",
                "racial_minority",
                "low_income"
              )
            )
          )
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
          height = "800px",
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
  
  tabPanel(title = "Methods"),
  tabPanel(title = "Species Model Results")
)


# SERVER --------------------------------------------

server <- function(input, output, session) {
  
  
  ## INTERACTIVE MAP --------------------------
  

  # species maps
  sp_patches <- reactive({
    patch_maps[[input$species]] %>%
      subset(grep(input$patch_variable, names(.)))
  })
  
  sp_corridors <- reactive({
    corr_maps[[input$species]]
  })
  
  # palettes and legend titles
  
  ## patches
  sp_patch_pal <- reactive({
    colorNumeric(
        palette = "YlGn",
        domain = values(sp_patches()),
        na.color = "transparent"
      )
  })
  
  sp_patch_title <- reactive({
    if(names(sp_patches()) == "qwa") {
      "Quality-Weighted Area"
    } else if (names(sp_patches()) == "btwn") {
      "Betweenness Centrality"
    } else {
      "Equivalent Connectivity"
    }
  })
  
  ## corridors
  sp_corr_pal <- reactive({
    colorNumeric(
      palette = "RdPu",
      domain = values(sp_corridors()),
      na.color = "transparent"
    )
  })

  
  # Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -105.05, lat = 40.57, zoom = 12) 
  })
  
  # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
  outputOptions(output, "map", suspendWhenHidden=FALSE)
  
  # Observe layer selection changes
  observe({
    
    input$nav

    if(!input$model_type){
    ## Add Species Layers
    leafletProxy("map") %>%
      clearShapes() %>% 
      clearControls() %>% 
      clearImages()
    
    # Add raster layers
    if("Patches" %in% input$map_type){
      leafletProxy("map") %>% 
        addRasterImage(sp_patches(), 
                       colors = sp_patch_pal(), 
                       opacity = 0.9,
                       group = "Patches") %>%
        #addLegendNumeric(
        addLegend(
          group = "Patches",
          pal = sp_patch_pal(),
          values = na.omit(values(sp_patches())),
          title = sp_patch_title(),
          position = "bottomright"
          #decreasing = T,
          #labelStyle = "font-family: 'Arial'; font-size: 14px; color: #555;"
        )  
      
    }
    
    if("Corridors" %in% input$map_type){
      leafletProxy("map") %>% 
        addRasterImage(sp_corridors(), 
                       colors = sp_corr_pal(), 
                       opacity = 0.9,
                       group = "Corridors") %>%
        #addLegendNumeric(
        addLegend(
          group = "Corridors",
          pal = sp_corr_pal(),
          values = na.omit(values(sp_corridors())),
          title = "Corridor Priority",
          position = "bottomright"
          #decreasing = T,
          #labelStyle = "font-family: 'Arial'; font-size: 14px; color: #555;"
        ) 
      
    }
      
      # remove species maps if neither box is checked
      if(is.null(input$map_type)){
        leafletProxy("map") %>% 
          clearGroup(c("Patches", "Corridors"))
        
      }
  }
    
    ## JOINT MAPS -----------------
    if(input$model_type){
      leafletProxy("map") %>% 
        clearImages()
      
      # # Add raster layers
      # if("Patches" %in% input$richness_maps){
      #   leafletProxy("map") %>% 
      #     addRasterImage(patch_all, 
      #                    colors = joint_patch_pal, 
      #                    opacity = 0.9,
      #                    group = "Patches") %>%
      #     #addLegendNumeric(
      #     addLegend(
      #       group = "Patches",
      #       pal = patch_all,
      #       values = na.omit(values(patch_all)),
      #       title = "Joint Patch Priority",
      #       position = "bottomright"
      #       #decreasing = T,
      #       #labelStyle = "font-family: 'Arial'; font-size: 14px; color: #555;"
      #     )  
      #   
      # }
      
      if("Corridors" %in% input$richness_maps){
        leafletProxy("map") %>% 
          addRasterImage(corr_all, 
                         colors = joint_corr_pal, 
                         opacity = 0.9,
                         group = "Corridors") %>%
          #addLegendNumeric(
          addLegend(
            group = "Corridors",
            pal = corr_all,
            values = na.omit(values(corr_all)),
            title = "Joint Corridor Priority",
            position = "bottomright"
            #decreasing = T,
            #labelStyle = "font-family: 'Arial'; font-size: 14px; color: #555;"
          ) 
        
      }
      
      
      
    }
      
    
    # Add Census Data
    if(input$show_blocks) {
      leafletProxy("map") %>% 
      addPolygons(
        data = cleaned_acs,
        fillColor = ~ pal_list[[input$socio_layers]](get(input$socio_layers)),
        fillOpacity = 0.7,
        weight = 0.5,
        color = "#444444",
        group = "Census Data",
        label = ~ paste0(var_labels[input$socio_layers], ": ", formatC(get(
          input$socio_layers
        ), big.mark = ",")),
        popup = ~paste("Total Population:", total_population,"<br>Median Income: $", format(median_income, big.mark=","))
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_list[[input$socio_layers]],
        values = cleaned_acs[[input$socio_layers]],
        title = var_labels[[input$socio_layers]],
        group = "Census Data",
        na.label = "No data"
      )
        
    }

   

  })
  
  
  

    # # Statistics output
  # output$stats_text <- renderText({
  #   n_priorities <- nrow(filtered_habitat())
  #   paste("Number of priority habitat areas shown:", n_priorities,
  #         "\nAverage priority score:", 
  #         round(mean(filtered_habitat()$priority_score), 1))
  # })
  # 
  # # Priority distribution plot
  # output$priority_plot <- renderPlot({
  #   ggplot(filtered_habitat(), aes(x = priority_score)) +
  #     geom_histogram(binwidth = 1, fill = "forestgreen", color = "white") +
  #     theme_minimal() +
  #     labs(title = "Distribution of Habitat Priority Scores",
  #          x = "Priority Score",
  #          y = "Count")
  # })
  
  
}

shinyApp(ui, server)