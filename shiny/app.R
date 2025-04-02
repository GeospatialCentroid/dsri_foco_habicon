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
    "app_data/output_habicon/",
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
    "app_data/output_habicon/",
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
patch_all <- terra::rast("app_data/output_habicon/patch_priority_all.tif")
corr_all <- terra::rast('app_data/output_habicon/corridor_priority_all.tif')

# richness palettes (patches is reactive below)
joint_corr_pal <- colorNumeric(
  palette = "RdPu",
  domain = values(corr_all),
  na.color = "transparent"
)


# census data
load("app_data/acs_Larimer_2022.RData")

## create color palettes for each variable
pal_list <- list(
  population_density = colorNumeric("Greys", cleaned_acs$population_density, na.color = "red"),
  median_income = colorNumeric("Greens", cleaned_acs$median_income, na.color = "gray"),
  housing_burden = colorNumeric("Blues", cleaned_acs$housing_burden, na.color = "gray"),
  percent_housing_burden = colorNumeric("Blues", cleaned_acs$percent_housing_burden, na.color = "gray"),
  racial_minority = colorNumeric("Purples", cleaned_acs$racial_minority, na.color = "gray"),
  percent_racial_minority = colorNumeric("Purples", cleaned_acs$percent_racial_minority, na.color = "gray"),
  low_income = colorNumeric("Reds", cleaned_acs$low_income, na.color = "gray"),
  percent_low_income = colorNumeric("Reds", cleaned_acs$percent_low_income, na.color = "gray")
  
)

## nice labels for census variables
var_labels <- c(
  "Population Density",
  "Median Household Income",
  "Housing Burden",
  "Percent Housing Burden",
  "Racial Minority",
  "Percent Racial Minority",
  "Low Income",
  "Percent Low Income"
)
names(var_labels) <- names(pal_list)


# Natural Areas
nat_areas <- read_sf("app_data/Natural_Areas.shp") %>% 
  st_transform(4326)


# vector of species names
species_names <- read_csv("app_data/species_names.csv") %>% 
  # remove yellow warbler (SDM errors)
  filter(!common_name %in% c("Yellow Warbler", "Bumblebees"))


# NA LEGEND FIX
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))


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
        ## SPECIES LAYERS --------------------
        
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
        ## CENSUS LAYERS ---------------------
        
        accordion(
          open = FALSE,
          accordion_panel(
            "Socioeconomic and Demographic Layers",
            materialSwitch("show_blocks", "Show Layers", value = FALSE),
            materialSwitch("show_pcnt", "Display Values as Percent", value = FALSE),
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
          leafletOutput("map", height = "100%"),
          HTML(html_fix)
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
  
  # joint maps
  joint_patches <- reactive({
    patch_all %>%
      subset(grep(input$patch_variable, names(.)))
  })
  
  
  # palettes and legend titles
  
  ## species patches
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
  
  ## joint patches
  joint_patch_pal <- reactive({
    colorNumeric(
      palette = "YlGn",
      domain = values(joint_patches()),
      na.color = "transparent"
    )
  })
  
  joint_patch_title <- reactive({
    if(names(joint_patches()) == "qwa") {
      "Joint Quality-Weighted Area"
    } else if (names(joint_patches()) == "btwn") {
      "Joint Betweenness Centrality"
    } else {
      "Joint Equivalent Connectivity"
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
      addTiles(group = "OpenStreetMap") %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      setView(lng = -105.05, lat = 40.57, zoom = 12) %>% 
      addPolygons(
        data = nat_areas,
        color = "green",
        weight = 0.5,
        opacity = 1,
        group = "Natural Areas",
        popup = ~ paste(
          str_to_title(NA_NAME))
      ) %>% 
      addLayersControl(
        baseGroups = c("CartoDB Positron", "OpenStreetMap"),
        overlayGroups = c("Natural Areas"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      hideGroup("Natural Areas") %>% 
      htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
    }
  ")
  })
  
  # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
  outputOptions(output, "map", suspendWhenHidden=FALSE)
  
  # Observe layer selection changes
  observe({
    
    input$nav

    ## SPECIES MAPS -------------------------
    
    if (!input$model_type) {
      ## Add Species Layers
      leafletProxy("map") %>%
        #clearShapes() %>%
        clearGroup("Patches") %>%
        clearGroup("Corridors") %>%
        clearControls() %>%
        clearImages()
      
      # Add raster layers
      if ("Patches" %in% input$map_type) {
        leafletProxy("map") %>%
          addRasterImage(
            sp_patches(),
            colors = sp_patch_pal(),
            opacity = 0.9,
            group = "Patches"
          ) %>%
          #addLegendNumeric(
          addLegend(
            group = "Patches",
            pal = sp_patch_pal(),
            values = na.omit(values(sp_patches())),
            bins = 5,
            title = sp_patch_title(),
            labFormat = function(type, cuts, p) {
              c("Low", rep("", length(cuts) - 2), "High")
            },
            position = "topright"
            #decreasing = T,
            #labelStyle = "font-family: 'Arial'; font-size: 14px; color: #555;"
          )
        
      }
      
      if ("Corridors" %in% input$map_type) {
        leafletProxy("map") %>%
          addRasterImage(
            sp_corridors(),
            colors = sp_corr_pal(),
            opacity = 0.9,
            group = "Corridors"
          ) %>%
          #addLegendNumeric(
          addLegend(
            group = "Corridors",
            pal = sp_corr_pal(),
            bins = 5,
            values = na.omit(values(sp_corridors())),
            title = "Corridor Priority",
            labFormat = function(type, cuts, p) {
              c("Low", rep("", length(cuts) - 2), "High")
            },
            position = "topright"
            #decreasing = T,
            #labelStyle = "font-family: 'Arial'; font-size: 14px; color: #555;"
          )
        
      }
      
      # remove species maps if neither box is checked
      if (is.null(input$map_type)) {
        leafletProxy("map") %>%
          clearGroup(c("Patches", "Corridors"))
        
      }
    }
    
    ## JOINT MAPS -----------------
    
    if (input$model_type){
      leafletProxy("map") %>% 
        clearImages() %>% 
        clearControls()
      
      # Add raster layers
      if("Patches" %in% input$richness_maps){
        leafletProxy("map") %>%
          addRasterImage(joint_patches(),
                         colors = joint_patch_pal(),
                         opacity = 0.9,
                         group = "Patches") %>%
          #addLegendNumeric(
          addLegend(
            group = "Patches",
            pal = joint_patch_pal(),
            bins = 5,
            values = na.omit(values(joint_patches())),
            title = joint_patch_title(),
            labFormat = function(type, cuts, p) {
              c("Low", rep("", length(cuts) - 2), "High")},
            position = "topright"
            #decreasing = T,
            #labelStyle = "font-family: 'Arial'; font-size: 14px; color: #555;"
          )

      }
      
      if ("Corridors" %in% input$richness_maps){
        leafletProxy("map") %>% 
          addRasterImage(corr_all, 
                         colors = joint_corr_pal, 
                         opacity = 0.9,
                         group = "Corridors") %>%
          #addLegendNumeric(
          addLegend(
            group = "Corridors",
            pal = joint_corr_pal,
            bins = 5,
            values = na.omit(values(corr_all)),
            title = "Joint Corridor Priority",
            labFormat = function(type, cuts, p) {
              c("Low", rep("", length(cuts) - 2), "High")},
            position = "topright"
            #decreasing = T,
            #labelStyle = "font-family: 'Arial'; font-size: 14px; color: #555;"
          )
        
      }
      
      
      
    }
      
    
    ## CENSUS DATA -----------------
    
    if (input$show_blocks) {
      
      if (input$show_pcnt & input$socio_layers %in% c("housing_burden", "racial_minority", "low_income")) {
        
        leafletProxy("map") %>%
          clearGroup("Census Data") %>%
          addPolygons(
            data = cleaned_acs,
            fillColor = ~ pal_list[[paste0("percent_", input$socio_layers)]](get(paste0("percent_", input$socio_layers))),
            fillOpacity = 0.5,
            weight = 0.5,
            color = "#444444",
            group = "Census Data",
            label = ~ paste0(var_labels[input$socio_layers], ": ", round(get(paste0("percent_", input$socio_layers)), 0), "%"),
            popup = ~ paste(
              "Total Population:",
              format(total_population, big.mark = ","),
              "<br>Median Income: $",
              format(median_income, big.mark = ",")
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal_list[[paste0("percent_", input$socio_layers)]],
            values = cleaned_acs[[paste0("percent_", input$socio_layers)]],
            title = var_labels[[paste0("percent_", input$socio_layers)]],
            group = "Census Data",
            na.label = "No data"
          )
        
      } else {
      
      leafletProxy("map") %>%
        clearGroup("Census Data") %>%
        addPolygons(
          data = cleaned_acs,
          fillColor = ~ pal_list[[input$socio_layers]](get(input$socio_layers)),
          fillOpacity = 0.5,
          weight = 0.5,
          color = "#444444",
          group = "Census Data",
          label = ~ paste0(var_labels[input$socio_layers], ": ", format(round(get(input$socio_layers), 0), big.mark = ",")),
          popup = ~ paste(
            "Total Population:",
            format(total_population, big.mark = ","),
            "<br>Median Income: $",
            format(median_income, big.mark = ",")
          )
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
    } else {
      
      leafletProxy("map") %>%
        clearGroup("Census Data") 
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