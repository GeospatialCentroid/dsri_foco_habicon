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
species_names <- read_csv("app_data/species_names.csv")

species_vect <- column_to_rownames(species_names, var = "common_name")

## model results
#map(list.files("app_data/output_sdm/", full.names = TRUE), ~load(.x, envir = .GlobalEnv))


# NA LEGEND FIX
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))


# UI ------------------------------------------------------
ui <- navbarPage(
  id = "nav",
  title = HTML("Fort Collins Decision Support Tool <em>(beta version)</em>"),
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
        
        # card(
        #   height = "400px",
        #   card_header("Area Statistics"),
        #   div(style = "padding: 15px;", textOutput("stats_text")),
        #   div(style = "height: 300px; padding: 15px;", plotOutput("priority_plot", height = "100%"))
        # )
      )
    )
  ),
  # ABOUT TAB -------------
  tabPanel(
    title = "About",
    # Overview Section
    card(id = "overview_section", card_header(h3("Project Overview")), card_body(
      p(
        "Our team has developed a reproducible data science pipeline that can model spatially
              explicit habitat priorities across any species, area and time frame.
              This novel methodology simultaneously considers both habitat suitability
              and connectivity characteristics to calculate land prioritization maps.
              This tool demonstrates our first application of these methods, mapping priority areas and corridors
              for several species of interest within the City of Fort Collins.
              This interactive web application also allows for integration of these
              species priority maps with various spatial socioeconomic and demographic
              datasets for the city. The main goal of this decision-support tool is to make
              these findings more accessible to policymakers, stakeholders, and the public
              to better facilitate land use planning decisions and promote a socially equitable decision-making process."
      ),
    )),
    # Goals Section
    card(id = "goals_section", card_header(h3("Goals & Objectives")), card_body(
      p(
        "Develop a co-benefits (to people and biodiversity) approach to land-use planning by:"
      ),
      tags$ul(
        tags$li(
          "Developing a quantitative, scalable pipeline for prioritizing species landscape connectivity."
        ),
        tags$li(
          "Integrating spatial landscape priority data with sociodemographic and economic data to promote a socially equitable decision-making process."
        )
      ),
    )),
    
    # Team Section
    card(id = "team_section", card_header(
      h3("Colorado State University Research Team")
    ), card_body(
      div(
        style = "margin-bottom: 20px;",
        h5("Principal Investigator:"),
        p("Caitlin Mothes, PhD, Geospatial Centroid"),
      ), div(
        h5("Co-Investigators:"),
        p(
          "Sara Bombaci, Department of Fish, Wildlife and Conservation Biology"
        ),
        p(
          "Liba Pejchar, Department of Fish, Wildlife and Conservation Biology"
        )
      )
    ), div(
      h5("Student Contributors:"),
      p(
        "Mikko Jimenez, Ph.D. Candidate, Department of Fish, Wildlife and Conservation Biology and Graduate Degree Program in Ecology"
      ),
    )),
    
    # Funding Section
    card(id = "funding_section", card_header(h3("Funding & Support")), card_body(
      p(
        "This project was funded by CSU's Data Science Research Institute (DSRI) DISCOVER Seed Grant."
      ),
      div(
        style = "margin-top: 20px; text-align: center;",
        div(style = "display: flex; justify-content: left; flex-wrap: wrap; gap: 100px;", #div(img(src = "path/to/partner1/logo.png", height = "60px")),
            div(
              img(src = "dsri_logo.png", height = "100px")
            ), # div(img(src = "path/to/partner3/logo.png", height = "60px")))
        )
      ))),
      
      # Contact Section
      card(id = "contact_section", card_header(h3(
        "Contact Information"
      )), card_body(
        p("For questions about this project or tool, please contact:"),
        p(
          strong("Project Lead:"),
          " Caitlin Mothes, ccmothes@colostate.edu"
        ),
        
        hr(),
        # p("To cite this tool:"),
        # p(em("Author(s) (Year). Fort Collins Conservation Decision Support Tool. Institution. URL"))
      ))
    ), 

  tabPanel(title = "Methods",
           card(id = "glossary", card_header(h3("Glossary")), card_body(
             tags$ul(
               tags$li(
                 strong("Quality-Weighted Area"), ": The area of the patch multiplied by the average habitat suitability of the patch."
               ),
               tags$li(
                 strong("Betweenness Centrality"), ": The ability for the patch to serve as a stepping stone habitat weighted by the distance among patches."
               ),
               tags$li(
                 strong("Equivalent Connectivity"), ": Patch importance based on equivalent connectivity (Saura et al. 2011),
                 calculated as the difference in equivalent connecitivty of the entire landscape if the patch is removed. This metric represents 
                 a patch’s importance to the total amount and quality of reachable habitat in the landscape."
               )
             ),
           )
           ),
           ),
  
  # SDM Results --------------
  # tabPanel(title = "Species Model Results",
  #          sidebarLayout(
  #            sidebarPanel(width = 3,
  #              selectInput(
  #                "sdm_species",
  #                "Select a Species:",
  #                choices = species_vect
  #              ),
  #            ),              
  #            mainPanel(
  #              h3("Metadata"),
  #              DTOutput("meta_table"),
  #              h3("Model Performace"),
  #              h4("Tuning Results"),
  #              plotOutput("tuning_plot"),
  #              h3("Null Models"),
  #              p(
  #                "Compare empirical results to simulated. Looking for empirical evaluation metrics that are significantly better than a null model."
  #              ),
  #              tableOutput("null_table"),
  #              p(
  #                "Plots of the null model results. Looking for the empirical validation value (red point) to be significantly higher than the 99th quantile of the null values (first line in the violin plot)."
  #              ),
  #              plotOutput("null_plot"),
  #              h3("Variable Importance"),
  #              plotOutput("varimp_plot"),
  #              h3("Response Curves"),
  #              plotOutput("response_plot")
  #            )
  #          )
  # )
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
  
  
  # ### SDM RESULTS -----------------
  # # reactive RData object
  # sdm_data <- reactive({
  #   paste0(input$sdm_species, "_SDM_output")
  #   
  # })
  # 
  # # metadata plot
  # output$meta_table <- renderDT({
  #   get(sdm_data())[["metadata"]] %>% 
  #     filter(name != "projection") %>% 
  #     datatable( 
  #       options = list(
  #         scrollY = "400px",
  #         scrollCollapse = TRUE,
  #         paging = FALSE,
  #         dom = "frti"  # Controls which elements appear (no pagination)
  #       )
  #     )
  #   
  #   
  # })
  # 
  # # tuning plots
  # output$tuning_plot <- renderPlot({
  #   evalplot.stats(
  #     e = get(sdm_data())[["all_mods"]],
  #     stats = c("auc.diff", "cbi.val", "or.10p"),
  #     color = "fc",
  #     x.var = "rm",
  #     error.bars = FALSE
  #     #dodge = 0.5
  #   )
  # })
  # 
  # # null model stats
  # output$null_table <- renderTable({
  #   null.emp.results(get(sdm_data())[["null_mods"]])
  #   
  # })
  # 
  # 
  # # null model plots
  # output$null_plot <- renderPlot({
  #   evalplot.nulls(
  #     get(sdm_data())[["null_mods"]],
  #     stats = c("or.10p", "auc.val", "cbi.val"),
  #     plot.type = "violin"
  #   )
  #   
  # })
  # 
  # # variable importance plots
  # output$varimp_plot <- renderPlot({
  #   ggplot(get(sdm_data())[["variable_importance"]], aes(
  #     x = reorder(Variable, -Permutation_importance),
  #     y = Permutation_importance
  #   )) +
  #     geom_col() +
  #     geom_errorbar(aes(ymin = Permutation_importance - sd, ymax = Permutation_importance + sd),
  #                   width = 0.2) +
  #     xlab("") +
  #     theme(axis.text.x = element_text(
  #       angle = 45,
  #       vjust = 1,
  #       hjust = 1
  #     ))
  # })
  # 
  # # response curves
  # 
  # # get list of variables
  # vars <- reactive({
  #   get(sdm_data())[["variable_importance"]]$Variable
  # })
  # 
  # rc <- reactive({
  #   get(sdm_data())[["response_curves"]]
  # })
  # 
  # output$response_plot <- renderPlot({
  #   plots <- map(vars(),
  #                ~ ggplot(rc(), aes(
  #                  x = !!sym(.x), y = !!sym(paste0("preds_", .x))
  #                )) +
  #                  geom_line() +
  #                  xlab(.x) +
  #                  ylab("Presence Probability"))
  #   
  #   wrap_plots(plots, ncol = 3)
  # })
  # 
  # 
  

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