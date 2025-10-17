library(shiny)
library(leaflet)
library(bslib)
library(dplyr)
library(sf)
library(tidyverse)
library(terra)
library(leaflegend)
library(shinyWidgets)
library(DT)
library(ENMeval)
library(patchwork)

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
  palette = c("#fffbeb", "#fcd34d", "#fbbf24", "#f59e0b", "#d97706", "#b45309","#92400e"),
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
map(list.files("app_data/output_sdm/", full.names = TRUE), ~load(.x, envir = .GlobalEnv))


# NA LEGEND FIX
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))


# UI ------------------------------------------------------
ui <- navbarPage(
  id = "nav",
  title = HTML("Fort Collins Decision Support Tool <em>(beta version)</em>"),
  theme = bs_theme(bootswatch = "litera"),
  # Map ----------
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
      layout_columns(
        col_widths = c(8, 4),
        
      #div(
        #style = "height: calc(100vh - 60px); overflow-y: auto;",
        card(
          full_screen = TRUE,
          height = "800px",
          #card_header("Interactive Map"),
          leafletOutput("map", height = "100%"),
          HTML(html_fix)
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
        
        # card(
        #   height = "400px",
        #   card_header("Area Statistics"),
        #   div(style = "padding: 15px;", textOutput("stats_text")),
        #   div(style = "height: 300px; padding: 15px;", plotOutput("priority_plot", height = "100%"))
        # )
      )
    )
  ),
  # About -------------
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
      p(strong("Disclaimer:"), "This application is currently in its", em("beta"), "version. As this tool is the product of a seed grant, there are aspects that could use updates and improvements, such as outdated predictor datasets. However, it is important to note that all species models reported high model performance."),
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

  # Methods --------------
  tabPanel(title = "Methods",
           card(id = "glossary", card_header(h4("Glossary")), card_body(
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
                 a patch's importance to the total amount and quality of reachable habitat in the landscape."
               )
             ),
           )
           ),
           # Analytical Overview Section
           card(id = "analytical_overview", card_header(h4("Analytical Methods Overview")), card_body(
             p("The workflow is divided into 1) process inputs, 2) execute habitat suitability models, 3) execute connectivity models, 4) execute R 'habicon' priority models and 5) post-processing. The reproducible codebase can be seen on GitHub here:"),
             p(a("https://github.com/GeospatialCentroid/dsri_foco_habicon", href = "https://github.com/GeospatialCentroid/dsri_foco_habicon", target = "_blank"))
           ),
           
           # Process Inputs Section
           card(id = "process_inputs", card_header(h5("1) Process Inputs")), card_body(
             h6("Occurrence Data"),
             p("Species occurrence data was retrieved from two main sources. The first source was bird and butterfly survey data that was collected as a part of a multi-year project with Nature in the City. This included occurrence data for all modeled species except for Bombus spp. The second source was occurrence data from public databases. This data was retrieved using the 'spocc' R package (Owens et al. 2024) that currently interfaces with seven different biodiversity repositories (full list here: ", 
               a("https://docs.ropensci.org/spocc/", href = "https://docs.ropensci.org/spocc/", target = "_blank"), ")."),
             
             h6("Environmental Predictors"),
             p("The full list of environmental predictor variables used in all models, with sources and metadata, can be found here: ",
               a("Variable Metadata", href = "https://github.com/GeospatialCentroid/dsri_foco_habicon/blob/main/docs/variable_metadata.md", target = "_blank"), 
               ". Predictors were processed to a resolution of 100m for the City of Fort Collins Growth Management Area, using a buffer around the area to account for edge effects in distance and percent cover calculations.")
           )),
           
           # Species Distribution Models Section
           card(id = "sdm_section", card_header(h5("2) Habitat Suitability Models")), card_body(
             p("Habitat suitability models were used to map suitable habitat for each species across Fort Collins. Models were performed using MaxEnt (Phillips et al. 2006), a machine learning algorithm shown to have the highest predictive performance compared to other methods (Valavi et al. 2022). We executed models using the 'ENMEval' R package (Kass et al. 2021). For each species, a total of 95 models were run using a combination of five different feature classes and 19 different regularization multipliers. The top performing model was chosen based on the highest average validation Continuous Boyce Index (Hirzel et al. 2006), a commonly used performance metric. Final models were also tested against null models to assess significant predictions. Model results for each species can be seen in the \"Model Results\" tab.")
           )),
           
           # Connectivity Models Section
           card(id = "connectivity_section", card_header(h5("3) Connectivity Models")), card_body(
             p("To map connectivity across Fort Collins for each species, we first calculated a resistance layer that reflects resistance to movement. For this study, we used the inverse of the habitat suitability maps modeled in the previous step at the resistance layer, which has shown to be effective in predicting animal movements (Zeller et al. 2018). We then calculated continuous connectivity maps using Omniscape (Landau et al. 2021), a newer model based upon Circuitscape (Anantharaman et al. 2020), an award-winning connectivity analysis software. Omniscape is better designed for spatially continuous input surfaces, which is the case for our resistance layers. The output maps represent the total current flowing through the landscape, where higher current reflects higher movement probability.")
           )),
           
           # Priority Models Section
           card(id = "priority_models", card_header(h5("4) Priority Models")), card_body(
             p("The species maps shown in this application are the product of this final step. Habitat priority maps are calculated using an R package called 'habicon' (", 
               a("https://github.com/ccmothes/habicon", href = "https://github.com/ccmothes/habicon", target = "_blank"), 
               "), developed by the PI on this project. The package calculates maps of habitat patch and corridor priority in terms of their degree of importance to ecological connectivity by integrating habitat suitability and connectivity maps."),
             
             p("There are two main functions: patch priority and corridor priority. Patch priority identifies and ranks individual habitat patches based on three different connectivity metrics: quality-weighted area, weighted betweenness centrality, and the difference in ecological connectivity. This model takes into account species dispersal distances and identifies individual patches using the top 20% of suitability values. Dispersal distances for each species were retrieved from the literature and can be found in the table below."),
             
             p("The corridor priority function weights all possible movement routes within identified corridors by mapping all the shortest paths from all possible points of origin between each pair of connected patches and weights each path based on the area, quality and weighted betweenness centrality with a negative effect of path distance. The final map output then sums, for each cell, the values for all paths that cross through that cell. Therefore, areas that have both higher quantity and quality of paths are given higher priority.")
           ))),
           
           # Movement Distance Table Section
           card(id = "movement_table", card_header(h4("Species Movement Distances")), card_body(
             p("Table 1. The mean movement distance for each indicator species and the scientific literature and expert sources for the home range sizes and foraging distances used to estimate the mean movement distance. For Bombus species, we took the mean of all maximum reported distances for all species and castes."),
             
             div(style = "overflow-x: auto;",
                 tags$table(class = "table table-striped", style = "margin-top: 15px;",
                            tags$thead(
                              tags$tr(
                                tags$th("Species", style = "font-weight: bold;"),
                                tags$th("Mean Movement Distance", style = "font-weight: bold;"),
                                tags$th("Sources", style = "font-weight: bold;")
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td("Red-winged Blackbird"),
                                tags$td("421.8 m"),
                                tags$td("Meanley 1965; Picman 1987; Orians and Christman 1968; Rocky Mountain Bird Observatory; Searcy and Yasukawa 1995; White et al. 1985")
                              ),
                              tags$tr(
                                tags$td("Western Kingbird"),
                                tags$td("305.0 m"),
                                tags$td("Cuesta 1974; Blancher and Robertson 1985; Holmes and Morgan 2004; Rocky Mountain Bird Observatory")
                              ),
                              tags$tr(
                                tags$td("Western Meadowlark"),
                                tags$td("280.7 m"),
                                tags$td("Kendeigh 1941; Rocky Mountain Bird Observatory; Schaeff and Picman 1988")
                              ),
                              tags$tr(
                                tags$td("Yellow Warbler"),
                                tags$td("114.9 m"),
                                tags$td("Ficken and Ficken 1966; Kendeigh 1941; Lowther et al. 1999; Rocky Mountain Bird Observatory")
                              ),
                              tags$tr(
                                tags$td("Clouded Sulphur"),
                                tags$td("804.7 m"),
                                tags$td("Paul Opler")
                              ),
                              tags$tr(
                                tags$td("Orange Sulphur"),
                                tags$td("3,218.7 m"),
                                tags$td("Paul Opler")
                              ),
                              tags$tr(
                                tags$td("Painted Lady"),
                                tags$td("16,093.4 m"),
                                tags$td("Paul Opler")
                              ),
                              tags$tr(
                                tags$td("Bombus spp."),
                                tags$td("2172 m"),
                                tags$td("Mola and Williams 2025")
                              )
                            )
                 )
             )
           )),
           
           # References Section
           card(id = "references", card_header(h4("References")), card_body(
             div(style = "font-size: 0.9em; line-height: 1.4;",
                 p("Anantharaman, R., Hall, K., Shah, V. B., & Edelman, A. (2020). Circuitscape in Julia: High performance connectivity modelling to support conservation decisions. Proceedings of the JuliaCon Conferences, 1(1), 58. https://doi.org/10.21105/jcon.00058"),
                 
                 p("Hirzel, A. H., Le Lay, G., Helfer, V., Randin, C., & Guisan, A. (2006). Evaluating the ability of habitat suitability models to predict species presences. Ecological Modelling, 199: 142-152."),
                 
                 p("Kass J, Muscarella R, Galante P, Bohl C, Buitrago-Pinilla G, Boria R, Soley-Guardia M, Anderson R (2021). \"ENMeval 2.0: Redesigned for customizable and reproducible modeling of species' niches and distributions.\" Methods in Ecology and Evolution, 12(9), 1602-1608. doi:10.1111/2041-210X.13628"),
                 
                 p("Landau, V.A., V.B. Shah, R. Anantharaman, and K.R. Hall. 2021. Omniscape.jl: Software to compute omnidirectional landscape connectivity. Journal of Open Source Software, 6(57), 2829."),
                 
                 p("Owens H, Barve V, Chamberlain S (2024). spocc: Interface to Species Occurrence Data Sources. R package version 1.2.3, https://CRAN.R-project.org/package=spocc."),
                 
                 p("Phillips, S. J., R. P. Anderson, and R. E. Schapire. 2006. Maximum entropy modeling of species geographic distributions. Ecological Modelling 190: 231-259."),
                 
                 p("Valavi, R., G. Guillera-Arroita, J. J. Lahoz-Monfort, and J. Elith. 2022. Predictive performance of presence-only species distribution models: a benchmark study with reproducible code. Ecological Monographs 92(1):e01486. 10.1002/ecm.1486"),
                 
                 p("Zeller, K. A., Jennings, M. K., Winston Vickers, T., Ernest, H. B., Cushman, S. A., Boyce, W. M. (2018). Are all data types and connectivity models created equal? Validating common connectivity approaches with dispersal data. Diversity and Distribution. 24:868-879.")
             )
           ))
          
  ),
  
  # Results --------------
  tabPanel(title = "Species Model Results",
           h4("Species habitat suitability model results"),
           hr(),
           sidebarLayout(
             sidebarPanel(width = 3,
               selectInput(
                 "sdm_species",
                 "Select a Species:",
                 choices = species_vect
               ),
             ),
             mainPanel(
               h4("Model Performace"),
               h5("Null Models"),
               p(
                 "Empirical results were tested against a set of 100 simulated null models. Significant p-values represent empirical results that were significantly better than a null model."
               ),
               tableOutput("null_table"),
               p(
                 "High model performance is represented by empirical validation values (red points) that are significantly higher than the 99th quantile of the null values (first line in the violin plot)."
               ),
               plotOutput("null_plot"),
               h5("Tuning Results"),
               p("The top model was chosen based on the highest average validation Continuous Boyce Index (CBI). The AUC diff and 10% omission rate were used in the case of ties. CBI values closer to 1 represent higher performance, whereas smaller values for AUC diff and 10% omission rate indicate higher performance."),
               plotOutput("tuning_plot"),
               hr(),
               h4("Variable Importance"),
               plotOutput("varimp_plot"),
               h4("Response Curves"),
               plotOutput("response_plot"),
               hr(),
               h4("Metadata"),
               p("Technical details reguarding model parameters and model selection."),
               DTOutput("meta_table")
             )
           )
  )
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
        #palette = "YlGn",
        #palette = c("#86efac", "#22c55e", "#166534", "#0f2419"),
        palette = c("#d1fae5", "#86efac", "#4ade80", "#22c55e", "#15803d", "#166534", "#14532d"),
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
      #palette = c("#cafad9", "#86efac", "#22c55e", "#166534", "#0f2419"),
      palette = c("#d1fae5", "#86efac", "#4ade80", "#22c55e", "#15803d", "#166534", "#14532d", "#0f2419"),
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
      #palette = c("#fef3c7", "#fcd34d", "#f59e0b", "#d97706", "#92400e"),
      palette = c("#fffbeb", "#fcd34d", "#fbbf24", "#f59e0b", "#d97706", "#b45309","#92400e"),
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
  # reactive RData object
  sdm_data <- reactive({
    paste0(input$sdm_species, "_SDM_output")

  })

  # metadata plot
  output$meta_table <- renderDT({
    get(sdm_data())[["metadata"]] %>%
      filter(name != "projection") %>%
      datatable(
        options = list(
          scrollY = "400px",
          scrollCollapse = TRUE,
          paging = FALSE,
          dom = "frti"  # Controls which elements appear (no pagination)
        )
      )


  })

  # tuning plots
  output$tuning_plot <- renderPlot({
     p <- evalplot.stats(
      e = get(sdm_data())[["all_mods"]],
      stats = c("cbi.val", "auc.diff", "or.10p"),
      color = "fc",
      x.var = "rm",
      error.bars = FALSE
      #dodge = 0.5
    )
     
     p +
       theme(strip.text = element_text(size = 14),
             axis.text = element_text(size = 12),
             axis.title = element_text(size = 12))
  })

   # null model stats
  output$null_table <- renderTable({
    null.emp.results(get(sdm_data())[["null_mods"]])

  })


  # null model plots
  output$null_plot <- renderPlot({
    p <- evalplot.nulls(
      get(sdm_data())[["null_mods"]],
      stats = c("cbi.val", "auc.val" #"or.10p", "auc.val", 
                ),
      plot.type = "violin"
    )
    
    p +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))

  })

  # variable importance plots
  output$varimp_plot <- renderPlot({
    ggplot(get(sdm_data())[["variable_importance"]], aes(
      x = reorder(Variable, -Permutation_importance),
      y = Permutation_importance
    )) +
      geom_col() +
      geom_errorbar(aes(ymin = Permutation_importance - sd, ymax = Permutation_importance + sd),
                    width = 0.2) +
      xlab("") +
      theme(axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.text = element_text(size = 14))
  })

  # response curves

  # get list of variables
  vars <- reactive({
    get(sdm_data())[["variable_importance"]]$Variable
  })

  rc <- reactive({
    get(sdm_data())[["response_curves"]]
  })

  output$response_plot <- renderPlot({
    plots <- map(vars(),
                 ~ ggplot(rc(), aes(
                   x = !!sym(.x), y = !!sym(paste0("preds_", .x))
                 )) +
                   geom_line() +
                   xlab(.x) +
                   ylab("Presence Probability"))

    wrap_plots(plots, ncol = 3)
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