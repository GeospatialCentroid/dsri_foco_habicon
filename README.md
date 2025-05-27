# Mapping habitat connectivity priorities throughout Fort Collins, Colorado

This is the working repository for the reproducible codebase and decision support tool funded by the Colorado State University Data Science Research Institute (DSRI) DISCOVER seed grant.

Our team has developed a reproducible data science pipeline that can model spatially explicit habitat priorities across any species, area and time frame. This novel methodology simultaneously considers both habitat suitability and connectivity characteristics to calculate land prioritization maps. Priority maps were modeled for 8 species of interest (4 birds and 4 pollinators) and integrated into a decision support tool (shiny app) that demonstrates our first application of these methods, mapping priority areas and corridors within the City of Fort Collins. The interactive web application also allows integrates these species priority maps with various spatial socioeconomic and demographic datasets for the city. The main goal of this decision-support tool is to make these findings more accessible to policymakers, stakeholders, and the public to better facilitate land use planning decisions and promote a socially equitable decision-making process.

Forward any questions regarding this project and code base to the PI, Caitlin Mothes (ccmothes\@colostate.edu).

## Workflow and Methods Overview

The workflow is divided into 1) process inputs, 2) execute habitat suitability models, 3) execute connectivity models, 4) execute R \`habicon\` priority models and 5) post-processing. The reproducible codebase can be seen on GitHub here: <https://github.com/GeospatialCentroid/dsri_foco_habicon.>  

### Process Inputs 

*Occurrence Data*

-   `01_process_occurrences.Rmd`

    -    `src/get_occ.R` 

Species occurrence data was retrieved from two main sources. The first source was bird and butterfly survey data that was collected as a part of a multi-year project with Nature in the City. This included occurrence data for all modeled species except for Bombus spp. The second source was occurrence data from public databases. This data was retrieved using the \`spocc\` R package (Owens et al. 2024) that currently interfaces with seven different biodiversity repositories (full list here: <https://docs.ropensci.org/spocc/>).  

*Environmental Predictors* 

-   `02_process_predictors`

    -    `src/tif_sep.R`

    -    `src/process_preds.R`

The full list of environmental predictor variables used in all models, with sources and metadata, can be found here: <https://github.com/GeospatialCentroid/dsri_foco_habicon/blob/main/docs/variable_metadata.md.> Predictors were processed to a resolution of 100m for the City of Fort Collins Growth Management Area, using a buffer around the area to account for edge effects in distance and percent cover calculations. 

### Habitat Suitability Models 

-   `03_execute_habitat_suitability.Rmd`

    -   `src/execute_SDM.R`

Habitat suitability models were used to map suitable habitat for each species across Fort Collins. Models were performed using MaxEnt (Phillips et al. 2006), a machine learning algorithm shown to have the highest predictive performance compared to other methods (Valavi et al. 2022). We executed models using the \`ENMEval\` R package (Kass et al. 2021). For each species, a total of 95 models were run using a combination of five different feature classes and 19 different regularization multipliers. The top performing model was chosen based on the highest average validation Continuous Boyce Index (Hirzel et al. 2006), a commonly used performance metric. Final models were also tested against null models to assess significant predictions. Model results for each species can be seen in the “Model Results” tab. 

### Connectivity Models 

-   `04_execute_connectivity.Rmd`

To map connectivity across Fort Collins for each species, we first calculated a resistance layer that reflects resistance to movement. For this study, we used the inverse of the habitat suitability maps modeled in the previous step at the resistance layer, which has shown to be effective in predicting animal movements (Zeller et al. 2018). We then calculated continuous connectivity maps using Omniscape (Landau et al. 2021), a newer model based upon Circuitscape (Anantharaman et al. 2020), an award-winning connectivity analysis software. Omniscape is better designed for spatially continuous input surfaces, which is the case for our resistance layers. The output maps represent the total current flowing through the landscape, where higher current reflects higher movement probability.  

### Priority Models 

-   `05_execute_habicon.Rmd`

The species maps shown in this application are the product of this final step. Habitat priority maps are calculated using an R package called \`habicon\`([https://github.com/ccmothes/habicon),](https://github.com/ccmothes/habicon),) developed by the PI on this project. The package calculates maps of habitat patch and corridor priority in terms of their degree of importance to ecological connectivity by integrating habitat suitability and connectivity maps. There are two main functions: patch priority and corridor priority. Patch priority identifies and ranks individual habitat patches based on three different connectivity metrics: quality-weighted area, weighted betweenness centrality, and the difference in ecological connectivity. This model takes into account species dispersal distances and identifies individual patches using the top 20% of suitability values. Dispersal distances for each species were retrieved from the literature and can be found in the table below. The corridor priority function weights all possible movement routes within identified corridors by mapping all the shortest paths from all possible points of origin between each pair of connected patches and weights each path based on the area, quality and weighted betweenness centrality with a negative effect of path distance. The final map output then sums, for each cell, the values for all paths that cross through that cell. Therefore, areas that have both higher quantity and quality of paths are given higher priority. 

------------------------------------------------------------------------

## References

Anantharaman, R., Hall, K., Shah, V. B., & Edelman, A. (2020). Circuitscape in Julia: High performance connectivity modelling to support conservation decisions. Proceedings of the JuliaCon Conferences, 1(1), 58. <https://doi.org/10.21105/jcon.00058> 

Hirzel, A. H., Le Lay, G., Helfer, V., Randin, C., & Guisan, A. (2006). Evaluating the ability of habitat suitability models to predict species presences. Ecological Modelling, 199: 142-152. 

Kass J, Muscarella R, Galante P, Bohl C, Buitrago-Pinilla G, Boria R, Soley-Guardia M, Anderson R (2021). “ENMeval 2.0: Redesigned for customizable and reproducible modeling of species’ niches and distributions.” Methods in Ecology and Evolution, 12(9), 1602-1608. doi:10.1111/2041-210X.13628 <https://doi.org/10.1111/2041-210X.13628>. 

Landau, V.A., V.B. Shah, R. Anantharaman, and K.R. Hall. 2021. Omniscape.jl: Software to compute omnidirectional landscape connectivity. Journal of Open Source Software, 6(57), 2829. 

Owens H, Barve V, Chamberlain S (2024). spocc: Interface to Species Occurrence Data Sources. R package version 1.2.3, [https://CRAN.R-project.org/package=spocc](https://cran.r-project.org/package=spocc). 

Phillips, S. J., R. P. Anderson, and R. E. Schapire. 2006. Maximum entropy modeling of species geographic distributions. Ecological Modelling 190: 231–259. 

Valavi, R., G. Guillera-Arroita, J. J. Lahoz-Monfort, and J. Elith. 2022. Predictive performance of presence-only species distribution models: a benchmark study with reproducible code. Ecological Monographs 92(1):e01486. 10.1002/ecm.1486 

Zeller, K. A., Jennings, M. K., Winston Vickers, T., Ernest, H. B., Cushman, S. A., Boyce, W. M. (2018). Are all data types and connectivity models created equal? Validating common connectivity approaches with dispersal data. Diversity and Distribution. 24:868–879. 
