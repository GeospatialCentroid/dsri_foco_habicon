# general setup script for all workflows


# install (if necessary) and load all required packages ----------------

packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }


# vector of packages to load (add new packages to the end of this list)
packages <- c(
  'sf',
  "mapview",
  "ggpubr",
  "plotly",
  "tigris",
  "terra",
  "stars",
  "remotes",
  'devtools',
  "stars",
  "janitor",
  "akima",
  "tmap",
  "spocc",
  "furrr",
  "ENMeval",
  "SDMtune",
  "ini",
  "tidycensus")

# tack tidyverse on the end always to keep tidyverse functions as default for function conflicts
packages <- c(packages, "tidyverse")


packageLoad(packages)


# source all functions --------------------------

purrr::map(list.files(
  path = "src/",
  pattern = "*.R",
  full.names = TRUE,
  recursive = TRUE
),
source)
