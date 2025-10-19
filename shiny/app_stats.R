# code to calcualte stats for app dashboard

library(sf)
library(tidyverse)
library(terra)


# patch priority
patch_maps <- map(
  list.files(
    "shiny/app_data/output_habicon/",
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
    "shiny/app_data/output_habicon/",
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
patch_all <- terra::rast("shiny/app_data/output_habicon/patch_priority_all.tif")
corr_all <- terra::rast('shiny/app_data/output_habicon/corridor_priority_all.tif')

# Calculate areas

patch_areas <- map(patch_maps, function(f) {
  r <- f$dECA
  non_na_cells <- global(r, fun = function(x) sum(!is.na(x)))
  non_na_cells * 100 * 100 / 1e6  # area in km²
}) %>% 
  bind_rows()

corr_areas <- map(corr_maps, function(f) {
  non_na_cells <- global(f, fun = function(x) sum(!is.na(x)))
  non_na_cells * 100 * 100 / 1e6  # area in km²
}) %>% 
  bind_rows()


# Create a dataframe with results
results <- tibble(
  species = names(patch_maps),
  patch_area_km2 = patch_areas$global,
  corr_area_km2 = corr_areas$global
)
