# some code for making static maps for presentations

library(tmap)
library(sf)
library(maptiles)
library(tigris)


source("setup.R") 

# read in spatial layer that includes full extent of interest, in this case a boundary .shp
aoi <- read_sf("data/nature_in_the_city/gis/ft_collins_GMA_boundary.shp")

## Download tiles and compose basemap raster (high res, takes a few seconds)
tile_maps <- get_tiles(x = aoi, provider =  "CartoDB.Positron", zoom = 12)


# load in maps
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




#  patches
tm_shape(tile_maps) +
  tm_rgb() +
  tm_shape(patch_maps$Tyrannus_verticalis$qwa) +
  tm_raster(
    palette = "YlGn",
    title = "Quality-Weighted Area",
    alpha = 0.9,
    style = "cont",
    labels = c("Low", "High")
  ) +
  tm_layout(legend.position = c("left", "top"),
            legend.width = 1,
            legend.text.size = 2,  # Increase legend text size
            legend.title.fontface = "bold",
            legend.title.size = 3) +
  tm_compass() +
  tm_scale_bar()


# corridors
tm_shape(tile_maps) +
  tm_rgb() +
  tm_shape(corr_maps$Tyrannus_verticalis) +
  tm_raster(
    palette = "Oranges",
    title = "Corridor Priority",
    alpha = 0.9,
    style = "cont",
    labels = c("Low", "", "High")
  ) +
  tm_layout(legend.position = c("left", "top"),
            legend.width = 1,
            legend.text.size = 2,  # Increase legend text size
            legend.title.fontface = "bold",
            legend.title.size = 3) +
  tm_compass() +
  tm_scale_bar()



# FoCo Site Map

# add more layers
states <- tigris::states(cb = TRUE)

co <- states %>% 
  dplyr::filter(NAME == "Colorado")

nat_areas <- st_read("data/input_raw/Natural_Areas/Natural_Areas.shp") %>% 
  st_crop(st_bbox(st_transform(aoi, st_crs(.))))


## Download tiles and compose basemap raster (high res, takes a few seconds)
tiles_co <- get_tiles(x = co, provider =  "Esri.WorldShadedRelief", zoom = 8)
tiles_foco <- get_tiles(x = aoi, provider =  "Esri.WorldTopoMap", zoom = 12)
tiles_sat <- get_tiles(x = st_buffer(aoi, 1000), provider =  "Esri.WorldImagery", zoom = 12)

tmap_mode("plot")

#co map
tm_shape(tiles_co) +
  tm_rgb() +
  tm_shape(states)+
  tm_borders(col = "black")

# foco map
tm_shape(tiles_sat) +
  tm_rgb()  +
   tm_shape(aoi) +
   tm_polygons(col = "white", fill_alpha = 0, lwd = 3) +
  tm_shape(nat_areas) +
  tm_polygons(fill = "blue", fill_alpha = 0.75) +
  tm_compass()+
  tm_scalebar()
