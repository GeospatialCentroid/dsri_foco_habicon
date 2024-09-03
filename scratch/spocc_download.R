################################################################################
################################################################################
##################### Downloading Occurrences via SPOCC ########################
################################################################################
################################################################################

# libraries
library(spocc)
library(sf)
library(tidyverse)


# Load previous pre-processed data
spp <- read.csv("data/Nic_survey_occ_processed.csv")

# make it spatial
nic <- sf::st_as_sf(spp, coords = c("longitude", "latitude"), crs = 4326)
table(nic$species)

# Fort Collins
foco_GMA <- sf::st_read("data/gis/ft_collins_GMA_boundary.shp")
foco_GMA <- sf::st_transform(foco, st_crs(nic))

# Plot
plot(foco$geometry, axes = T) # boundary
plot(nic["species"], add = T, pch = 16, pal = sf.colors(length(unique(nic$species))))


# occ() // This can take some time with multiple species
spp.listA = c("Agelaius phoeniceus", "Tyrannus verticalis", "Sturnella neglecta",
              "Setophaga petechia", "Colias philodice", "Colias eurytheme",
              "Vanessa cardui") # replace Genus species
spp.occ <- occ(query = spp.listA, 
               from = c("gbif", "inat", "ecoengine", "vertnet", 
                        "bison", "ala", "idigbio", "obis"), # This will search occurrences in these databases
               limit = 20000, has_coords = T) # Requiring data has coordinates
spp.occ.df <- occ2df(spp.occ)

# make spatial
spp.raw <- occ2df(spp.occ) %>% # Load csv
  na.omit(spp.raw) %>% # omit NA values
  st_as_sf(., coords = c("longitude","latitude"), crs = st_crs(nic)) %>% # make spatial
  st_crop(., foco_GMA) # clip to FOCO GMA 


# crop to FOCO and plot
plot(st_geometry(spp.raw.clip), add = T, col = "green", pch = 14)

# save spocc data
write_sf(spp.raw.clip, "data/spocc/spp_raw_GMAclip.shp")
write.csv(spp.raw.clip, "data/spocc/spp_raw_GMA_clip.csv")



