#' tif_sep
#'
#' This function reads in a raster with multiple layers represented in a band and separates
#' them into distinct layers.
#'
#' @param raster_path File pathway to the raw predictor spatial file that contains multiple layers
#' @param lookup_table Table that relates numerical/categorical layer name to land cover type
#' e.g.: 
# lookup_table <- data.frame(
# category_value = c(1, 2, 3, 4, 5, 6, 7),
# land_cover = c("TreeCanopy", "Grass_Shrubs", "BareSoil", "Water", "Buildings", "Roads_Railroads", "OtherPaved")
# )
#'
#' @param output_path File/directory pathway where you would like to save output
#'
#' @return A spatraster with crs and ext that is consistent with template and res that is set to desired resolution

tif_sep <- function(raster_path, lookup_table, output_path, save = TRUE) {
  # check if output path exists, create if not
  if (save) {
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
      print(paste("Created directory:", output_path))
    }
  }
  
  # read in raster
  raster_data <- rast(raster_path)
  
  # create list to store values
  layer_list <- list()
  
  # get unique values, removing NA
  unique_values <- na.omit(terra::freq(raster_data)$value)
  
  # loop through each unique value and create separate layers
  for (value in unique_values) {
    # run check if the value exists in the lookup table
    if (value %in% lookup_table$category_value) {
      # binary numeric raster for each category
      category_layer <- ifel(raster_data == value, 1, NA)
      
      # assign name to each layer based on lookup table
      category_name <- paste0("lc_", lookup_table$land_cover[lookup_table$category_value == value])
      
      # sanitize category name
      category_name <- gsub("[^a-zA-Z0-9_]", "_", category_name)
      
      # set the name of the layer
      names(category_layer) <- category_name
      
      # store the layer in the list
      layer_list[[category_name]] <- category_layer
    } else {
      print(paste("Warning: No match found for value", value))  # handle unexpected values
    }
  }
  
  # write each layer to a new file
  if (save) {
    for (layer_name in names(layer_list)) {
      # construct output file path
      output_file <- file.path(output_path, paste0(layer_name, ".tif"))
      
      # write each layer to a new file
      writeRaster(layer_list[[layer_name]], filename = output_file, overwrite = TRUE)
      
      print(paste("Written:", output_file))  
    }
  }
} 
