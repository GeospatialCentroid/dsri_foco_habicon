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

tif_sep <- function(raster_path, lookup_table, output_path) {
  # read in McHale raster
  raster_data <- rast(raster_path)
  
  # create list to store values
  layer_list <- list()
  
  # get unique values
  unique_values <- terra::freq(raster_data)$value
  
  # loop through each unique value and create separate layers
  for (value in unique_values) {
    # run check if the value exists in the lookup table
    if (value %in% lookup_table$category_value) {
      #  binary numeric raster for each category 
      category_layer <- classify(raster_data == value, cbind(TRUE, 1), others = NA)
      
      # assign name to each layer based on lookup table
      category_name <- paste0("lc_", lookup_table$land_cover[lookup_table$category_value == value])
      
      # Check if the length of the name is valid for the raster layer
      print(paste("Category:", value, "Name:", category_name))  # Debugging output
      
      # Set the name of the layer using `names()` function
      names(category_layer) <- category_name
      
      # Store the layer in the list
      layer_list[[category_name]] <- category_layer
    } else {
      print(paste("Warning: No match found for value", value))  # Handle unexpected values
    }
  }
  
  # Optionally, write each layer to a new file
  for (layer_name in names(layer_list)) {
    # Ensure the output file path has the correct file extension (.tif)
    output_path <- paste0(output_dir, "/", layer_name, ".tif")
    
    # Write each layer to a new file, specifying the output file path
    writeRaster(layer_list[[layer_name]], filename = output_path, overwrite = TRUE)
    
    print(paste("Written:", output_path))  # Optional: Print to confirm the file was written
  }
}