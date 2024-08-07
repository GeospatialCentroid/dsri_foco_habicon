#' process_preds
#'
#' This function reads in a list of spatial predictor file paths and processes them for a niche
#' model. Specifically, it reads in raw files, sets their crs and extent to a define 'template'
#' file, and rasterizes the file if necessary.
#' 
#' @param input_path File pathway to the raw predictor spatial file
#' @param template_path File pathway to the 'template' file that will be used to standardize crs and ext
#' @param resolution Desired resolution for processed raster file output
#' @param distance Whether to calculate output as a 'distance to' metric (TRUE) or not (FALSE; default)
#' @param save Whether to save (TRUE) the resulting dataframe (as .tif) or not (FALSE)
#' @param output_path If `save = TRUE`, the file path to save the dataframe.
#'
#' @return A spatraster with crs and ext that is consistent with template and res that is set to desired resolution

process_preds <- function(predictor_path, template, resolution, distance=FALSE, save, output_path) {
  
  # Initialize variables to hold the processed objects
  rast_shapefile <- NULL
  raster_file <- NULL
  output_object <- NULL
  
  if (grepl("\\.shp$", predictor_path, ignore.case = TRUE)) {
    # Read in shapefile
    shapefile <- st_read(predictor_path)
    
    # Set CRS to match the template
    shapefile <- st_transform(shapefile, crs = crs(template))
    
    # Set extent to match the template (crop if necessary)
    shapefile <- st_crop(shapefile, st_bbox(template))
    
    ## Rasterize the shapefile
    # Create an empty raster template with the specified resolution
    raster_template <- rast(ext(template), resolution = resolution)
    # Rasterize shapefile 
    rast_shapefile <- terra::rasterize(shapefile, raster_template, fun = mean)
    # Set CRS to match the shapefile
    crs(rast_shapefile) <- st_crs(shapefile)$proj4string
    
    if(distance == TRUE) {
      rast_shapefile <- terra::distance(rast_shapefile)
    }
    
    # Extract filename without extension for naming 
    file_name <- tools::file_path_sans_ext(basename(predictor_path))
    
    # Store the processed object with the filename
    output_object <- rast_shapefile
    
  } else if (grepl("\\.tif$", predictor_path, ignore.case = TRUE)) {
    # Read in raster and template files
    raster_file <- rast(predictor_path)
    
    # Set template temporarily to raster CRS
    shapefile_temp <- st_transform(template, crs = crs(raster_file))
    raster_file <- crop(raster_file, shapefile_temp)
    
    # Project cropped raster into template CRS
    raster_file <- project(raster_file, crs(template), res = resolution)
    
    if(distance == TRUE) {
      raster_file <- terra::distance(raster_file)
    }
    
    # Extract filename without extension for naming
    file_name <- tools::file_path_sans_ext(basename(predictor_path))
    
    # Store the processed object with the filename
    output_object <- raster_file
    
  } else {
    stop("Unsupported file type.")
  }
  
  # Save the processed objects to the specified directory if 'save' is TRUE
  if (save) {
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    
    if (!is.null(rast_shapefile)) {
      # Write raster file
      writeRaster(rast_shapefile, filename = file.path(output_path, paste0(file_name, ".tif")), overwrite = TRUE)
    }
    
    if (!is.null(raster_file)) {
      # Write raster file
      writeRaster(raster_file, filename = file.path(output_path, paste0(file_name, ".tif")), overwrite = TRUE)
      
    }
  }
  
  # Assign the output_object to the global environment with the filename as the variable name
  #assign(file_name, output_object, envir = .GlobalEnv)
  
  # Return the processed object with the filename
  return(output_object)
} 