#' process_preds
#'
#' This function reads in a list of spatial predictor file paths and processes them for a niche
#' model. Specifically, it reads in raw files, sets their crs and extent to a define 'aoi'
#' file, and rasterizes the file if necessary.
#' 
#' @param input_path File pathway to the raw predictor spatial file
#' @param aoi_path File pathway to the 'aoi' file that will be used to standardize crs and ext
#' @param resolution Desired resolution for processed raster file output
#' @param type Categorical argument that specifies the landscape metric calculation ("perccov" = percent cover;
#' @param dist Buffer distance (in m) to add around the AOI (useful for dist and perc cover calculations)
#' @param buffer If type="perccov", the window size for focal statistics. Must be an odd number. Default is 3, which means a square of all the touching cells.
#' @param save Whether to save (TRUE) the resulting dataframe (as .tif) or not (FALSE)
#' @param output_path If `save = TRUE`, the file path to save the dataframe.
#'
#' @return A spatraster with crs and ext that is consistent with aoi and res that is set to desired resolution

# write function that reads in predictors and matches CRS and extent to the aoi
process_preds <- function(predictor_path, aoi, resolution, type, dist = 1000, buffer = 3, field = NULL, save, output_path) {
  
  # buffer the aoi
  aoi <- st_buffer(aoi, dist = dist)
  
  # Initialize variables to hold the processed objects
  rast_shapefile <- NULL
  raster_file <- NULL
  output_object <- NULL
  
  
  
  ## SHAPEFILE ----------------------------------
  
  if (grepl("\\.shp$", predictor_path, ignore.case = TRUE)) {
    # Read in shapefile
    shapefile <- st_read(predictor_path)
    
    # Set CRS to match the aoi
    shapefile <- st_transform(shapefile, crs = crs(aoi))
    
    # Set extent to match the aoi (crop if necessary)
    shapefile <- st_crop(shapefile, st_bbox(aoi))
    
    # Rasterize the shapefile
    
    ## Create an empty raster aoi with the specified resolution
    raster_aoi <- rast(ext(aoi), resolution = resolution, crs = crs(aoi))
    
    # Rasterize shapefile
    
    ## field is given
    if (is.null(field)) {
      rast_shapefile <- terra::rasterize(shapefile, raster_aoi, fun = mean)
    } else {
      rast_shapefile <- terra::rasterize(shapefile, raster_aoi, field = field, fun = mean)
    }
    # Set CRS to match the shapefile
    #crs(rast_shapefile) <- st_crs(shapefile)$proj4string
    
    
    
    ### DISTANCE ------------------------------------
    
    # Perform calculations based on 'type' argument
    if (type == "dist") {
      rast_shapefile <- terra::distance(rast_shapefile)
      
    ## PERCENT COVER -------------------------------
    } else if (type == "perccov") {
      if (is.null(buffer)) {
        stop("Buffer must be specified when calculating percent cover.")
      }
      # Calculate percent cover within the buffer distance
      rast_shapefile <- terra::focal(
        rast_shapefile,
        w = buffer,
        fun = function(x)
          mean(!is.na(x), na.rm = TRUE) * 100
      )
    }

    
    # Extract filename without extension for naming
    file_name <- tools::file_path_sans_ext(basename(predictor_path))
    
    # Store the processed object with the filename
    output_object <- rast_shapefile
    
    names(output_object) <- file_name
    
    
    ## GEOTIFF -------------------------------------------
    
  } else if (grepl("\\.tif$", predictor_path, ignore.case = TRUE)) {
    # Read in raster and aoi files
    raster_file <- rast(predictor_path)
    
    # Set aoi temporarily to raster CRS - so we don't need to project full, large rasters
    shapefile_temp <- st_transform(aoi, crs = crs(raster_file))
    
    # crop to the temp shapefile
    raster_file <- crop(raster_file, shapefile_temp)
    
    # Convert the aoi to a raster with the same extent and resolution
    aoi_raster <- rast(ext(aoi), resolution = resolution, crs = crs(aoi))
    
    # Project cropped raster into aoi CRS
    raster_file <- project(raster_file, aoi_raster)
    
    # Perform calculations based on 'type' argument
    
    ### DISTANCE ----------------------------------------
    if (type == "dist") {
      raster_file <- terra::distance(raster_file)
      
    ### PERCENT COVER -----------------------------------
    } else if (type == "perccov") {
      if (is.null(buffer)) {
        stop("Buffer must be specified when calculating percent cover.")
      }
      # Calculate percent cover within the buffer distance
      raster_file <- terra::focal(
        raster_file,
        w = buffer,
        fun = function(x)
          mean(!is.na(x), na.rm = TRUE) * 100
      )
    }
    
    # Extract filename without extension for naming
    file_name <- tools::file_path_sans_ext(basename(predictor_path))
    
    # Store the processed object with the filename
    output_object <- raster_file
    
    names(output_object) <- file_name
    
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
      writeRaster(
        rast_shapefile,
        filename = file.path(output_path, paste0(file_name, ".tif")),
        overwrite = TRUE
      )
    }
    
    if (!is.null(raster_file)) {
      # Write raster file
      writeRaster(raster_file,
                  filename = file.path(output_path, paste0(file_name, ".tif")),
                  overwrite = TRUE)
    }
  }
  
  # Return the processed object with the filename
  return(output_object)
}