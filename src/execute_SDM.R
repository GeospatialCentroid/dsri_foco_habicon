#' Run Maxent model and save all results
#'
#' This function is mostly a wrapper around `ENMeval::ENMevaluate()` and 
#' subsequent steps laid out in the vignette: https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html
#' 
#' @param species The scientific name of the species running the models for
#' @param occ A data frame of occurrences with only the "species, latitude, longitude" columns
#' @param predictors A SpatRaster object of all processed predictor variables 
#' @param features A list of all feature classes to compare. Default is all avaialble feature classes.
#' @param rm Vector of numbers of regularization multipliers to compare. Default is 1-10 in 0.5 increments.
#' @param save Logical; Whether to save the resulting list as an .RData file or not.
#' @param output_path If `save = TRUE`, the file path to save the .RData file
#'
#' @return  A list object with all model results of interest
execute_SDM <- function(species, occ, predictors, features = c("L", "LQ", "LQP", "H", "LQHP"), rm = seq(1, 10, 0.5),
                        save = FALSE, output_path) {
  
  # Prepare Maxent inputs ----
  
  ## Background points
  bg <- dismo::randomPoints(raster::raster(predictors), n = 10000) %>% as.data.frame() %>% 
    rename(longitude = x, latitude = y) %>% 
    st_as_sf(coords = c("longitude","latitude"), crs = crs(predictors))
  
  ## Model -specific requirements
  # convert points to a df of just lat/long for model input
  occ_mod <- occ_sf %>%
    mutate(latitude = st_coordinates(.)[, "Y"], longitude = st_coordinates(.)[, "X"]) %>%
    dplyr::select(longitude, latitude) %>%
    st_drop_geometry()
  
  bg_mod <- bg %>%
    mutate(latitude = st_coordinates(.)[, "Y"], longitude = st_coordinates(.)[, "X"]) %>%
    dplyr::select(longitude, latitude) %>%
    st_drop_geometry()
  
  # convtert SpatRaster to RasterStack
  preds_mod <- raster::brick(predictors)
  
  # Run Maxent ----
  
  all_mods <- ENMevaluate(
    occs = occ_mod,
    envs = preds_mod,
    bg = bg_mod, #can also leave this out and set 'n.bg = ##' instead
    algorithm = 'maxnet',
    partitions = 'randomkfold',
    taxon.name = species,
    tune.args = list(
      fc = features,
      rm = rm
    ),
    parallel = TRUE
  )
  
  # Model Selection ----
  
  # top model based on boyce index, auc diff and 10% omission rate used for ties
  top_mod_args <- eval.results(all_mods) %>%
    filter(cbi.val.avg == max(cbi.val.avg, na.rm = TRUE)) %>% 
    filter(auc.diff.avg == max(auc.diff.avg, na.rm = TRUE)) %>% 
    filter(or.10p.avg == min(or.10p.avg, na.rm = TRUE))
  
  selected_mod <- eval.models(all_mods)[[top_mod_args$tune.args]]
  
  # Model Predictions ----
  
  predicted_mod <- eval.predictions(all_mods)[[top_mod_args$tune.args]]
  
  
  # Null Models ----
  
  null_mod <- ENMnulls(
    e = all_mods,
    mod.settings = list(fc = as.character(top_mod_args$fc), rm = as.numeric(top_mod_args$rm)),
    no.iter = 100
  )
  
  # Compile Metadata
  
  rmm <- eval.rmm(all_mods)
  
  output_metadata <- tibble(
    #variable_names = I(list(rmm$data$environment$variableNames)),
    projection = rmm$data$environment$projection,
    species = rmm$data$occurrence$taxon,
    sample_size = rmm$data$occurrence$presenceSampleSize,
    background_sample_size = rmm$data$occurrence$backgroundSampleSize,
    prediction_units = "suitability (cloglog transformation)",
    min_prediction = raster::cellStats(predicted_mod, min),
    max_prediction = raster::cellStats(predicted_mod, max),
    selection_rules = "highest correlation Boyce index with lowest 10 percentile omission rate and AUC diff to break ties"
  ) %>% 
    bind_cols(top_mod_args) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = everything())
  
  ### Raw response curve data
  # function to pull rc data for each variable
  get_rc <- function(v,
                     mod,
                     levels = unlist(mod$levels[v]),
                     mm = mod$samplemeans,
                     min = mod$varmin[v],
                     max = mod$varmax[v]) {
    nr <- if (is.null(levels)) {
      100
    } else {
      length(levels)
    }
    
    m <- data.frame(matrix(mm, nr, length(mm), byrow = T))
    colnames(m) <- names(mm)
    
    m[, v] <- if (!is.null(levels)) {
      levels
    } else {
      seq(min - 0.1 * (max - min), max + 0.1 * (max - min), length = nr)
    }
    
    preds <- predict(mod, m, type = "cloglog")
    
    output <- tibble(preds = preds[, 1], val = m[, v]) %>%
      rename("preds_{v}" := preds, !!sym(v) := val)
    
    return(output)
    
  }
  
  # create df for all variables
  response_curves <- map(names(selected_mod$samplemeans), ~get_rc(v = .x, mod = selected_mod)) %>% 
    bind_cols()
  
  
  # Save final output
  final_output <- list(
    metadata = output_metadata,
    tuning_results = eval.results(all_mods),
    tuning_results_plot = evalplot.stats(
      e = all_mods,
      stats = c("auc.diff", "cbi.val", "or.10p"),
      color = "fc",
      x.var = "rm",
      error.bars = FALSE),
    prediction_map = predicted_mod,
    selected_mod = selected_mod,
    response_curves = response_curves,
    null_models = null.emp.results(null_mod),
    null_model_plots = evalplot.nulls(null_mod, stats = c("or.10p", "auc.val", "cbi.val"), plot.type = "histogram")
  )
  
  return(final_output)
  
  
}
             