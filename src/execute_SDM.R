#' Run Maxent model and save all results
#'
#' This function is mostly a wrapper around `ENMeval::ENMevaluate()` and 
#' subsequent steps laid out in the vignette: https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html
#' NOTE: must be using version 2.0.5 installed from GitHub
#' 
#' 
#' @param species The scientific name of the species running the models for
#' @param occ A data frame of occurrences with only the "species, latitude, longitude" columns
#' @param predictors A SpatRaster object of all processed predictor variables 
#' @param features A list of all feature classes to compare. Default is all avaialble feature classes.
#' @param rm Vector of numbers of regularization multipliers to compare. Default is 1-10 in 0.5 increments.
#' @param null_models Logical, whether or not to run the null models component (since this is a time sink)
#' @param save Logical; Whether to save the resulting list as an .RData file or not.
#' @param output_path If `save = TRUE`, the file path to save the .RData file
#'
#' @return  A list object with all model results of interest
execute_SDM <- function(species,
                        occ,
                        predictors,
                        features = c("L", "LQ", "LQP", "LQH", "LQHP"),
                        rm = seq(1, 10, 0.5),
                        null_models = FALSE,
                        save = FALSE,
                        output_path) {
  
  # Prepare Maxent inputs ----
  
  ## Occurrences
  occ_mod <- occ %>% 
    dplyr::select(longitude, latitude)
  
  ## Background points
  bg_mod <- dismo::randomPoints(raster::raster(predictors), n = 10000) %>% as.data.frame() %>% 
    rename(longitude = x, latitude = y) %>% 
    dplyr::select(longitude, latitude)
  
  # convtert SpatRaster to RasterStack
  #preds_mod <- raster::brick(predictors)
  preds_mod <- predictors
  
  
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
  
  # need an if/then if there are ties, just choose first one
  if(nrow(top_mod_args) > 1) {
    top_mod_args <- top_mod_args[1,]
  }
  
  
  selected_mod <- eval.models(all_mods)[[top_mod_args$tune.args]]
  
  # Model Predictions ----
  
  predicted_mod <- eval.predictions(all_mods)[[as.numeric(top_mod_args$tune.args)]] #%>% 
    #convert to terra object
    #terra::rast()
  
  
  # Null Models ----
  if (null_models) {
  
    null_mod <- ENMnulls(
    e = all_mods,
    mod.settings = list(fc = as.character(top_mod_args$fc), rm = as.numeric(top_mod_args$rm)),
    no.iter = 100,
    parallel = TRUE
    
  )
  
  }
  
  # Compile Metadata ----
  
  rmm <- eval.rmm(all_mods)
  
  output_metadata <- tibble(
    #variable_names = I(list(rmm$data$environment$variableNames)),
    projection = rmm$data$environment$projection,
    species = rmm$data$occurrence$taxon,
    sample_size = rmm$data$occurrence$presenceSampleSize,
    background_sample_size = rmm$data$occurrence$backgroundSampleSize,
    prediction_units = "suitability (cloglog transformation)",
    min_prediction = terra::minmax(predicted_mod)["min",],
    max_prediction = terra::minmax(predicted_mod)["max",],
    selection_rules = "highest correlation Boyce index with lowest 10 percentile omission rate and AUC diff to break ties"
  ) %>% 
    bind_cols(top_mod_args) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = everything())
  
  
  # Raw response curve data ----
  
  ## function to pull rc data for each variable
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
  
  ## create df for all variables
  response_curves <- map(names(selected_mod$samplemeans),
                         ~ get_rc(v = .x, mod = selected_mod)) %>%
    bind_cols()
  
  
  # Calculate variable importance ----
  ## create SWD object
  swd_obj <- prepareSWD(species = species,
                        p = occ_mod,
                        a = bg_mod,
                        env = preds_mod)
                          #terra::rast(preds_mod))
  
  
  ## Use cross validation
  folds <-  randomFolds(swd_obj,
                        k = 10,
                        only_presence = TRUE)
  ## create SDMmodel
  SDM_mod <- train(method = "Maxnet",
                   data = swd_obj,
                   fc = tolower(as.character(top_mod_args$fc)),
                   reg = as.numeric(top_mod_args$rm),
                   folds = folds)
  
  ## Compute variable importance
  vi <- varImp(SDM_mod,
               permut = 10)
  
  
  # Save final output ----
  final_output <- list(
    metadata = output_metadata,
    tuning_results = eval.results(all_mods),
    tuning_results_plot = evalplot.stats(
      e = all_mods,
      stats = c("auc.diff", "cbi.val", "or.10p"),
      color = "fc",
      x.var = "rm",
      error.bars = FALSE),
    #prediction_map = predicted_mod, # error with saving terra objects to .RData
    all_mods = all_mods,
    selected_mod = selected_mod,
    response_curves = response_curves,
    variable_importance = vi)
  
  if (null_models) {
    final_output <- c(
      final_output,
      null_mods = null_mod
      # null_models = null.emp.results(null_mod),
      # null_model_plots = evalplot.nulls(
      #   null_mod,
      #   stats = c("or.10p", "auc.val", "cbi.val"),
      #   plot.type = "histogram"
      # )
    )
  }
  
  # save the file
  if (save) {
    
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    
    #assign meaningful environmental name
    filename <- paste0(gsub(" ", "_", species), "_SDM_output")
    
    assign(filename, final_output)
    
    # save all model objects to .RData file
    save(list = filename,
         file = paste0(output_path, "/", gsub(" ", "_", species), "_SDM_results.RData"))
    
    # save habitat suitability map as .tif (needed for connectivity module)
    terra::writeRaster(predicted_mod, paste0(output_path, "/", gsub(" ", "_", species), "_prediction.tif"), overwrite = TRUE)
    
  }
  
  return(final_output)
  
  
}
             