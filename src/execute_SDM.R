#' Run Maxent model and save all results
#'
#' This function is mostly a wrapper around `ENMeval::ENMevaluate()` and 
#' subsequent steps laid out in the vignette: https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html
#' 
#' @param occ A data frame of occurrences with only the "species, latitude, longitude" columns
#' @param predictors A SpatRaster object of all processed predictor variables 
#' @param features A list of all feature classes to 
#' @param limit The maximum number of occurrences to return for a single species. Default is 100000.
#' @param type  Either 'sf' to return an sf object or 'df' to return a dataframe. Default is a dataframe.
#' @param save Whether to save (TRUE) the resulting dataframe or not (FALSE)
#' @param output_path If `save = TRUE`, the file path to save the dataframe or sf object.
#'
#' @return  Either an sf object or tabular dataframe of all cleaned species occurrences.