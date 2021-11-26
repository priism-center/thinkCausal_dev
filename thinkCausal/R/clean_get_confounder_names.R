clean_get_confounder_names <- function(columns = store$column_types, get = c('catagorical', 'continuous', 'all')){
  cols_categorical <- columns$categorical
  cols_continuous <- columns$continuous
  X_cols_categorical <- grep("^X_", cols_categorical, value = TRUE)
  X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
  cols_categorical_cleaned <- gsub("X_", '', X_cols_categorical)
  cols_continuous_cleaned <- gsub("X_", '', X_cols_continuous)
  out <- list(cols_categorical_cleaned, cols_continuous_cleaned )
  if(get == 'catagorical') out <- out[[1]]
  if(get == 'continuous') out <- out[[2]]
  if(get == 'all') out <- unlist(out)
  return(out)
}