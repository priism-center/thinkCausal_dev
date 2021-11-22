clean_confounders_for_bart <- function(df){
  character_vars <- names(which(sapply(df, is.character)))
  factor_vars <- names(which(sapply(df, is.factor)))
  
  
  if(length(factor_vars) >0 | length(character_vars) > 0 ){
  x <- dplyr::select(df, c(all_of(factor_vars), all_of(character_vars)))
  
  get_levels <- function(vec){
    out <- length(unique(vec))
  }
  levels <- sapply(x, get_levels)
  col_names <- names(x)
  names_list <- list()
  for (i in 1:length(col_names)) {
    names_list[[i]] <- rep(col_names[i], levels[i])
    names_list[[i]] <- paste(names_list[[i]], unique(x[[i]]), sep = "_")
  }
  
  y <- dplyr::select(df, -c(all_of(factor_vars), all_of(character_vars)))
  dummy_vec <- function(vec){
    # get unique vals
    uni_vals <- unique(vec)
    dummys <- list()
    
    for (i in 1:length(uni_vals)) {
      dummys[[i]] <- ifelse(vec == uni_vals[i], 1, 0)
    }
    
    names(dummys) <- uni_vals
    dummys <- bind_cols(dummys)
    return(dummys)
  }
  
  x <- bind_cols(map(x, dummy_vec))
  names(x) <- unlist(names_list)
  confounders_mat <- as.matrix(cbind(y, x))
  }
  
  else{
    confounders_mat <- as.matrix(df)
  }
  
  return(confounders_mat)
}
clean_confounders_for_bart(df)

