clean_confounders_for_bart <- function(df){
  character_vars <- names(which(sapply(df, is.character)))
  factor_vars <- names(which(sapply(df, is.factor)))
  
  if(length(factor_vars) >0 | length(character_vars) > 0 ){
  x <- dplyr::select(df, c(all_of(factor_vars), all_of(character_vars)))
  y <- dplyr::select(df, -c(all_of(factor_vars), all_of(character_vars)))
  dummy_vec <- function(x){
    # get unique vals
    uni_vals <- unique(x)
    dummys <- list()
    
    for (i in 1:length(uni_vals)) {
      dummys[[i]] <- ifelse(x == uni_vals[i], 1, 0)
    }
    
    names(dummys) <- uni_vals
    dummys <- bind_cols(dummys)
    return(dummys)
  }
  
  x <- bind_cols(map(x, dummy_vec))
  confounders_mat <- as.matrix(cbind(y, x))
  colnames(confounders_mat) <- NULL
  }
  
  else{
    confounders_mat <- as.matrix(df)
    colnames(confounders_mat) <- NULL
  }
  
  return(confounders_mat)
}


