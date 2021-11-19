
# for reproducible script
clean_dummies_to_categorical <- function(df, group_names){
  
  # find the column indexes of dummy variables in the same group 
  idx <- which(colnames(df) %in% group_names)
  tmp <- df[,idx]
  categorical <- apply(tmp, 1, function(x) ifelse(sum(x, na.rm = T) == 0, 'REFERENCE', colnames(tmp)[which(x == TRUE)]))
  # remove the multiple dummies from the dataset
  df <- df[,-idx]
  # add the new categorical variable into the dataset
  df <- cbind(df, categorical)
  colnames(df)[ncol(df)] <- deparse(substitute(group_names))
  return(df)
  
}



  