# use within thinkCausal
clean_dummies_to_categorical_internal <- function(i, df, group_names, rename_group, problematic_group_names){
  
  # find the column indexes of dummy variables in the same group 
  idx <- which(colnames(df) %in% group_names)
  # a list to save group names and corresponding dummies if there are more than one dummy in a group
  group_list <- list()
  
  # if there are more than one dummies in a group, convert the dummies to a categorical variable
  if(length(idx) > 1){
    tmp <- df[,idx]
    
    # if the sum of all the categories in a row is zero, then the reference group is missing for the categorical variable, filling with 'REFERENCE'
    # otherwise, filling with the column name of the binary variable whose values is TRUE 
    categorical <- apply(tmp, 1, function(x) ifelse(sum(x, na.rm = T) == 0, 'REFERENCE', colnames(tmp)[which(x == TRUE)]))
    
    # remove the multiple dummies
    df <- df[,-idx]
    
    # add the new categorical variable into the dataset
    df <- cbind(df, categorical)
    
    # clean the user input name
    name <- clean_names(rename_group)
    # check if user input variable name is empty
    if(name == "" | grepl("blank", name, ignore.case = TRUE)) {
      problematic_group_names <- c(problematic_group_names, paste0('Group', i))
    }
    colnames(df)[ncol(df)] <- name
    groups <- c(group_list, list(c(name, colnames(tmp))))
  }
  
  return(list(problematic_group_names, df, groups))
}
