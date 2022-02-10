# 
#' Convert dummies in the same group to a categorical variable in a dataframe 
#' For thinkCausal app development use
#' 
#' @param i an integer to indicate the ith group of dummies. Useful to keep track of each group when there are multiple categorical variables in a dataframe
#' @param df dataframe
#' @param group_names a vector of strings that are the names of dummies in the same group
#' @param rename_group a string of the name for the categorical variable
#' @param problematic_group_names a vector of strings to indicate which groups have empty names. Initialize it as an empty vector, c(), 
#' and then the function will return the group number if `rename_group` is an empty string and NUll if `rename_group` is not an empty string
#' 
#' @author Junhui Yang
#' 
#' @return a list containing group number of problematic group names, a cleaned daatframe with dummies collapsed to a categorical variable, 
#' and a list of categorical variables' names and their components
#' @export
#'
#' @examples
#' df <- data.frame(
#' momwhite = c(1,0,0,0,0,0,1),
#' momblack = c(0,1,1,1,1,0,0),
#' momhisp = c(0,0,0,0,0,0,0),
#' y.obs = rnorm(7, 10, 3)
#' )
#' clean_dummies_to_categorical_internal(i = 1, df = df, 
#'                                       group_names = c('momwhite', 'momblack', 'momhisp'), 
#'                                       rename_group = 'race', problematic_group_names = c())
#' 
#' clean_dummies_to_categorical_internal(i = 1, df = df, 
#'                                       group_names = c('momwhite', 'momblack', 'momhisp'), 
#'                                       rename_group = '', problematic_group_names = c())

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
    df <- df[,-idx, drop=FALSE]
    
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


#' Convert dummies in a dataframe to a categorical variable
#' For downloaded reproducible script use
#' 
#' @param df dataframe
#' @param group_names a vector of strings containing dummies' names of a categorical variable. 
#' The name of the vector will used as the categorical variable's name
#' 
#' @author Junhui Yang
#' 
#' @return
#' @export
#'
#' @examples
#' df <- data.frame(
#' momwhite = c(1,0,0,0,0,0,1),
#' momblack = c(0,1,1,1,1,0,0),
#' momhisp = c(0,0,0,0,0,0,0),
#' y.obs = rnorm(7, 10, 3)
#' )
#' race <- c("momwhite", "momblack", "momhisp")
#' clean_dummies_to_categorical(df = df, group_names = race)

clean_dummies_to_categorical <- function(df, group_names){
  
  # find the column indexes of dummy variables in the same group 
  idx <- which(colnames(df) %in% group_names)
  tmp <- df[,idx]
  categorical <- apply(tmp, 1, function(x) ifelse(sum(x, na.rm = T) == 0, 'REFERENCE', colnames(tmp)[which(x == TRUE)]))
  # remove the multiple dummies from the dataset
  df <- df[,-idx, drop=FALSE]
  # add the new categorical variable into the dataset
  df <- cbind(df, categorical)
  colnames(df)[ncol(df)] <- deparse(substitute(group_names))
  return(df)
  
}
