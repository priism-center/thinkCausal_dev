# 
#' Identify levels of an indicator variable when data is has alredy been one hot encoded 
#' For thinkCausal app development use
#' 
#' @param x a string that corresponds to the name of a variable that may be part of a categorical variable
#' @param cat_df dataframe of all non continuous confounders

#' @author George Perrett
#' 
#' @return a vector of names that corresponds to all other levels of the confounder
#' @export
#'
#' @examples
#' cat_df <- data.frame(
#'  momwhite = c(1,0,0,0,0,0,1),
#'  momblack = c(0,1,1,1,1,0,0),
#'  momhisp = c(0,0,0,0,0,0,0),
#'  arc =c(1,0,0,0,1,0,1)
#'  )
#'identify_indicators(x = 'momwhite', cats = cat_df)


identify_indicators <- function(x, cats){
  
  transform_indicator_to_categorical <- function(.x, cat_df){
    compare <- cat_df %>% 
      select(-all_of(.x))
    
    run_test <- lapply(1:length(compare), function(i){
      tmp <- tibble(cat_df[[all_of(.x)]], compare[[i]])
      rows <- tmp %>% 
        mutate(rSum = rowSums(across())) %>% 
        filter(rSum > 1) %>% 
        nrow()
      if(rows == 0) names(compare)[i]
    })
    
    out <- unlist(run_test)
    if(length(out) > 1) out <- c(.x, out) 
    out <- sort(out)
    return(out)
  }
  
  run <- transform_indicator_to_categorical(.x = x, cat_df = cats)
  eval <- lapply(run, function(i){transform_indicator_to_categorical(.x = i, cat_df = cats)})
  probs <- table(match(eval, unique(eval)))/length(eval)
  out <- eval[[which(probs == max(probs))]]
  return(out)
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
