#' Return a list of the column types
#'
#' Categorizes the types of columns in a dataframe by categorical or continuous.
#'
#' @param .data a dataframe
#'
#' @return
#' @export
#'
#' @examples
#' X <- data.frame(X1 = 1:5, X2 = rnorm(5), X3 = LETTERS[1:5], X4 = as.factor(LETTERS[1:5]))
#' clean_detect_column_types(X)
clean_detect_column_types <- function(.data){
  
  if (!isTRUE(is.data.frame(.data))) stop(".data must be a dataframe")
  
  # set the classes that are categorical vs continuous
  classes_categorical <- c('logical', 'character', 'factor')
  classes_continuous <- c('numeric', 'integer', 'complex')
  
  # split the column types by class
  cols_by_class <- split(colnames(.data), sapply(.data, function(x) class(x)[1]))
  
  # organize the column names by categorical vs continuous
  columns_categorical <- as.vector(unlist(cols_by_class[classes_categorical]))
  columns_continuous <- as.vector(unlist(cols_by_class[classes_continuous]))
  
  column_types <- list(categorical = columns_categorical, continuous = columns_continuous)
  
  return(column_types)
}

clean_detect_continuous_or_logical <- function(x){
  # used to test if response variable can be modeled by BART

  # continuous and logicals will work
  classes_good <- c('numeric', 'integer', 'complex', 'logical')
  if (inherits(x, classes_good)) return(TRUE)
  
  # if its a categorical and only two levels then it will work
  classes_maybe <- c('character', 'factor')
  if (inherits(x, classes_maybe) & length(unique(x)) == 2) return(TRUE)
  
  return(FALSE)
}
