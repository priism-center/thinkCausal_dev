#' Return a list of the column types
#'
#' Categorizes the types of columns in a dataframe by categorical or continuous.
#'
#' @param .data a dataframe
#'
#' @return a character vector of column types
#' @noRd
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


# TODO: should adjust this to accept yes, no

#' Convert all psuedo-logical columns in a dataframe to booleans
#'
#' Converts columns of a dataframe containing binary c(0, 1), c("T", "F"), c("True", "False") to boolean c(TRUE, FALSE). Is agnostic to case.
#'
#' @param input_data dataframe
#'
#' @author Joe Marlo
#'
#' @return dataframe
#' @noRd
#'
#' @importFrom readr parse_logical
#'
#' @examples
#' x <- data.frame(
#'   zero_one = sample(0:1, 10, replace = TRUE),
#'   TF = sample(c("T", "F"), 10, replace = TRUE),
#'   truefalse = sample(c('true', 'false'), 10, replace = TRUE),
#'   char = sample(LETTERS, 10),
#'   yn = sample(c("yes", "no"), 10, replace = TRUE)
#'   )
#' clean_auto_convert_logicals(x)
clean_auto_convert_logicals <- function(input_data){
  # function converts columns of 0:1, T:F, True:False to logicals

  for (col in colnames(input_data)){
    is_in_list <- clean_detect_logical(input_data[[col]])

    # convert column to logical
    if (isTRUE(is_in_list)){
      col_as_char <- as.character(input_data[[col]])
      input_data[,col] <- readr::parse_logical(col_as_char)
    }
  }

  return(input_data)
}

clean_detect_logical <- function(x){

  if(inherits(x, 'data.frame')) stop('x cannot be a dataframe')

  # is x exclusively in list of pre-determined
  inclusion_list <- c(0, 1, 't', 'f', 'true', 'false')
  x_as_char <- as.character(x)
  x_cleaned <- base::tolower(unique(x_as_char))
  is_in_list <- length(setdiff(x_cleaned, inclusion_list)) == 0
  return(is_in_list)
}
