#' Convert all psuedo-logical columns in a dataframe to booleans
#'
#' Converts columns of a dataframe containing binary c(0, 1), c("T", "F"), c("True", "False") to boolean c(TRUE, FALSE). Is agnostic to case.
#'
#' @param input_data dataframe
#'
#' @author Joe Marlo
#'
#' @return dataframe
#' @export
#' 
#' @importFrom readr parse_logical
#'
#' @examples
#' x <- data.frame(
#'    zero_one = sample(0:1, 10, replace = TRUE),
#'    TF = sample(c("T", "F"), 10, replace = TRUE),
#'    truefalse = sample(c('true', 'false'), 10, replace = TRUE),
#'    char = sample(LETTERS, 10)
#'  )
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
