#' Convert all psuedo-logical columns in a dataframe to booleans
#'
#' @param input_data dataframe
#'
#' @author Joe Marlo
#'
#' @return dataframe
#' @export
#'
#' @examples
#' x <- data.frame(
#'    zero_one = sample(0:1, 10, replace = TRUE),
#'    TF = sample(c("T", "F"), 10, replace = TRUE),
#'    truefalse = sample(c('true', 'false'), 10, replace = TRUE)
#'  )
#' auto_convert_logicals(x)
auto_convert_logicals <- function(input_data){
  # function converts columns of 0:1, T:F, True:False to logicals
  
  for (col in colnames(input_data)){
    
    # is the column exclusively in list of pre-determined
    inclusion_list <- c(0, 1, 't', 'f', 'true', 'false')
    col_as_char <- as.character(input_data[[col]])
    col_cleaned <- base::tolower(unique(col_as_char))
    is_in_list <- length(setdiff(col_cleaned, inclusion_list)) == 0
    
    # convert column to logical
    if (isTRUE(is_in_list)){
      input_data[,col] <- readr::parse_logical(col_as_char)
    }
  }
  
  return(input_data)
}
