#' Simplify data types for humans
#'
#' Categorizes R data types into 'Continuous', 'Categorical', or 'Binary'  
#'
#' @param .data dataframe
#'
#' @return character vector of length ncol(.data)
#' @export
#'
#' @seealso \code{\link{convert_data_type_to_complex}}
#'
#' @examples
#' n_row <- 10
#' my_character = sample(c('one', 'two', 'three'), size = n_row, replace = TRUE)
#' my_logical = sample(c(TRUE, FALSE), size = n_row, replace = TRUE)
#' my_numeric = rnorm(n_row)
#' X <- data.frame(my_character = my_character, my_logical = my_logical, my_numeric = my_numeric)
#' convert_data_type_to_simple(X)
convert_data_type_to_simple <- function(.data){

  # get raw data types
  raw_data_types <- sapply(.data, class)
  
  # create mapping between complex and simple data types
  data_type_mapping <- data.frame(
    complex = c("character", "factor", "logical", "numeric", "integer"),
    simple = c("Categorical", "Categorical", 'Binary', "Continuous", "Continuous"),
    stringsAsFactors = FALSE
  )
  
  # get simple data
  simple_data_types <- left_join(
    x = data.frame(complex = as.vector(raw_data_types)),
    y = data_type_mapping,
    by = 'complex')
  
  return(simple_data_types$simple)
}
