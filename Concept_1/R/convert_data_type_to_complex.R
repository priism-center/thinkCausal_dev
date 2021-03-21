#' Convert human readable data type to R data
#'
#' The inverse of convert_data_type_to_simple(). Converts a dataframe to new data types based on human readable strings. 
#'
#' @param .data a dataframe
#' @param .simple_data_types human readable data types from convert_data_type_to_simple. Must be length == ncol(.data)
#'
#' @return a dataframe with dim == dim(.data)
#' @export
#' 
#' @seealso \code{\link{convert_data_type_to_simple}}
#'
#' @examples
convert_data_type_to_complex <- function(.data, .simple_data_types){

  if (!isTRUE(ncol(.data) == length(.simple_data_types))) stop('ncol(.data) must equal length(.simple_data_types)')

  # get raw data types
  raw_data_types <- sapply(input_data, class)

  # get simple col types specified by paired function
  default_simple_col_types <- convert_data_type_to_simple(.data)

  # which ones don't match
  which_cols_to_change <- which(!(.simple_data_types == default_simple_col_types))
  cols_to_change <- colnames(.data)[which_cols_to_change]

  # make the datatype changes
  # TODO: pickup here
  lapply(cols_to_change, function(col_to_change){
    # .data[, col_to_change] <- 
  })

}
# 
# 
# n_row <- 10
# my_character = sample(c('one', 'two', 'three'), size = n_row, replace = TRUE)
# my_logical = sample(c(TRUE, FALSE), size = n_row, replace = TRUE)
# my_numeric = rnorm(n_row)
# my_logical = sample(c(TRUE, FALSE), size = n_row, replace = TRUE)
# X <- data.frame(my_character = my_character, my_logical = my_logical, my_numeric = my_numeric)
# simple_types <- convert_data_type_to_simple(X)
# 
# .data = X
# .simple_data_types = simple_types

