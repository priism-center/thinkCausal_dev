#' Convert human readable data type to R data
#'
#' The inverse of convert_data_type_to_simple(). Converts a dataframe to new data types based on human readable strings. 
#'
#' @param .data a dataframe
#' @param .simple_data_types human readable data types from list("Categorical", "Binary", "Continuous"). Must be length == ncol(.data)
#'
#' @author Joe Marlo
#'
#' @return a dataframe with dim == dim(.data)
#' @export
#' 
#' @seealso \code{\link{convert_data_type_to_simple}}
#'
#' @examples
convert_data_type_to_complex <- function(.data, .simple_data_types, new_levels){

  if (!isTRUE(ncol(.data) == length(.simple_data_types))) stop('ncol(.data) must equal length(.simple_data_types)')

  # get raw data types
  raw_data_types <- sapply(.data, class)

  # get simple col types specified by paired function
  default_simple_col_types <- convert_data_type_to_simple(.data)

  # which ones don't match
  which_cols_to_change <- which(!(.simple_data_types == default_simple_col_types))
  cols_to_change <- colnames(.data)[which_cols_to_change]
  new_data_types <- .simple_data_types[which_cols_to_change]

  # make the datatype changes
  # TODO: pickup here
  mapply(cols_to_change, new_data_types, FUN = function(col_to_change, new_data_type){
    # .data[, col_to_change] <-
  })

  return(.data)
}


# n_row <- 10
# my_character = sample(c('one', 'two', 'three'), size = n_row, replace = TRUE)
# my_logical = sample(c(TRUE, FALSE), size = n_row, replace = TRUE)
# my_numeric = rnorm(n_row)
# my_logical = sample(c(TRUE, FALSE), size = n_row, replace = TRUE)
# char_log <- sample(c('yes', 'no'), size = n_row, replace = TRUE)
# X <- data.frame(my_character = my_character, my_logical = my_logical, my_numeric = my_numeric, char_log = char_log)
# simple_types <- convert_data_type_to_simple(X)
# 
# .data = X
# .simple_data_types = c("Categorical", "Binary", "Continuous", "Binary")
# 
# # create mapping between complex and simple data types
# # note this should match the mapping in convert_data_type_to_simple
# data_type_mapping <- data.frame(
#   complex = c("character", "factor", "logical", "numeric", "integer"),
#   simple = c("Categorical", "Categorical", 'Binary', "Continuous", "Continuous"),
#   conversion_function = c(deparse(as.character), deparse(as.character), deparse(as.logical), deparse(as.numeric), deparse(as.numeric)),
#   stringsAsFactors = FALSE
# )
# 
# # get simple data
# simple_data_types <- dplyr::left_join(
#   x = data.frame(complex = as.vector(raw_data_types)),
#   y = data_type_mapping,
#   by = 'complex')$simple
# 
# # get raw data types
# raw_data_types <- sapply(.data, class)
# 
# # get simple col types specified by paired function
# default_simple_col_types <- convert_data_type_to_simple(.data)
# 
# # which ones don't match
# which_cols_to_change <- which(!(.simple_data_types == default_simple_col_types))
# cols_to_change <- colnames(.data)[which_cols_to_change]
# new_data_types <- .simple_data_types[which_cols_to_change]
# 
# # make the datatype changes
# for (i in seq_along(new_data_types)){
#   col_to_change <- cols_to_change[i]
#   new_data_type <- new_data_types[i]
#   
#   # prompt user for input if necessary
#   # TODO: I think this should be outside of the function; the user should be prompted within 
#     # shiny server and results passed as an argument to this function via new_levels
#   
#   
#   # get function to convert to new datatype
#   new_complex_data_type <- data_type_mapping$conversion_function[data_type_mapping$simple == new_data_type][[1]]
#   conversion_function <- eval(parse(text = new_complex_data_type))
#   
#   # make the change
#   .data[, col_to_change] <- conversion_function(.data[, col_to_change])
# }
# 
# get_levels <- function(type){
#   # 
# }

