#' Convert the column types of a dataframe
#'
#' Designed to fail in an appropriate way
#'
#' @param .data
#' @param new_data_types
#'
#' @return data frame
#' @export
#' @noRd
#'
#' @examples
#' x <- data.frame(
#'   zero_one = sample(0:1, 10, replace = TRUE),
#'   integers = sample(0:10, 10, replace = TRUE),
#'   runif = runif(10),
#'   TF = sample(c("T", "F"), 10, replace = TRUE),
#'   char = sample(LETTERS, 10)
#'  )
#'  str(convert_data_types(x, c('Binary', 'Categorical', 'Continuous', 'Binary', 'Continuous')))
convert_data_types <- function(.data, new_data_types){
  # get current data types
  # old_data_types <- convert_data_type_to_simple(.data)

  # convert the data
  for (i in seq_along(.data)){
    .data[[i]] <- convert_data_types_(.data[[i]], new_data_types[i])
  }

  return(.data)
}

#' @describeIn convert_data_types converts x to the new_data_type
convert_data_types_ <- function(x, new_data_type){
  if (new_data_type %notin% c('Categorical', 'Binary', 'Continuous')) stop("new_data_type must be one of c('Categorical', 'Binary', 'Continuous')")

  convert_fn <- switch(
    new_data_type,
    "Categorical" = as.character,
    'Binary' = coerce_to_logical,
    "Continuous" = as.numeric
  )

  x <- convert_fn(x)

  return(x)
}

#' Simplify data types for humans
#'
#' Categorizes R data types into 'Continuous', 'Categorical', or 'Binary'
#'
#' @param .data dataframe
#'
#' @author Joe Marlo
#'
#' @return character vector of length ncol(.data)
#' @export
#' @noRd
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
  raw_data_types <- base::sapply(.data, class)

  # create mapping between complex and simple data types
  data_type_mapping <- data.frame(
    complex = c("character", "factor", "logical", "numeric", "integer"),
    simple = c("Categorical", "Categorical", 'Binary', "Continuous", "Continuous"),
    stringsAsFactors = FALSE
  )

  # get simple data
  simple_data_types <- dplyr::left_join(
    x = data.frame(complex = as.vector(raw_data_types)),
    y = data_type_mapping,
    by = 'complex')$simple

  return(simple_data_types)
}
