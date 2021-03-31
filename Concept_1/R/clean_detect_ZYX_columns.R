#' Attempt to detect which columns of a dataframe are Z, Y, and X
#' 
#' Attempts to auto detect which columns are treatment and response based on the column name alone. If none are detected, then returns the first two respectively.
#' 
#' @param input_colnames character vector of column names
#'
#' @author Joe Marlo
#'
#' @return a list denoting which column names are Z, Y, and X
#' @export
#'
#' @examples
#' x <- c("treatment", "rsp", "dummy1", "dummy2")
#' clean_detect_ZYX_columns(x)
clean_detect_ZYX_columns <- function(input_colnames) {
  # TODO: instead of regex, do string distance?

  # set list of potential column names to match
  Z_potentials <- c("^Z", "trt", "treat", "treatment")
  Y_potentials <- c("^Y", "response", "rsp")
  all_col_names <- input_colnames
  
  # find Z column
  Z_matches <- sapply(X = all_col_names, FUN = function(col){
    any(
      stringr::str_detect(
        string = col, 
        pattern = regex(Z_potentials, ignore_case = TRUE)
      )
    )
  })
  Z <- all_col_names[Z_matches][1]
  
  # find Y columns
  all_col_names_ex_Z <- setdiff(all_col_names, Z)
  Y_matches <- sapply(X = all_col_names_ex_Z, FUN = function(col){
    any(
      stringr::str_detect(
        string = col, 
        pattern = regex(Y_potentials, ignore_case = TRUE)
      )
    )
  })
  Y <- all_col_names_ex_Z[Y_matches][1]
  
  # defaults if none are found
  all_col_names_ex_Y <- setdiff(all_col_names, Y)
  if (isTRUE(is.na(Z))) Z <- NULL #all_col_names_ex_Y[1]
  all_col_names_ex_Z <- setdiff(all_col_names, Z)
  if (isTRUE(is.na(Y))) Y <- NULL #all_col_names_ex_Z[1]
  X <- setdiff(all_col_names, c(Z, Y))
  
  matched_cols <- list(Z = Z, Y = Y, X = X)
  return(matched_cols)
}
