#' Attempt to detect which columns of a dataframe are Z, Y, and X
#' 
#' Attempts to auto detect which columns are treatment, response, or ID columns. Treatment and response are detected based on column name alone; ID is based on column name and the values. If none are detected, then columns are categorized as 'X'. Will only return one item (the first) for Z, Y, and ID.
#' 
#' @param .data a dataframe
#'
#' @author Joe Marlo
#'
#' @return a list denoting which column names are Z, Y, X, and ID
#' @export
#'
#' @examples
#' .data <- data.frame(
#'  treatment = c(TRUE, TRUE, FALSE, TRUE, FALSE),
#'  rsp = c(0.808, 0.296,-1.579,-1.272, 0.627),
#'  ID = c(0.808, 0.296,-1.579,-1.272, 0.627),
#'  dummyID = 1:5,
#'  dummy1 = c(34, 35, 10, 5, 38)
#' )
#' clean_detect_ZYX_columns(.data)
clean_detect_ZYX_columns <- function(.data) {
  
  # set list of potential column names to match
  Z_potentials <- c("^Z_| ", '^Z$', "trt", "treat", "treatment")
  Y_potentials <- c("^Y_| ", '^Y$', "response", "rsp")
  ID_potentials <- c("^id")
  all_col_names <- colnames(.data)
  
  # find Z column
  Z_matches <- sapply(X = all_col_names, FUN = function(col){
    any(
      stringr::str_detect(
        string = col, 
        pattern = stringr::regex(Z_potentials, ignore_case = TRUE)
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
        pattern = stringr::regex(Y_potentials, ignore_case = TRUE)
      )
    )
  })
  Y <- all_col_names_ex_Z[Y_matches][1]
  
  # find ID columns
  all_col_names_ex_ZY <- setdiff(all_col_names, c(Z, Y))
  ID_matches <- sapply(X = all_col_names_ex_ZY, FUN = function(col){
    any(
      stringr::str_detect(
        string = col, 
        pattern = stringr::regex(ID_potentials, ignore_case = TRUE)
      )
    )
  })
  ID <- all_col_names_ex_ZY[ID_matches][1]
  # test if any columns are integers with spacing 1
  if (is.na(ID)) {
    is_ID <- unlist(sapply(X = .data[, all_col_names_ex_ZY], FUN = function(col){
      is_ID <- FALSE
      if (is.numeric(col)){
        is_ID <- all(table(diff(sort(col))) == 1)
      }
      return(is_ID)
    }))
    ID <- all_col_names_ex_ZY[is_ID][1]
  }
  
  # defaults if none are found
  if (isTRUE(is.na(Z))) Z <- NULL
  if (isTRUE(is.na(Y))) Y <- NULL
  if (isTRUE(is.na(ID))) ID <- NULL
  X <- setdiff(all_col_names, c(Z, Y, ID))
  
  matched_cols <- list(Z = Z, Y = Y, X = X, ID = ID)
  return(matched_cols)
}
