

#' Detect if a dataframe has potential dummy columns
#'
#' @param .data a dataframe
#'
#' @author Joe Marlo
#'
#' @return
#' @export
#'
#' @examples
#' .data <- tibble(test = 1:5,
#'                 to_dummy = c('level1', 'level1', 'level2', 'level2', 'level3'),
#'                 to_dummy2 = c('char1', 'char3', 'char3', 'char2', 'char1'))
#' .data <- fastDummies::dummy_cols(.data)
#' clean_detect_dummy_cols(.data)
clean_detect_dummy_cols <- function(.data){
  
  # filter to just logical columns
  data_cleaned <- clean_auto_convert_logicals(.data)
  is_logical <- sapply(data_cleaned, class) == 'logical'
  data_logical <- .data[, is_logical]
  
  # stop if there is only one column
  if (sum(is_logical) == 1) return(NULL)
  
  # create all possible combinations of columns
  col_indices <- seq_along(data_logical)
  col_combinations <- sapply(col_indices[-1], function(m) combn(col_indices, m, simplify = FALSE))
  col_combinations <- unlist(col_combinations, recursive = FALSE)

  # test for rank deficiency
  is_dummy <- sapply(col_combinations, function(cols) is_rank_deficient(data_logical[, cols]))
  contains_dummy <- isTRUE(any(is_dummy))
  
  # pull out dummy column names
  dummy_cols <- NULL
  if (contains_dummy) {
    dummy_col_indices <- col_combinations[is_dummy]
    col_names <- colnames(data_logical)
    dummy_cols <- lapply(dummy_col_indices, function(col_indices) col_names[col_indices])
  }
  
  rslts <- list(contains_dummy = contains_dummy, dummy_columns = dummy_cols)
  
  return(rslts)
}

is_rank_deficient <- function(.matrix){
  # rank <- qr(.matrix)$rank 
  # is_rank_deficient <- rank < ncol(.matrix) | rank < nrow(.matrix)
  
  is_rank_deficient <- all(rowSums(.matrix) == 1)
  
  return(is_rank_deficient)
}
