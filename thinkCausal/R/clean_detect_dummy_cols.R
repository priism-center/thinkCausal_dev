

#' Detect if a dataframe has dummy columns
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
  if (sum(is_logical) <= 1) return(list(contains_dummy = contains_dummy, dummy_columns = NULL))
  
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
  
  # this only works if reference column is included
  is_rank_deficient <- all(rowSums(.matrix) == 1)
  
  return(is_rank_deficient)
}


# tmp <- fastDummies::dummy_cols(.data, remove_first_dummy = T)[, 4:7]
# tmp2 <- fastDummies::dummy_cols(.data)[, 4:9]
# is_rank_deficient(tmp[,1:2])
# is_rank_deficient(tmp2[, 1:3])
# 
# .matrix <- tmp2[, 1:4]
# 
# tmp_cor <- cor.test(.matrix[[1]], .matrix[[2]], method = 'spearman')
# cor(.matrix[, -4])
# cor(c(1,1,0,0), c(0,0, 1,1))
# cor.test(c(1,1,0,0), c(0,0, 1,1),  method = 'spearman')

# wilcox.test(.matrix[[1]] ~ .matrix[[2]])
# 
# cor(tmp2[,1:3])
# 
# lmmod <- lm(1:nrow(tmp) ~ tmp2[[1]] + tmp2[[2]] + tmp2[[3]])

# goal is to create a popup that use can drag and drop grouppings that define the dummy code
# prompt user only if more than one logical column that is not the outcome
# could also integrate this into the XYZ selection page



