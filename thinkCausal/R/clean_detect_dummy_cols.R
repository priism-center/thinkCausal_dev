

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
  if (sum(is_logical) <= 1) return(list(contains_dummy = FALSE, dummy_columns = NULL))
  
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

clean_detect_dummy_cols_unique <- function(.data){
  # ensure no duplicate groupings
  # returns the largest group if a given column shows up in two groups
  # TODO: this is kind of a mess; I think it works well enough tho
  
  # get potential dummy columns
  dummy_cols <- clean_detect_dummy_cols(.data)$dummy_columns
  if (length(dummy_cols) <= 2) return(dummy_cols)
  
  # calculate group size
  group_size <- map_int(dummy_cols, length)
  all_potential_cols <- sort(unique(unlist(dummy_cols)))
  
  # find which group each column is in
  is_in_group <- map(all_potential_cols, function(dummy){
    is_in_group <- map_lgl(dummy_cols, function(dummies) dummy %in% dummies)
    which(is_in_group)
  })
  names(is_in_group) <- all_potential_cols
  
  # which groups have multiple items
  duplicates <- map(is_in_group, length)
  duplicates <- names(duplicates[duplicates > 1])
  duplicates <- is_in_group[duplicates]
  
  # for each duplicate, drop the smaller groups
  # if the same size, retain the first one
  groups_to_drop <- map(duplicates, function(duplicate){
    sizes <- group_size[duplicate]
    biggest_group <- duplicate[which.max(unlist(sizes))]
    smaller_groups <- setdiff(duplicate, biggest_group)
    return(smaller_groups)
  })
  filtered_groups <- dummy_cols[-unlist(groups_to_drop)]
  
  # if the above algo fails, just return the first group
  col_counts <- table(unlist(filtered_groups))
  if (any(col_counts > 1)) filtered_groups <- dummy_cols[1]
  
  return(filtered_groups)
}

# .data <- tibble(test = 1:5,
#                 to_dummy = c('level1', 'level1', 'level2', 'level2', 'level3'),
#                 to_dummy2 = c('char1', 'char3', 'char3', 'char2', 'char1'))
# .data <- fastDummies::dummy_cols(.data)
# .data <- bind_cols(.data, .data)
# clean_detect_dummy_cols_unique(.data)
      
