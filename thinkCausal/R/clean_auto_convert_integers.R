
#' Convert integer-like columns with few levels to a factor
#'
#' Useful for plotting
#'
#' @param .data a dataframe
#'
#' @return
#' @export
#'
#' @examples
#' x <- data.frame(
#'   zero_one = sample(0:1, 10, replace = TRUE),
#'   integers = sample(0:10, 10, replace = TRUE),
#'   runif = runif(10),
#'   TF = sample(c("T", "F"), 10, replace = TRUE),
#'   char = sample(LETTERS, 10)
#'  )
#'  str(clean_auto_convert_integers(x))
clean_auto_convert_integers <- function(.data){

  for (col in colnames(.data)){
    is_few_integers <- clean_detect_integers(.data[[col]])
    
    # convert column to categorical
    if (isTRUE(is_few_integers)){
      .data[[col]] <- factor(.data[[col]],
                             levels = sort(unique(.data[[col]])))
    }
  }
  
  return(.data)
}

clean_detect_integers <- function(x, n_levels_threshold = 15){
  
  if(inherits(x, 'data.frame')) stop('x cannot be a dataframe')
  
  # does x match its self coerced to an integer 
  is_integer <- tryCatch(all.equal(x, as.integer(x)),
                         error = function(e) FALSE)
  
  if (isTRUE(is_integer)){
    n_levels <- dplyr::n_distinct(x)
    if (n_levels <= n_levels_threshold) return(TRUE)
  }
  
  return(FALSE)
}


