#' Determine which default columns to use in plot_exploration()
#'
#' @param .column_types a list containing names of categorical and continuous columns. Ideally from output of clean_detect_column_types().
#' @param .treatment_column name of column denoting treatment
#' @param .response_column name of the outcome column
#'
#' @return a list denoting which columns to use in plotting
#' @export
#' 
#' @seealso \code{\link{plot_exploration} \link{clean_detect_column_types}}
#'
#' @examples
#' X <- data.frame(
#'  treatment = sample(as.logical(0:1), 5, TRUE), 
#'  response = rnorm(5),
#'  X1 = 1:5, 
#'  X2 = rnorm(5), 
#'  X3 = LETTERS[1:5], 
#'  X4 = as.factor(LETTERS[1:5])
#')
#' column_types <- clean_detect_column_types(X)
#' clean_detect_plot_vars(column_types, 'treatment', 'response')
clean_detect_plot_vars <- function(.column_types, .treatment_column, .response_column){
  
  trt_in_types <- .treatment_column %in% unlist(.column_types)
  outcome_in_types <- .response_column %in% unlist(.column_types)
  if (!isTRUE(trt_in_types & outcome_in_types)) stop(".treatment_column and .response_column must be in .column_types")
  
  vars_categorical <- setdiff(.column_types$categorical, c(.treatment_column, .response_column))
  vars_continuous <- setdiff(.column_types$continuous, c(.treatment_column, .response_column))
  
  n_categorical <- length(vars_categorical)
  n_continuous <- length(vars_continuous)
  
  Y <- NULL
  fill <- .treatment_column
  size <- "None"
  grouping <- "None"
  facet <- "None"
  
  # TODO implement response as logical
  
  if (.response_column %in% .column_types$continuous){
    Y <- .response_column
    if (n_continuous > 0){
      plot_type <- 'Scatter'
      X <- vars_continuous[1]
    } else {
      plot_type <- 'Boxplot'
      X <- vars_categorical[1]
    }
  } else if (.response_column %in% .column_types$categorical){
    if (n_continuous > 0){
      plot_type <- 'Boxplot'
      X <- vars_continuous[1]
      grouping <- .treatment_column
    } else {
      plot_type <- 'Barplot'
      X <- .response_column
      Y <- vars_continuous[1]
      grouping <- .treatment_column
    }
  } else stop("Outcome is somehow not classifed as either continuous or categorical")
  
  plot_variables <- list(
    plot_type = plot_type,
    X = X,
    Y = Y,
    fill = fill,
    size = size,
    grouping = grouping,
    facet = facet
  )
  
  return(plot_variables)
}
