#' Fit a BART model using a standardized interface
#'
#' @param .data the data. First column should be treatment, second column response.
#' @param support one of c('ate', 'atc', 'att'). See bartCause::bartc
#' @param ran.eff if not "None", then the name of the column within .data that represents the random effects
#' @param .estimand the causal estimand. See bartCause::bartc
#'
#' @return an object of class "bartcFit"
#' @export
fit_bart <- function(.data, support, ran.eff, .estimand){
  tryCatch({
    if(ran.eff == 'No'){
        bartCause::bartc(
          response = .data[, 2], 
          treatment = .data[, 1], 
          confounders = clean_confounders_for_bart(.data[, 3:length(.data)]), 
          estimand = .estimand, 
          commonSup.rule = support, 
          keepTrees = TRUE, 
          seed = 2
        )
    }
    
    else{
      bartCause::bartc(
        response = .data[[2]], 
        treatment = .data[[1]], 
        confounders = clean_confounders_for_bart(.data[, 3:length(.data)]), 
        estimand = .estimand, 
        group.by = ran.eff, 
        group.effects = TRUE, 
        commonSup.rule = support, 
        keepTrees = TRUE,
        seed = 2
      )
    }
    },
    error = function(e) NULL
  )
}
