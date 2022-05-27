#' Fit a BART model using a standardized interface
#'
#' @param .data the data. First column should be treatment, second column response.
#' @param support one of c('ate', 'atc', 'att'). See bartCause::bartc
#' @param ran.eff if not "None", then the name of the column within .data that represents the random effects
#' @param .estimand the causal estimand. See bartCause::bartc
#'
#' @return an object of class "bartcFit"
#' @export
fit_bart <- function(.data, support, block, .weights, ran_eff, .estimand){
  ind <- 3 + length(.weights) + length(ran_eff)

  tryCatch({
    if(is_null(ran_eff)){
        bartCause::bartc(
          response = .data[, 2], 
          treatment = .data[, 1], 
          confounders = clean_confounders_for_bart(.data[, ind:length(.data)]), 
          estimand = .estimand, 
          #if(!is_null(.weights) weights = .data[[.weights]], 
          commonSup.rule = support, 
          keepTrees = TRUE, 
          seed = 2
        )
    }
    
    else{
      bartCause::bartc(
        response = .data[[2]], 
        treatment = .data[[1]], 
        confounders = clean_confounders_for_bart(.data[, ind:length(.data)]), 
        estimand = .estimand, 
        weights = wgt, 
        group.by = ran_eff, 
        use.ranef = TRUE,  
        commonSup.rule = support, 
        keepTrees = TRUE,
        seed = 2
      )
    }
    },
    error = function(e) NULL
  )
}
