fit_bart <- function(.data, support, ran.eff, .estimand){
  if(ran.eff == 'None'){
      bartCause::bartc(
        response = .data[, 2], 
        treatment = .data[, 1], 
        confounders = clean_confounders_for_bart(.data[, 3:length(.data)]), 
        estimand = .estimand, 
        commonSup.rule = support, 
        keepTrees = T, 
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
      group.effects = T, 
      commonSup.rule = support, 
      keepTrees = T,
      seed = 2
      
    )
  }
}