#' Check for common support via the standard deviation and chi-square rules
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#'
#' @author George Perrett, Joe Marlo
#'
#' @return
#' @export
check_common_support <- function(.model){
  
  validate_model_fit_(.model)
  
  # check using standard deviation rule
  total <- sum(.model$sd.cf > max(.model$sd.obs) + sd(.model$sd.obs))
  prop_sd <- round(total / length(.model$sd.cf), 2)*100
  sd_output <- paste0(prop_sd, "% of cases would have been removed under the standard deviation common support rule")
  
  # check use chi-square rule
  total_chi <- sum(((.model$sd.cf / .model$sd.obs) ** 2) > 3.841)
  prop_chi <- round(total_chi / length(.model$sd.cf), 2)*100
  chi_output <- paste0(prop_chi, "% of cases would have been removed under the chi squared common support rule")
  
  # create message to user
  common_support_message <- paste(sd_output, chi_output, sep = '<br>')
  
  out <- list(
    message = common_support_message,
    proportion_removed_sd = prop_sd,
    proportion_removed_chi = prop_chi
  )
  
  return(out)
}
