#' Check for common support via the standard deviation and chi-square rules
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$analysis$model$model
#'
#' @author Vince Dorie, George Perrett, Joe Marlo
#'
#' @return
#' @export
check_common_support <- function(.model){

  validate_model_fit_(.model)

  inference_group <- switch (.model$estimand,
                             ate = length(.model$sd.obs[!.model$missingRows]),
                             att = length(.model$sd.obs[!.model$missingRows] & .model$trt == 1),
                             atc = length(.model$sd.obs[!.model$missingRows] & .model$trt == 0)
  )
  
  # calculate summary stats
  sd.cut = c(trt = max(.model$sd.obs[!.model$missingRows & .model$trt > 0]), ctl = max(.model$sd.obs[!.model$missingRows & .model$trt <= 0])) + sd(.model$sd.obs[!.model$missingRows])
  total_sd <- switch (.model$estimand,
                      ate = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]) + sum(.model$sd.cf[.model$trt==0] > sd.cut[2]),
                      att = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]),
                      atc = sum(.model$sd.cf[.model$trt==0] > sd.cut[2])
  )
  

  # calculate chisqr rule
  total_chi <-  switch (.model$estimand,
                        ate = sum((.model$sd.cf / .model$sd.obs) ** 2 > 3.841),
                        att = sum((.model$sd.cf[.model$trt ==1] / .model$sd.obs[.model$trt ==1]) ** 2 > 3.841),
                        atc = sum((.model$sd.cf[.model$trt ==0] / .model$sd.obs[.model$trt ==0]) ** 2 > 3.841)
  )
    
  # calculate sd rule
  prop_sd <- round((total_sd / inference_group)*100 , 2)
  prop_chi <- round((total_chi / inference_group)*100, 2)

  
  sd_output <- paste0(prop_sd, "% of cases would have been removed under the standard deviation overlap rule")
  
  chi_output <- paste0(prop_chi, "% of cases would have been removed under the chi squared overlap rule")
  
  
  # create message to user
  common_support_message <- paste(chi_output, sd_output, sep = '<br><br><br>')

  out <- list(
    message = common_support_message,
    proportion_removed_sd = prop_sd,
    proportion_removed_chi = prop_chi
  )

  return(out)
}

