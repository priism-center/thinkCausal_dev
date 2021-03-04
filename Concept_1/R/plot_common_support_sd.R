#' Plot common support based on the standard deviation rule
#' 
#' Returns a ggplot common support plot
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export

plot_common_support_sd <- function(.model){
  
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  
  # calculate summary stats
  total <- sum(.model$sd.cf > max(.model$sd.obs) + sd(.model$sd.obs))
  prop <- total / length(.model$sd.cf)
  sd_test <- paste0(prop, "% of cases would have been removed by the standard deviation common support check")
  
  # plot it
  p <- .model$sd.cf %>% 
    as_tibble() %>% 
    mutate(rownumber = row_number()) %>% 
    ggplot(aes(rownumber, value)) + 
    geom_point(alpha = 0.8)+
    geom_hline(aes(yintercept = max(.model$sd.obs) + sd(.model$sd.obs)),
               color = 'coral3', linetype = 'dashed') + 
    labs(title ="Diagnostics: Common Support Checks", 
         subtitle = paste0("Standard Deviation method: ", sd_test),
         x = NULL, #"Row index",
         y = 'Counterfactual Uncertanty') + 
    theme(legend.title = element_blank(), 
          legend.position = 'bottom')
  
  return(p)
}