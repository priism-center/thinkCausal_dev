#' Plot common support based on the chi-squared rule
#' 
#' Returns a ggplot common support plot
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export

plot_common_support_chi <- function(.model){
  
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  
  # calculate summary stats
  total <- sum((.model$sd.cf / .model$sd.obs) ** 2 > 3.841)
  prop <- total / length(.model$sd.cf)
  chi_test <- paste0(prop, "% of cases would have been removed by the chi squared common support check")
  
  # plot it
  p <- (.model$sd.cf / .model$sd.obs)**2  %>% 
    as_tibble() %>% 
    mutate(rownumber = row_number()) %>% 
    ggplot(aes(rownumber, value)) + 
    geom_point(alpha = 0.8) + 
    geom_hline(aes(color = 'Removal threshold', yintercept = 3.841), linetype = 'dashed') + 
    scale_color_manual(values = 'coral3') +
    labs(title = NULL, 
         subtitle = paste0("Chi Squared method: ", chi_test),
         x = "Row index",
         y = 'Counterfactual Uncertanty') + 
    theme(legend.title = element_blank(), 
          legend.position = 'bottom')
  
  return(p)
}