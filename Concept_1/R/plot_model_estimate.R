#' Plot the estimands of a bartCause::bartc() model
#' 
#' Returns a ggplot of the estimated effect
#'
#' @param model a bartCause::bartc() model, typically store$model_results
#' @author Joe Marlo, George Perrett
#'
#' @return ggplot object
#' @export 
plot_model_estimate <- function(model){
  
  p <- model %>% 
    bartCause::extract() %>% 
    as_tibble() %>% 
    mutate(index = row_number()) %>% 
    ggplot(aes(x = index, y = value)) + 
    geom_line() + 
    labs(title = 'Diagnostics: Trace Plot', 
         subtitle = 'Informative subtitle to go here',
         x = 'Iteration', 
         y = base::toupper(model$estimand))
  
  return(p)
}