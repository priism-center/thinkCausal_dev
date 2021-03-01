### model estimand plot
# Author: Joe Marlo & George Perrett
# Purpose: To plot the estimate from the bartc() function
# Inputs: takes 'model_results' from the store (store$model_results)
# Outputs: single ggplot of the estimated effect

model_estimate_plot <- function(dat){

  # extract model from store
  mod <- dat
  
  # plot it
  p <- mod %>% 
    bartCause::extract() %>% 
    as_tibble() %>% 
    mutate(index = row_number()) %>% 
    ggplot(aes(x = index, y = value)) + 
    geom_line() + 
    labs(title = 'Diagnostics: Trace Plot', 
         subtitle = 'Informative subtitle to go here',
         x = 'Iteration', 
         y = base::toupper(mod$estimand))
  
  return(p)
}