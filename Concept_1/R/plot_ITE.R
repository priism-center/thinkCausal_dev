#' Plot histogram of individual treatment effects
#' 
#' Returns a ggplot ITE plot
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
plot_ITE <- function(.model){
  
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  
  # calculate stats
  ites <- bartCause::extract(.model, 'icate')
  ite.m <- apply(ites, 2, mean)
  sd.ite <- apply(ites, 2, sd)
  ite.ub <- ite.m + 1.96 * sd.ite
  ite.lb <-  ite.m - 1.96 * sd.ite
  
  # plot it
  p <- tibble(ite.m, ite.ub, ite.lb) %>% 
    # arrange(ite.m) %>% 
    # mutate(rank = row_number()) %>% 
    ggplot(aes(x = ite.m)) + 
    geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey60') +
    geom_histogram(alpha = 0.8) + 
    labs(title = 'Individual Treatment Effects',
         x = base::toupper(.model$estimand), 
         y = 'Frequency')
  
  return(p)
}