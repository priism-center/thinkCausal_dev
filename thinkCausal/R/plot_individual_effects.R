#' Single Regression Tree for exploratory heterogeneous effects
#'
#' Fit single regression tree on bartc() icates to produce variable importance plot & table.
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @author George Perrett
#' @return a plot of ordered individual effects
#' @export 

plot_individual_effects <- function(.model, type = 'ordered'){
  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")

  icate.m <- apply(bartCause::extract(.model, 'icate'), 2, mean)
  icate.sd <- apply(bartCause::extract(.model, 'icate'), 2, sd)
  icate.ub <- icate.m + 2*icate.sd
  icate.lb <- icate.m - 2*icate.sd
  icate <- tibble(icate.m, icate.ub, icate.lb)
  
  if(type == 'ordered'){
  icate <- icate %>% arrange(desc(icate.m))
  icate <- icate %>% mutate(icate.o = row_number())

    p <- icate %>% 
    ggplot(aes(icate.o, icate.m)) + 
    geom_point(size = 1) + 
    geom_ribbon(aes(ymax = icate.ub, ymin = icate.lb), alpha = .5) + 
    labs(x = 'Effect Order', 
         y = 'Individual Treatment Effect', 
         title = 'Ordered Individual Treatment Effects', 
         subtitle = 'with 95% ci')
  }
  
  if(type == 'histogram'){
    p <- icate %>% 
      ggplot(aes(icate.m)) + 
      geom_histogram()+ 
      labs(x = 'Individual Treatment Effect', 
           y = 'frequency', 
           title = 'Histogram of Individual Treatment Effects')
  }
  return(p)
    
}
