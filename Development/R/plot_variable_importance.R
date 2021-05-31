#' Variable importance of Bayesian Additive Regression Trees
#'
#' Fit single regression tree on bartc() icates to produce variable importance plot & table.
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @param confounders matrix of confounders
#' @author George Perrett, Joe Marlo
#' @return a list containing variable importance plot & ordered table of confounders by scaled importance
#' @export

plot_variable_importance <- function(.model, confounders, out = 'all'){
  
  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")
  if (!is.matrix(confounders)) stop("confounders must be of class matrix")
  
  # extract individual conditional effects 
  icate <- bartCause::extract(.model , 'icate')
  icate.m <- apply(icate, 2, mean)
  icate.sd <- apply(icate, 2, sd)
  
  # fit regression tree 
  cart <- rpart::rpart(icate.m ~ confounders)
  
  # save variable importance
  importance <- cart$variable.importance/sum(cart$variable.importance)*100
  # names(importance) <- sub(".", "", names(importance))
  names(importance) <- sub("confounders", "", names(importance))
  
  # enframe and clean data
  importance_table <- importance %>% 
    as_tibble() %>% 
    mutate(Variable = names(importance)) %>% 
    dplyr::select(Variable, value) %>% 
    rename(Importance = value) %>% 
    arrange(desc(Importance)) 
  
  # plot variable importance
  importance_table_plt <- importance_table %>% slice(1:20)
  p1 <- ggplot(importance_table_plt, aes(Importance, reorder(Variable, Importance))) + 
    geom_segment(aes(xend = 0, yend = Variable))  +
    geom_point(size = 4) + 
    labs(x = 'Importance', y = 'Variable', title = 'Potential Moderators')
  
  if(out ==  'all'){
    results(list(p1, importance_table))
  }
  
  if(out == 'plot'){
    results <- p1
  }
  
  if(out == 'table'){
    results <- data.frame(importance_table)
  }
  
  return(results)
}