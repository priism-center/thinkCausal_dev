#' Single Regression Tree for exploratory heterogenious effects
#'
#' Fit single regression tree on bartc() icates to produce variable importance plot & table.
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @param confounders matrix of confounders
#' @author George Perrett
#' @return a list containing variable importance plot & ordered table of confounders by scaled importance
#' @export 

plot_single_tree <- function(.model, confounders, depth = 2){
  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")
  if (!is.matrix(confounders)) stop("confounders must be of class matrix")
  icate.m <- apply(extract(.model, 'icate'), 2, mean)
  
  # fit regression tree
  cart <- rpart::rpart(icate.m ~ ., data = as.data.frame(confounders), maxdepth = depth)
  
  p <- rpart.plot(cart)
  return(p)
}