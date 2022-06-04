#' Single Regression Tree for common support diagnostic
#'
#' Fit single regression tree on bartc() icates to produce variable importance plot & table.
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$analysis$model$model
#' @param confounders matrix of confounders
#' @author George Perrett
#' @return a list containing variable importance plot & ordered table of confounders by scaled importance
#' @export 

plot_tree_diagnostic <- function(.model, depth = 2){

  validate_model_fit_(.model)
  
  .data <- as.data.frame(.model$data.rsp@x)
  .data <- .data[1:(length(.data)-2)]
  sd_violation <- .model$sd.cf > (max(.model$sd.obs) + sd(.model$sd.obs))
  chi_violation <- ((.model$sd.cf / .model$sd.obs) ** 2) > 3.841
 
  # fit regression tree
  cart.sd <- rpart::rpart(sd_violation ~ ., data = as.data.frame(.data), maxdepth = depth, method = 'class')
  cart.chi <- rpart::rpart(chi_violation ~ ., data = as.data.frame(.data), maxdepth = depth, method = 'class')
  
  p.sd <- rpart.plot(cart.sd)
  p.chi <- rpart.plot(cart.chi)
  rules <- rpart.rules(cart.sd)
  p <- list(p.sd, p.chi, rules)
  return(p)
}