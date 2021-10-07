#' Variable importance of Bayesian Additive Regression Trees
#'
#' Fit single regression tree on bartc() icates to produce variable importance plot & conditional effects plots.
#'
#' @param .model a model produced by bartCause::bartc()
#' @param confounders a character list of column names which should be considered the confounders. Must match the column names used to original fit .model.
#' @author George Perrett, Joe Marlo
#' @return a list containing variable importance plot & plots for each conditional effect
#' @export
#'
#' @import ggplot2 dplyr bartCause
#' @importFrom tidyr pivot_longer
#' @importFrom stats quantile reorder sd
#'
#' @examples
#' data(lalonde, package = 'arm')
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_cate_test(model_results,  c('age', 'educ'))
plot_continuous_sub <- function(.model, grouped_on, rug = F){
  
  # TODO: we need a smarter way to make these plots; it takes forever
  # and the output is messy; can we have the user specify which ones
  # they want to see?
  
  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")
  
  # pull data from model
  .data <- as.data.frame(.model$data.rsp@x)
  if (!all(grouped_on %in% colnames(.data))) stop("confounders must be within the original data used to fit .model")
  
  
  X_input <- as.matrix(.data[, grouped_on])
  
  posterior <- bartCause::extract(.model, 'icate') %>%
    as_tibble() 
  
  .data <- .data[, grouped_on] %>% as_tibble()
  names(.data) <- 'X'
  
  .data$icate.m <- apply(posterior, 2, mean)
  
  X <- rep(X_input, nrow(posterior))
  
  posterior <- posterior %>% 
    mutate(draw = 1:nrow(posterior)) %>% 
    pivot_longer(cols = 1:length(posterior))
  

  posterior <- cbind.data.frame(posterior, X)

  if(rug == T){
    p <- ggplot() + 
      geom_smooth(
        data = posterior,
        aes(X, value, group = draw),
        se = F,
        color = 'grey60'
      )  +
      geom_smooth(data = .data, aes(X, icate.m), col = 'black') 
  }
  
  else{
  p <- ggplot() + 
      geom_smooth(
        data = posterior,
        aes(X, value, group = draw),
        se = F,
        color = 'grey60'
      )  +
      geom_smooth(data = .data, aes(X, icate.m), col = 'black') + 
    geom_rug(data = .data, aes(X))
    
  }
 

  return(p)
}

# 
# dat <- read_csv('data/IHDP_observational.csv')
# covs <- dat %>% dplyr::select(2:29)
# .model <- bartc(dat$y.obs, dat$treat, as.matrix(covs))
# colnames(fit$data.rsp@x)
# plot_cate(fit, 're75')
