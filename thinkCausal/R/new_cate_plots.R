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

plot_categorical_sub <- function(.model, grouped_on, rug = F){
  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")
  
  # pull data from model
  .data <- as.data.frame(.model$data.rsp@x)
  if (!all(grouped_on %in% colnames(.data))) stop("confounders must be within the original data used to fit .model")
  
  if(length(grouped_on) > 1){
  X_input <- .data[, grouped_on]
  X_input <-  X_input %>% pivot_longer(1:length(X_input)) %>% 
    filter(value == 1) %>% 
    select(name) %>% 
    rename(covariate = name)
  }
  else{
    X_input <- .data[, grouped_on]
  }
  
  posterior <- bartCause::extract(.model, 'icate') %>%
    as_tibble() 
  X <- rep(X_input$covariate, nrow(posterior))
  
  posterior <- posterior %>% 
    pivot_longer(1:length(posterior))
  posterior <- cbind.data.frame(posterior, X)
  
  means <- posterior %>% 
    group_by(X) %>% 
    summarise(point = mean(value))  %>% 
    ungroup() %>% 
    rename(variable = X)
  
  bounds <- posterior %>% group_by(X) %>% summarise(ninety = quantile(value, .9), 
                                          ten = quantile(value, .1), 
                                          nine_seven_five = quantile(value, .975), 
                                          zero_two_five = quantile(value, .025))
  
  density.df <- NULL
  cats <- unique(posterior$X)
  
  for (i in 1:length(cats)) {
    
    subset <- posterior %>% filter(X == cats[i])
    dd <- density(subset$value)
    dd <-  with(dd, data.frame(x, y))
    dd$class <- subset$X[1]
    dd$ub <- quantile(subset$value, .9)
    dd$lb <- quantile(subset$value, .1)
    dd$ub.95 <- quantile(subset$value, .975)
    dd$lb.95 <- quantile(subset$value, .025)
    if(is.null(density.df)){
      density.df <- dd
    }
    else{
      density.df <- rbind.data.frame(density.df, dd)
    }
  }
  
  
  p <- ggplot(density.df, aes(x, y)) + 
    geom_line() + 
    geom_ribbon(data = subset(density.df, x > lb.95 & x < ub.95),
                aes(ymax = y),
                ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
    geom_ribbon(data = subset(density.df, x > lb & x < ub),
                aes(ymax = y),
                ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
    geom_vline(data = means, aes(xintercept = point, col = variable)) + 
    facet_wrap(~class, ncol = 1) + 
      labs(x = 'Treatment Effect') 
  return(p)
  
  }

# # 
# dat <- read_csv('data/IHDP_observational.csv')
# covs <- dat %>% dplyr::select(2:29)
# .model <- bartc(dat$y.obs, dat$treat, as.matrix(covs))
# colnames(fit$data.rsp@x)
# plot_cate(fit, 're75')
