#' Plot histogram of individual treatment effects
#'
#' Returns a ggplot ITE plot
#'
#' @param .model a model produced by bartCause::bartc()
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause methods
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

plot_continuous_categorical_moderator <- function(.model, X, D, reference_line = NULL){
  
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  
  dat <- as_tibble(.model$data.rsp@x)
  names(dat)[names(dat) == X] <- 'x'
  names(dat)[names(dat) == D] <- 'd'
  
  icate_post <- bartCause::extract(.model, 'icate')
  dat$icate.m <- apply(icate_post, 2, mean)
  
  all.sims <- data.frame(icate_post) %>%
    mutate(draw = 1:(.model$n.chains*500)) %>% 
    pivot_longer(1:nrow(dat)) %>% 
    mutate(X_var = rep(dat$x, (.model$n.chains*500)), 
           D_var = rep(dat$d, (.model$n.chains*500)))
  
  
  p <- ggplot() + 
    geom_smooth(data = all.sims, 
                aes(X_var, value,
                    group = interaction(draw, D_var), 
                    color = as.factor(D_var)),
                se = F) + 
    geom_smooth(
      data = dat,
      aes(x, icate.m, group = d, linetype = as.factor(d)),
      se = F,
      col = 'black'
    ) + 
    labs(x = X,  y = 'ICATE', title = paste('ICATE by', X, 'and', D))
  
  return(p)
  
}
