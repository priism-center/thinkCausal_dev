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
plot_cate <- function(.model, confounder){
  
  # TODO: we need a smarter way to make these plots; it takes forever
  # and the output is messy; can we have the user specify which ones
  # they want to see?
  
  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")
  
  # pull data from model
  .data <- as.data.frame(.model$data.rsp@x)
  if (!all(confounder %in% colnames(.data))) stop("confounders must be within the original data used to fit .model")
  confounder_input <- as.matrix(.data[, confounder])
  
  # extract individual conditional effects
  icate <- bartCause::extract(.model , 'icate')
  icate.m <- apply(icate, 2, mean)
  icate.sd <- apply(icate, 2, sd)
  
  # fit regression tree
  #cart <- rpart::rpart(icate.m ~ confounders)
  
  # save variable importance
  #importance <- cart$variable.importance/sum(cart$variable.importance)*100
  # names(importance) <- sub(".", "", names(importance))
  #names(importance) <- sub("confounders", "", names(importance))
  
  # enframe and clean data
  #importance_table <- importance %>%
    # as_tibble() %>%
    # mutate(Variable = names(importance)) %>%
    # dplyr::select(Variable, value) %>%
    # rename(Importance = value)
    # 
  # plot variable importance
  # p1 <- ggplot(importance_table, aes(Importance, reorder(Variable, Importance))) +
  #   geom_segment(aes(xend = 0, yend = Variable))  +
  #   geom_point(size = 4) +
  #   labs(x = 'Importance', y = 'Variable', title = 'Potential Moderators')
  # 
  # reorder confounder matrix by var importance
  #X <- confounders[, names(importance)]
  
  # plot conditional effects
  # TODO: wheres 445 come from? should make explicit
  #   in case dependents accidental change this
  posterior <- bartCause::extract(.model, 'icate') %>%
    as_tibble() 
  posterior <- posterior %>% pivot_longer(cols = 1:length(posterior))
  
  X_long <- rep(confounder_input, 5000)
  
    if(length(unique(confounder_input)) > 2) {
      p1 <-  posterior %>%
        mutate(X = X_long) %>%
        group_by(record = name) %>%
        mutate(ci.1 = quantile(value, probs = 0.1),
               ci.9 = quantile(value, probs = 0.9),
               ci.025 = quantile(value, probs = 0.025),
               ci.975 = quantile(value, probs = 0.975),
               point = mean(value)) %>%
        dplyr::select(-c(value, name)) %>%
        distinct() %>%
        ungroup() %>%
        ggplot(aes(X, point)) +
        geom_ribbon(aes(ymin = ci.025, ymax = ci.975,fill = '95% ci'), alpha = 0.7) +
        geom_ribbon(aes(ymin = ci.1,ymax = ci.9, fill = '80% ci'), alpha = 0.7) +
        scale_fill_manual(values = c('steelblue', 'steelblue3')) +
        geom_point(size = 0.8) +
        geom_smooth(method = 'gam' ,aes(y = point, x = X), col = 'black', se = F) +
        labs(y = 'CATE', x = 'confounder')
      
      # p2 <- posterior %>%
      #   mutate(confounder = rep(confounder, 5000)) %>%
      #   ggplot(aes())
    }
  
    else{
      p1 <- posterior %>%
        mutate(X = X_long,
               X = factor(X)) %>%
        ggplot(aes(value, group = as.factor(name), col = X)) +
        geom_density(alpha = 0.7)
    }
    
  
  # 
  # # TODO: this takes >20sec to run
  # cate_plts <- list()
  # for (i in 1:ncol(X)) {
  #   cate_plts[[i]] <- ploter(X[,i])
  #   
  # }
  
  return(p1)
}

# 
# dat <- read_csv('Dropbox/PRIISM/thinkCausal_dev/Concept_1/data/lalonde.csv')
# X <- dat %>% dplyr::select(-c(treat, re78)) %>% as.matrix()
# fit <- bartc(dat$re78, dat$treat, X)
# colnames(fit$data.rsp@x)
# plot_cate(fit, 're75')
