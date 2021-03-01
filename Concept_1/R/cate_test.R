### cate_test
# Author: George Perrett
# Inputs: .fit= output from bartc and matrix of confounders
# Purpose: Fit single regression tree on bartc() icates to produce 
# variable importance plot & conditional effects plots.
# Output: 2 objects: variable importance plot & plots for each conditional effect


cate_test <- function(.fit = fit, confounders = X){
  # extract individual conditional effects 
  icate <- bartCause::extract(.fit , 'icate')
  icate.m <- apply(icate, 2, mean)
  icate.sd <- apply(icate, 2, sd)
  
  # fit regression tree 
  cart <- rpart::rpart(icate.m ~ confounders)
  # svae variable importance
  importance <- cart$variable.importance/sum(cart$variable.importance)*100
  names(importance) <- sub(".", "", names(importance))
  
  importance_table <- importance %>% 
    as_tibble() %>% 
    mutate(Variable = names(importance)) %>% 
    dplyr::select(Variable, value) %>% 
    rename(Importance = value) 
  
  # plot variable importance
  p1 <- ggplot(importance_table, aes(Importance, reorder(Variable, Importance))) + 
    geom_segment(aes(xend = 0, yend = Variable))  +
    geom_point(size = 4) + 
    labs(x = 'Importance', y = 'Variable', title = 'Potential Moderators')
  
  # Reorder confounder matrix by var importance
  X <- X[, names(importance)]
  
  # plot conditional effects
  posterior <- bartCause::extract(.fit, 'icate') %>% 
    as_tibble() %>% 
    pivot_longer(cols = 1:445)
  
  ploter <- function(x) {
    if(length(unique(x)) > 2) {
      p <-  posterior %>% 
        mutate(confounder = rep(x, 5000)) %>% 
        group_by(record = name) %>% 
        mutate(ci.1 = quantile(value, probs = .1), 
               ci.9 = quantile(value, probs = .9), 
               ci.025 = quantile(value, probs = .025), 
               ci.975 = quantile(value, probs = .975), 
               point = mean(value)) %>% 
        dplyr::select(-c(value, name)) %>% 
        distinct() %>% 
        ungroup() %>% 
        ggplot(aes(confounder, point)) + 
        geom_ribbon(aes(ymin = ci.025, ymax = ci.975,fill = '95% ci'), alpha = .7) + 
        geom_ribbon(aes(ymin = ci.1,ymax = ci.9, fill = '80% ci'), alpha = .7) + 
        scale_fill_manual(values = c('steelblue', 'steelblue3')) + 
        geom_point(size = .8) + 
        geom_smooth(method = 'gam' ,aes(y = point, x = confounder), col = 'black', se = F) + 
        labs(y = 'CATE')
    }
    else{
      p <- posterior %>%
        mutate(confounder = rep(x, 5000),
               confounder = factor(confounder)) %>%
        ggplot(aes(value, group = as.factor(name), col = confounder)) +
        geom_density(alpha = .7)
    }
    
    return(p)
  }
  
  cate_plts <- list()
  for (i in 1:ncol(X)) {
    cate_plts[[i]] <- ploter(X[,i])
    
  }
  
  results <- list(p1, cate_plts)
  return(results)
}
