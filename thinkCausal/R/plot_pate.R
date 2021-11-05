#' Plot histogram of individual treatment effects
#'
#' Returns a ggplot ITE plot
#'
#' @param .model a model produced by bartCause::bartc()
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
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
#' plot_ITE(model_results)
plot_PATE <- function(.model, type = 'Histogram', ci_80 = FALSE, ci_95 = FALSE,  reference = NULL, .mean = FALSE, .median = FALSE){
  
  # TODO: Joe to rewrite
  
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  
  # calculate stats
  pate <- bartCause::extract(.model)
  pate <- as_tibble(pate)
  ub <- quantile(pate$value, .9)
  lb <- quantile(pate$value, .1)
  ub.95 <- quantile(pate$value, .975)
  lb.95 <- quantile(pate$value, .025)
  dd <- density(pate$value)
  dd <-  with(dd, data.frame(x, y))

  # plot hist
  if(type == 'Histogram'){
    
    if(ci_80 == F & ci_95 == F & .mean == F & .median == F){
      p <- pate %>%
        ggplot(aes(x = value)) +
        geom_histogram(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    
    if(ci_80 == F & ci_95 == F  & .mean == T & .median == F){
      p <- pate %>%
        ggplot(aes(x = value)) +
        geom_histogram(alpha = 0.8, fill = 'grey30') +
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        geom_vline(xintercept = reference)+
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == F & .median == T){
      p <- pate %>%
        ggplot(aes(x = value)) +
        geom_histogram(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == T & .median == T){
      p <- pate %>%
        ggplot(aes(x = value)) +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(2,3)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == F & .median == F){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference) +
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    
    
    if(ci_80 == T & ci_95 == T  & .mean == F & .median == T){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == T & .median == F){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') +  
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == T & .median == T){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    
    if(ci_80 == F & ci_95 == T  & .mean == F & .median == F){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_vline(xintercept = reference)+
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    
    if(ci_80 == F & ci_95 == T & .mean == F & .median == T){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    if(ci_80 == F & ci_95 == T  & .mean == T & .median == F){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    if(ci_80 == F & ci_95 == T & .mean == T & .median == T){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    
    if(ci_80 == T & ci_95 == F & .mean == F & .median == F){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    
    if(ci_80 == T & ci_95 == F  & .mean == F & .median == T){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    if(ci_80 == T & ci_95 == F  & .mean == T & .median == F){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
      geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    if(ci_80 == T & ci_95 == F  & .mean == T & .median == T){
      p <- ggplot(pate, aes(value)) + 
        geom_histogram(fill = 'grey60') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'frequency')
    }
    
    
  }
  
  if(type == 'Density'){


    if(ci_80 == F & ci_95 == F & .mean == F & .median == F){
      p <- pate %>%
        ggplot(aes(x = value)) +
        geom_vline(xintercept = NULL, linetype = 'dashed', color = 'grey60') +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == T & .median == F){
      p <- pate %>%
        ggplot(aes(x = value)) +
        geom_vline(xintercept = NULL, linetype = 'dashed', color = 'grey60') +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == F & .median == T){
      p <- pate %>%
        ggplot(aes(x = value)) +
        geom_vline(xintercept = NULL, linetype = 'dashed', color = 'grey60') +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == T & .median == T){
      p <- pate %>%
        ggplot(aes(x = value)) +
        geom_vline(xintercept = NULL, linetype = 'dashed', color = 'grey60') +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(2,3)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == F & .median == F){
    p <- ggplot(dd, aes(x, y)) + 
      geom_line() + 
      geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                  aes(ymax = y),
                  ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
      geom_ribbon(data = subset(dd, x > lb & x < ub),
                  aes(ymax = y),
                  ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
      geom_vline(xintercept = reference) +
      labs(title = 'Posterior of Average Treatment Effect',
           x = 'ATE',
           y = 'density')
    }
    
    
    
    if(ci_80 == T & ci_95 == T  & .mean == F & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == T & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) +  
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == T & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    
    if(ci_80 == F & ci_95 == T  & .mean == F & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    
    if(ci_80 == F & ci_95 == T & .mean == F & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    if(ci_80 == F & ci_95 == T  & .mean == T & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    if(ci_80 == F & ci_95 == T & .mean == T & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    
    if(ci_80 == T & ci_95 == F & .mean == F & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    
    if(ci_80 == T & ci_95 == F  & .mean == F & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    if(ci_80 == T & ci_95 == F  & .mean == T & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
    
    if(ci_80 == T & ci_95 == F  & .mean == T & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(pate$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(pate$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = 'ATE',
             y = 'density')
    }
  
  }
  
  return(p)
}

# plot_PATE(fit, type = 'density', interval = .95, .mean = T, .median = T, reference = 0) + theme_bw() + 
#   theme(legend.position = c(0.1, 0.9), 
#         legend.title= element_blank())

#plot_PATE(fit,ci_80 = F, .mean = T, reference = NULL)
