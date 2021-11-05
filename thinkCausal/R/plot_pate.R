#' Plot histogram or density of individual treatment effects
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
#' plot_PATE(model_results)
plot_PATE <- function(.model, type = 'histogram', ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){
  
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  type <- tolower(type)
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")
  
  # calculate stats
  pate <- bartCause::extract(.model)
  pate <- as_tibble(pate)
  ub <- quantile(pate$value, .9)
  lb <- quantile(pate$value, .1)
  ub.95 <- quantile(pate$value, .975)
  lb.95 <- quantile(pate$value, .025)
  dd <- density(pate$value)
  dd <-  with(dd, data.frame(x, y))
  
  # build base plot
  p <- ggplot(pate, aes(value)) + 
    scale_linetype_manual(values = c(2, 3)) + 
    theme(legend.title = element_blank()) + 
    labs(title = 'Posterior of Average Treatment Effect',
         x = 'ATE')
  
  # histogram
  if (type == 'histogram'){
    p <- p + 
      geom_histogram(fill = 'grey60') +
      labs(y = 'Frequency')
    
    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30')
  }
  
  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density')
    
    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }
  
  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pate, aes(xintercept = mean(value), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pate, aes(xintercept = median(value), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)
  
  return(p)
}
