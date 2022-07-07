#' @title Plot the overlap of variables
#' @description Plot histograms showing the overlap between variables by treatment status.
#'
#' @param .data dataframe
#' @param treatment character. Name of the treatment column within .data
#' @param confounders character list of column names denoting confounders within .data
#' @param plot_type the plot type, one of c('histogram', 'density'). Defaults to 'histogram'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @seealso \code{\link{plot_overlap_pScores}}
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' data(lalonde)
#' plot_overlap_vars(
#'  .data = lalonde,
#'  treatment = 'treat',
#'  confounders = c('age', 'educ'),
#'  plot_type = 'Histogram'
#')
#'



`%notin%` <- Negate(`%in%`)

# coerce_to_logical_(c(1, 0, 'T', FALSE, '0', '1'))
coerce_to_logical_ <- function(x){
  x[x %in% c(1, '1')] <- TRUE
  x[x %in% c(0, '0')] <- FALSE
  x <- as.logical(x)
  if (!is.logical(x) | sum(is.na(x)) > 0) stop("treatment_col must be logical with no NAs")
  return(x)
}

# validate the model is a bartc model
validate_model_ <- function(.model){
  if (!inherits(.model, "bartcFit")) stop(".model must be of class 'bartcFit'")
}

is_numeric_vector_ <- function(x){
  if (!inherits(x, 'numeric')) stop('moderator must be numeric vector')
}

is_discrete_ <- function(x){
  # must be more than one level and all levels can't be unique
  is_discrete <- length(unique(x)) > 1 && length(unique(x)) < length(x)
  if (!isTRUE(is_discrete)) stop('moderator must be discrete')
}

# adjust [moderator] to match estimand
adjust_for_estimand_ <- function(.model, x){
  validate_model_(.model)
  
  out <- switch(
    .model$estimand,
    ate = x,
    att = x[.model$trt == 1],
    atc = x[.model$trt != 1]
  )
  
  return(out)
}

# used within plot_moderator_c_pd()
fit_pd_ <- function(x, z1, z0, index, .model){
  z1[, index] <- x
  z0[, index] <- x
  preds.1 <- predict(.model, newdata = z1)
  preds.0 <- predict(.model, newdata = z0)
  preds <- preds.1 - preds.0
  
  cate <- apply(preds, 1, mean)
  return(cate)
}

pclamp_ <- function(x, x_min, x_max) pmin(x_max, pmax(x, x_min))

# to satisfy CMD CHECK when using pipe variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      'x',
      'xend',
      'y',
      'yend',
      'y_new',
      'yend_new',
      'name',
      'value',
      'support_rule',
      'index',
      'threshold',
      'point',
      '.min',
      '.max',
      'label',
      'Z',
      'Z_treat',
      '..count..',
      '..density..',
      'iteration',
      'Chain',
      'icate.o',
      'ci_2.5',
      'ci_97.5',
      'ci_10',
      'ci_90'
    )
  )
}




temp_plot_overlap_vars <- function(.data, treatment, confounders, plot_type = c("histogram", "density")){
  
  plot_type <- tolower(plot_type[1])
  if (plot_type %notin% c('histogram', 'density')) stop('plot_type must be one of c("histogram", "density"')
  if (treatment %notin% colnames(.data)) stop('treatment not found in .data')
  if (any(confounders %notin% colnames(.data))) stop('Not all confounders are found in .data')
  

  # coerce treatment column to logical
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])
  
  # extract the relevant columns and rename treatment column
  .data <- .data[, c(treatment, confounders)]
  colnames(.data) <- c("Z_treat", confounders)
  
  # pivot the data
  dat_pivoted <- pivot_longer(.data, cols = -Z_treat)
  
    if (plot_type == 'histogram'){
    # histograms showing overlaps
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60')
    
    if(is.numeric(.data[[confounders]])){
      p <- p + geom_histogram(data = filter(dat_pivoted, Z_treat == 1),
                              aes(x = value, y = ..count.., fill = Z_treat),
                              alpha = 0.8) +
        geom_histogram(data = filter(dat_pivoted, Z_treat == 0),
                       aes(x = value, y = -..count.., fill = Z_treat),
                       alpha = 0.8)
    }else{
      p <- p + geom_bar(data = filter(dat_pivoted, Z_treat == 1),
                        aes(x = value, y = ..count.., fill = Z_treat),
                        alpha = 0.8) +
        geom_bar(data = filter(dat_pivoted, Z_treat == 0),
                 aes(x = value, y = -..count.., fill = Z_treat),
                 alpha = 0.8)
    }
    p <- p +  scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c('#bd332a', '#262991')) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Count',
           fill = "Treatment")
    
  }
  
  if (plot_type == 'density') {
    # density plots showing overlaps
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_density(data = filter(dat_pivoted, Z_treat == 1),
                   aes(x = value, y = ..density.., fill = Z_treat),
                   alpha = 0.8)+
      geom_density(data = filter(dat_pivoted, Z_treat == 0),
                   aes(x = value, y = -..density.., fill = Z_treat),
                   alpha = 0.8) +
      scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c('#bd332a', '#262991')) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Density',
           fill = "Treatment")
    
  }
  
  return(p)
}
