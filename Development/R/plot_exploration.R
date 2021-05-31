#' General purpose plotting for EDA
#'
#' To be used programmatically. Not recommended for script/console use.
#'
#' @param .data typically store$selected_df
#' @param .plot_type  one of c("Pairs", 'Scatter', 'Histogram', 'Density', 'Boxplot')
#' @param .x 
#' @param .y 
#' @param .fill 
#' @param .fill_static 
#' @param .size 
#' @param shape
#' @param .alpha 
#' @param .vars_pairs 
#' @param .n_bins 
#' @param .jitter 
#' @param .groups 
#' @param .facet 
#' @param .facet_second 
#' @param .include_regression one of c("Include", "None")
#'
#' @return
#' @export
#' 
#' @seealso \code{\link{clean_detect_plot_vars}}
#' 
#' @import ggplot2
#' @importFrom GGally ggpairs
#'
#' @examples
#' library(arm)
#' data('lalonde', package = 'arm')
#' X <- lalonde
#' X <- dplyr::select(X, 'treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' X <- clean_auto_convert_logicals(X)
#' plot_exploration(
#'  .data = X,
#'  .plot_type = 'Boxplot', #c("Pairs", 'Scatter', 'Histogram', 'Density', 'Boxplot'),
#'  .x = 're78',
#'  .y = 'age',
#'  .fill = NULL,
#'  .fill_static = "#5c5980",
#'  .size = 'age',
#'  .shape = 'None',
#'  .alpha = 0.5,
#'  .vars_pairs,
#'  .n_bins = 30,
#'  .jitter = FALSE,
#'  .groups = 'None',
#'  .facet = 'None',
#'  .facet_second = 'None',
#'  .include_regression = 'None'
#' )
plot_exploration <- function(.data,
                             .plot_type = c("Pairs", 'Scatter', 'Histogram', "Barplot", 'Density', 'Boxplot'),
                             .x,
                             .y,
                             .fill,
                             .fill_static = "grey20",
                             .size,
                             .shape,
                             .alpha = 0.9,
                             .vars_pairs,
                             .n_bins,
                             .jitter,
                             .groups,
                             .facet,
                             .facet_second,
                             .include_regression = c("Include", "None")) {
  
  # convert "None"s to NULL
  if (isTRUE(.fill == "None")) .fill <- NULL
  .color <- .fill 
  if (isTRUE(.size == "None")) .size <- NULL
  if (isTRUE(.shape == "None")) .shape <- NULL
  
  # create base ggplot object
  p <- ggplot(.data, aes_string(x = sym(.x)))
  
  # pairs plot
  if (.plot_type == 'Pairs'){
    p <- GGally::ggpairs(.data[, .vars_pairs])
  }
  
  # scatter
  if (.plot_type == 'Scatter'){
    
    if (.jitter){
      p <- p +
        geom_jitter(aes_string(y = sym(as.character(.y)),
                               fill = if(is.null(.fill)) NULL else sym(.fill),
                               color = if(is.null(.color)) NULL else sym(.color),
                               size = if(is.null(.size)) NULL else sym(.size),
                               shape = if(is.null(.shape)) NULL else sym(.shape)
                               ), alpha = .alpha)
    } else {
      p <- p +
        geom_point(aes_string(y = sym(as.character(.y)),
                              fill = if(is.null(.fill)) NULL else sym(.fill),
                              color = if(is.null(.color)) NULL else sym(.color),
                              size = if(is.null(.size)) NULL else sym(.size),
                              shape = if(is.null(.shape)) NULL else sym(.shape),
                              ), alpha = .alpha)
    }
    
    # regression line
    if(.include_regression == 'Include'){
      p <- p + geom_smooth(
        aes_string(y = sym(.y)),
        method = "lm",
        formula = 'y ~ x',
        color = "grey20"
      )
    }
  }
  
  # histogram
  if (.plot_type == 'Histogram'){
    p <- p + geom_histogram(color = 'white', bins = .n_bins,
                            fill = .fill_static, alpha = 0.9) +
      labs(y = NULL)
  }
  
  # barplot
  if (.plot_type == "Barplot"){
    p <- p + geom_bar(color = 'white', fill = .fill_static, alpha = 0.9) +
      labs(y = "Count of observations")
  }
  
  # density
  if (.plot_type == 'Density'){
    p <- p + geom_density(fill = .fill_static, alpha = 0.5) +
      labs(y = NULL)
  }
  
  # boxplot
  if (.plot_type == 'Boxplot'){
    p <- p +
      geom_boxplot(fill = .fill_static, alpha = 0.5,
                   if(.groups != 'None') aes_string(y = sym(.groups))
      ) +
      coord_flip() +
      scale_y_discrete()
  }
  
  # add faceting
  if (.facet != "None"){
    
    if (.facet_second == "None"){
      p <- p + facet_grid(sym(.facet), labeller = label_both)
    } else {
      p <- p + facet_grid(list(sym(.facet), sym(.facet_second)),
                          labeller = label_both)
    }
  }

  return(p)
}
