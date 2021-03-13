#' Plot the overlap via propensity score method
#' 
#' Plot histograms showing the overlap between propensity scores by treatment status.
#'
#' @param .data dataframe. Typically store$selected_df
#' @author George Perrett
#'
#' @return ggplot object
#' @export
plot_overlap_pScores <- function(.data, plt_type) {
  
  # popup alert to user while waiting for bartc fit
  shinyWidgets::show_alert(
    title = 'Reducing Dimensions...',
    # text = "Please wait",
    # type = 'info',
    # text = tags$div(
    #   class = 'spinner-grow',
    #   role = 'status',
    #   tags$span(class = 'sr-only', "Loading...")
    # ),
    text = tags$div(img(
      src = file.path('img', 'tree.gif'),
      width = "50%"
    )),
    html = TRUE,
    btn_labels = NA,
    closeOnClickOutside = FALSE
  )
  
  # pull the response, treatment, and confounders variables out of the df
  response_v <- .data[, 2]
  treatment_v <- .data[, 1]
  confounders_mat <- as.matrix(.data[, 3:ncol(.data)])
  dim.red_results <- bartCause::bartc(response = response_v,
                                      treatment = treatment_v,
                                      confounders = confounders_mat)
  
  # pull the propensity scores
  pscores <- dim.red_results$p.score
  
  # close the popup
  shinyWidgets::closeSweetAlert()
  
  # pivot the data
  dat_pivoted <- .data %>%
    rename(Z = starts_with("Z")) %>%
    dplyr::select(Z) %>%
    mutate(pscores = pscores, 
           Z = as.logical(Z))
  
  
  if(plt_type == 'Histogram'){
  # plot it
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
    geom_histogram(data = dat_pivoted %>% filter(Z == 1),
                   aes(x = pscores, y = ..count.., fill = Z),
                   alpha = 0.8) +
    geom_histogram(data = dat_pivoted %>% filter(Z == 0),
                   aes(x = pscores, y = -..count.., fill = Z),
                   alpha = 0.8) +
    scale_y_continuous(labels = function(brk) abs(brk)) +
    # scale_x_continuous(labels = seq(0, 1, .1)) +
    scale_fill_manual(values = c('#bd332a', '#262991')) +
    labs(title = "Overlap by treatment status",
         subtitle = 'Informative subtitle to go here',
         x = NULL,y = 'Count',
         fill = "Treatment")
  }
  
  else{
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_density(data = dat_pivoted %>% filter(Z == 1),
                     aes(x = pscores, y = ..density.., fill = Z),
                     alpha = 0.8) +
      geom_density(data = dat_pivoted %>% filter(Z == 0),
                     aes(x = pscores, y = -..density.., fill = Z),
                     alpha = 0.8) +
      scale_y_continuous(labels = function(brk) abs(brk)) +
      # scale_x_continuous(labels = seq(0, 1, .1)) +
      scale_fill_manual(values = c('#bd332a', '#262991')) +
      labs(title = "Overlap by treatment status",
           subtitle = 'Informative subtitle to go here',
           x = NULL,y = 'Count',
           fill = "Treatment")
  }
  
  return(p)
}
