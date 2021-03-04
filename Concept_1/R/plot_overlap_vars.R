#' Plot the overlap of variables
#' 
#' Plot histograms showing the overlap between variables by treatment status.
#'
#' @param .data dataframe. Typically store$selected_df
#' @param selected_cols list of columns from .data to plot
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
plot_overlap_vars <- function(.data, selected_cols){
  
  # pivot the data
  dat_pivoted <- .data %>% 
    rename(Z = starts_with("Z")) %>% 
    dplyr::select(all_of(c(selected_cols, "Z"))) %>% 
    pivot_longer(cols = -Z) %>% 
    mutate(Z = as.logical(Z))
  
  # histograms showing overlaps
  p <- ggplot() + 
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
    geom_histogram(data = dat_pivoted %>% filter(Z == 1), 
                   aes(x = value, y = ..density.., fill = Z), 
                   alpha = 0.8)+ 
    geom_histogram(data = dat_pivoted %>% filter(Z == 0), 
                   aes(x = value, y = -..density.., fill = Z), 
                   alpha = 0.8) +
    scale_y_continuous(labels = function(brk) abs(brk)) +
    scale_fill_manual(values = c('#bd332a', '#262991')) +
    facet_wrap(~name, scales = 'free', ncol = 3) +
    labs(title = "Overlap by treatment status",
         subtitle = 'Informative subtitle to go here',
         x = NULL,
         y = 'Density',
         fill = "Treatment")
  
  return(p)
}
