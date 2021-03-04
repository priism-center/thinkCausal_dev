#' Plot the balance
#'
#' Returns a ggplot balance plot
#'
#' @param X dataframe formatted with Z, Y, and X variables. Typically store$selected_df
#' @param selected_cols character list of column names to plot. Typically input$analysis_plot_balance_select_var
#' @author Joe Marlo
#'
#' @return ggplot object
#' @export
#'
plot_balance <- function(.data, selected_cols){

 p <- .data %>% 
  rename(Z = starts_with("Z")) %>% 
  dplyr::select(all_of(c(selected_cols, "Z"))) %>% 
  pivot_longer(cols = -Z) %>% 
  group_by(name) %>% 
  mutate(value = scale(value)[,1]) %>%
  group_by(name, Z) %>% 
  summarize(mean = mean(value),
            .groups = 'drop') %>% 
  group_by(name) %>% 
  summarize(diff = mean - lag(mean),
            .groups = 'drop') %>% 
  na.omit() %>% 
   ggplot(aes(x = diff, y = name, color = abs(diff))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray60') +
   geom_point(size = 4) +
  scale_colour_gradient(low = 'gray30', high = 'red3') + #or should color be scaled to finite values?
  labs(title = 'Treatment and control balance',
       subtitle = 'Informative subtitle to go here',
       x = 'Scaled mean difference',
       y = NULL) +
  theme(legend.position = 'none')
 
 return(p)
}