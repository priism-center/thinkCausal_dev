overlap.1 <- function(dat, selected_cols){
  
  # stop here if data hasn't been uploaded and selected
  validate(need(is.data.frame(dat), 
                "Data must be first uploaded and selected. Please see 'Data' tab."))
  
  # columns to plot
  #selected_cols <- input$analysis_plot_overlap_select_var
  
  # pivot the data
  dat_pivoted <- dat %>% 
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

overlap.2 <- function(dat){
  
shinyWidgets::show_alert(
  title = 'Reducing Dimensions...',
  # text = "Please wait",
  # type = 'info',
  # text = tags$div(
  #   class = 'spinner-grow',
  #   role = 'status',
  #   tags$span(class = 'sr-only', "Loading...")
  # ),
  text = tags$div(
    img(src = file.path('img', 'tree.gif'),
        width = "50%")
  ),
  html = TRUE,
  btn_labels = NA,
  closeOnClickOutside = FALSE
)
  
# pull the response, treatment, and confounders variables out of the df
response_v <- dat[, 2]
treatment_v <- dat[, 1]
confounders_mat <- as.matrix(dat[, 3:ncol(dat)])
dim.red_results <- bartCause::bartc(
  response = response_v,
  treatment = treatment_v,
  confounders = confounders_mat)


pscores <- dim.red_results$p.score



shinyWidgets::closeSweetAlert()


dat_pivoted <- dat %>% 
  rename(Z = starts_with("Z")) %>% 
  dplyr::select(Z) %>% 
  mutate(pscores = pscores)


p <- ggplot() + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
  geom_histogram(data = dat_pivoted %>% filter(Z == 1), 
                 aes(x = pscores, y = ..count.., fill = as_factor(Z)), 
                 alpha = 0.8)+ 
  geom_histogram(data = dat_pivoted %>% filter(Z == 0), 
                 aes(x = pscores, y = -..count.., fill = as_factor(Z)), 
                 alpha = 0.8) +
  scale_y_continuous(labels = function(brk) abs(brk)) +
  # scale_x_continuous(labels = seq(0, 1, .1)) +
  scale_fill_manual(values = c('#bd332a', '#262991')) +
  labs(title = "Overlap by treatment status",
       subtitle = 'Informative subtitle to go here',
       x = NULL,
       y = 'Count',
       fill = "Treatment")

return(p)
}