balance_plot <- function(dat, selected_cols){

# stop here if data hasn't been uploaded and selected
validate(need(is.data.frame(dat), 
              "Data must be first uploaded and selected. Please see 'Data' tab."))

# stop here if there are no numeric columns selected
validate(need(length(selected_cols) > 0,
              "No numeric columns selected"))

# plot it
 p <- dat %>% 
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
  ggplot(aes(x=diff, y=name, color=abs(diff))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray60') +
  geom_point(size=4) +
  scale_colour_gradient(low = 'gray30', high = 'red3') + #or should color be scaled to finite values?
  labs(title = 'Treatment and control balance',
       subtitle = 'Informative subtitle to go here',
       x = 'Scaled mean difference',
       y = NULL) +
  theme(legend.position = 'none')
 
 return(p)

}