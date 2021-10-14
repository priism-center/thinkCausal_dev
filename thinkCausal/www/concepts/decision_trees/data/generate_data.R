
n <- 1000
tibble(x = runif(n, 0, 100),
       y = runif(n, 0, 100)) %>% 
  mutate(label = case_when(
    x <= 30 ~ 'positive',
    y >= 50 ~ 'negative',
    x <= 65 ~ 'negative',
    TRUE ~ 'positive'
  )) %>% 
  write_csv('www/concepts/decision_trees/data/data.csv')
