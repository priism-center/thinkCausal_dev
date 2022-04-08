library(tidyverse)
dat <- read_csv('~/Dropbox/thinkCausal_dev/thinkCausal/data/IHDP_observational.csv')


transform_indicator_to_categorical <- function(.x, cat_df){
compare <- cat_df %>% 
  select(-.x)
  
  run_test <- lapply(1:length(compare), function(i){
      tmp <- tibble(cat_df[[all_of(.x)]], compare[[i]])
      rows <- tmp %>% 
        mutate(rSum = rowSums(across())) %>% 
        filter(rSum > 1) %>% 
        nrow()
      if(rows == 0) names(cat_df)[i]
    })
  
  out <- unlist(run_test)
  if(length(out) > 1) out <- c(.x, out) 
  return(out)
}

transform_indicator_to_categorical(.x = 'treat', cat_df = dat)


cat_df <- dat
