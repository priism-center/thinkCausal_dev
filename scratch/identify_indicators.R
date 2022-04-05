library(tidyverse)
dat <- read_csv('~/Dropbox/thinkCausal_dev/thinkCausal/data/IHDP_observational.csv')
cats <- dat %>% select(contains('mom'), -momage, ark, ein, har, mia, pen, tex, was, cig, drugs, booze, work.dur, prenatal, twin)


get_groups <- function(selected_x, cat_df){
compare <- cat_df %>% 
  select(-selected_x)
  
  run_test <- lapply(1:length(compare), function(i){
      tmp <- tibble(cat_df[[all_of(selected_x)]], compare[[i]])
      rows <- tmp %>% 
        mutate(rSum = rowSums(across())) %>% 
        filter(rSum > 1) %>% 
        nrow()
      if(rows == 0) i
    })
  
  out <- names(compare)[unlist(run_test)]
  out <- c(test, out)
  return(out)
}

get_groups(test = 'mom.lths', cat_df = cats)


