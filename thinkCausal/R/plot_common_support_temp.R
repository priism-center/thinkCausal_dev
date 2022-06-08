plot_common_support_temp <- function(.model, .x = 'Propensity Score',  rule = c('both', 'sd', 'chi')){
  
  # ensure model is a of class bartcFit
  validate_model_(.model)
  
  rule <- rule[1]
  if (rule %notin% c('both', 'sd', 'chi')) stop('rule must be one of c("both", "sd", "chi")')
  if (rule == 'both') rule <- c('sd', 'chi')
  
  # calculate summary stats
  sd.cut = c(trt = max(.model$sd.obs[!.model$missingRows & .model$trt > 0]), ctl = max(.model$sd.obs[!.model$missingRows & .model$trt <= 0])) + sd(.model$sd.obs[!.model$missingRows])
  total_sd <- switch (.model$estimand,
                      ate = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]) + sum(.model$sd.cf[.model$trt==0] > sd.cut[2]),
                      att = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]),
                      atc = sum(.model$sd.cf[.model$trt==0] > sd.cut[2])
  )
  
  inference_group <- switch (.model$estimand,
                             ate = length(.model$sd.obs[!.model$missingRows]),
                             att = length(.model$sd.obs[!.model$missingRows] & .model$trt == 1),
                             atc = length(.model$sd.obs[!.model$missingRows] & .model$trt == 0)
  )

  
  # create dataframe of the sd and chi values
  dat <- as_tibble(.model$data.rsp@x) %>%
    rename(`Propensity Score` = ps)
  
  dat.sd <- dat %>%
    mutate(sd.cut = if_else(.model$trt == 1, sd.cut[1], sd.cut[2]),
           removed = if_else(.model$sd.cf > sd.cut, 'Removed', 'Included'),
           support_rule = 'sd',
           stat = .model$sd.cf,
           sd.cf = .model$sd.cf) %>%
    select(-sd.cut)
  
  
  dat.chi <- dat %>%
    mutate(removed = if_else((.model$sd.cf / .model$sd.obs) ** 2 > 3.841, 'Removed', 'Included'),
           support_rule = 'chi',
           stat = (.model$sd.cf / .model$sd.obs) ** 2,
           sd.cf = .model$sd.cf)
  
  dat <- rbind(dat.sd, dat.chi)

  if(.model$estimand == 'att') dat <- dat[rep(.model$trt, 2) == 1,]
  if(.model$estimand == 'atc') dat <- dat[rep(.model$trt, 2) == 0,]
  
  prop <- dat %>% 
    group_by(support_rule) %>% 
    count(removed) %>% 
    mutate(prop = n/sum(n)*100) %>% 
    filter(removed == 'Removed') %>% 
    arrange(support_rule) %>% 
    ungroup() %>% 
    select(prop) %>% 
    as_vector() %>% 
    round(2)
  
  # orderd alphabetically
  if(is.na(prop[1])) prop[1] <- 0
  if(is.na(prop[2])) prop[2] <- 0
  
  dat <- dat %>% 
    mutate(support_rule_text = if_else(support_rule == 'chi', 
                                       paste0('Chi-squared rule: ', prop[1], "% of cases would have been removed"), 
                                       paste0('Standard deviation rule: ', prop[2], "% of cases would have been removed")))

  # plot it
  p <- dat %>%
    filter(support_rule %in% rule) %>%
    ggplot(aes(x = !!rlang::sym(.x), y = sd.cf, color = removed)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c(1, 2)) +
    facet_wrap(~support_rule_text, ncol = 1, scales = 'free_y') +
    labs(title ="Common support checks",
         x = .x,
         y = 'Predicted counterfactual standard deviation',
         color = NULL) +
    theme(legend.position = 'bottom',
          strip.text = element_text(hjust = 0))
  
  return(p)
}
