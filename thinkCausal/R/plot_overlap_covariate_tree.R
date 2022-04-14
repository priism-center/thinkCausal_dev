library(tidyverse)
library(bartCause)

# validate the model is a bartc model
validate_model_ <- function(.model){
  if (!inherits(.model, "bartcFit")) stop(".model must be of class 'bartcFit'")
}

# read data for testing functions
df <- read.csv("./data/IHDP_observational.csv") %>%
  select(-c(yc0hat, yc1hat))
fit <- bartc(y.obs, treat, ., data = df)


plot_overlap_covariate_tree <- function(.model, rule){
  
  validate_model_(.model)
  
  if (!rule %in% c('sd', 'chi')) stop('rule must be one of c("sd", "chi")')
  
  # pull data from model 
  .data <- as.data.frame(.model$data.rsp@x)[, -1] %>% select(-ps)
  tmp <- .data

  if(rule == "sd"){
    # calculate overlap binary variable based on sd rule
    tmp$overlap_sd <- ifelse(.model$sd.cf > max(.model$sd.obs) + sd(.model$sd.obs), 0, 1)
    # fit logistic regression to get the probability of overlap 
    fit_sd <- glm(overlap_sd ~ ., data = tmp, family = binomial(link = "logit"))
    .data$overlap_prob_sd <- predict(fit_sd, type = 'response')
    # fit regression tree
    cart <- rpart::rpart(overlap_prob_sd ~ ., data = .data)
    p <- rpart.plot::rpart.plot(cart, type = 2, branch = 1, box.palette = 0)
  }else if(rule == "chi"){
    # calculate overlap binary variable based on chi rule
    tmp$overlap_chi <- ifelse((.model$sd.cf / .model$sd.obs) ** 2 > 3.841, 0, 1)
    fit_chi <- glm(overlap_chi ~ ., data = tmp, family = binomial(link = "logit"))
    .data$overlap_prob_chi <- predict(fit_chi, type = 'response')
    # fit regression tree
    cart <- rpart::rpart(overlap_prob_chi ~ ., data = .data)
    p <- rpart.plot::rpart.plot(cart, type = 2, branch = 1, box.palette = 0)
  }
  # return(p)
}


plot_overlap_covariate_tree(fit, rule = "sd")
plot_overlap_covariate_tree(fit, rule = "chi")



