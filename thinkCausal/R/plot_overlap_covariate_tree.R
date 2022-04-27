library(tidyverse)
library(bartCause)

# validate the model is a bartc model
validate_model_ <- function(.model){
  if (!inherits(.model, "bartcFit")) stop(".model must be of class 'bartcFit'")
}

# read data for testing functions
# df <- read.csv("./data/IHDP_observational.csv") %>%
#   select(-c(yc0hat, yc1hat))
# fit <- bartc(y.obs, treat, ., data = df)


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
    cart <- rpart::rpart(overlap_prob_sd ~ ., data = .data, maxdepth = 3)
    # p <- rpart.plot::rpart.plot(cart, type = 2, branch = 1, box.palette = 0)
    p_gg <- rpart_ggplot_overlap(cart)
  }else if(rule == "chi"){
    # calculate overlap binary variable based on chi rule
    tmp$overlap_chi <- ifelse((.model$sd.cf / .model$sd.obs) ** 2 > 3.841, 0, 1)
    fit_chi <- glm(overlap_chi ~ ., data = tmp, family = binomial(link = "logit"))
    .data$overlap_prob_chi <- predict(fit_chi, type = 'response')
    # fit regression tree
    cart <- rpart::rpart(overlap_prob_chi ~ ., data = .data, maxdepth = 3)
    # p <- rpart.plot::rpart.plot(cart, type = 2, branch = 1, box.palette = 0)
    # create dendrogram
    p_gg <- rpart_ggplot_overlap(cart)
  }
  return(p_gg)
}


rpart_ggplot_overlap <- function(.model){
  
  # remove depth information from model so resulting plot is easy to read
  .model$frame$dev <- 1
  
  # extract data to construct dendrogram
  fitr <- ggdendro::dendro_data(.model)
  n_leaf <- .model$frame$n[.model$frame$var == '<leaf>']
  n_split <- .model$frame$n[.model$frame$var != '<leaf>']
  pred_split <- round(.model$frame$yval[.model$frame$var != '<leaf>'], 1)
  terminal_leaf_y <- 0.1
  leaf_labels <- tibble(
    x = fitr$leaf_labels$x,
    y = terminal_leaf_y,
    label = paste0(
      'y = ', fitr$leaf_labels$label,
      '\nn = ', n_leaf)
  )
  yes_no_offset <- c(0.65, 1.35)
  yes_no <- tibble(
    x = c(fitr$labels$x[[1]] * yes_no_offset[1],
          fitr$labels$x[[1]] * yes_no_offset[2]),
    y = rep(fitr$labels$y[[1]], 2),
    label = c("yes", "no")
  )
  split_labels <- tibble(
    x = fitr$labels$x,
    y = fitr$labels$y + 0.08,
    label = paste0(
      'y = ', pred_split,
      '\nn = ', n_split
    )
  )
  
  # set terminal segments to y = terminal_leaf_y
  initial_node_y <- fitr$labels$y[[1]]
  fitr$segments <- fitr$segments %>%
    mutate(y_new = ifelse(y > yend, y, yend),
           yend_new = ifelse(yend < y, yend, y)) %>%
    select(n, x, y = y_new, xend, yend = yend_new) %>%
    mutate(y = ifelse(y > initial_node_y, terminal_leaf_y, y),
           yend = ifelse(x == xend & x == round(x) & y > yend, terminal_leaf_y, yend))
  
  # set plot constants
  label_text_size <- 3
  x_limits <- c(0.5, nrow(fitr$leaf_labels) + 0.5)
  y_limits <- c(min(fitr$segments$y) - 0.05,
                max(fitr$segments$y) + 0.15)
  
  # plot it
  p <- ggplot() +
    geom_segment(data = fitr$segments,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_label(data = yes_no,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = leaf_labels,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = split_labels,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = fitr$labels,
               aes(x = x, y = y, label = label),
               label.size = NA, fontface = 'bold') +
    expand_limits(x = x_limits,
                  y = y_limits) +
    scale_x_continuous(labels = NULL, breaks = NULL) +
    scale_y_continuous(labels = NULL, breaks = NULL) +
    labs(title = 'Exploratory non-overlap covariates',
         x = NULL,
         y = NULL) +
    theme(panel.background = element_blank())
  
  return(p)
}

# plot_overlap_covariate_tree(fit, rule = "sd")
# plot_overlap_covariate_tree(fit, rule = "chi")
