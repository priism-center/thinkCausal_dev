library(ggplot2)
library(reactable)
library(dplyr)
setwd('inst/app/www/learn/ignorability/img/')
plots <- list()
set.seed(64)
hyperShoe <- c(rep(1, 300), rep(0, 724))
pro <- ifelse(hyperShoe == 1, rbinom(300, 1, .85), rbinom(724, 1, .15))
age <- ifelse(hyperShoe == 1, rnorm(sum(hyperShoe), 29, 5), runif(sum(hyperShoe == 0), 18, 45))
age[age < 18] <- runif(sum(age < 18), 22, 27)
y0 <- 240 + pro*-70 + age*1 +  rnorm(1024, 0, 15)
y1 <- 240 + -5 + pro*-70 + age*1 + rnorm(1024, 0, 15)
finish <- ifelse(hyperShoe == 1, y1, y0)

# first figure mean of treatment
mean_trt <- round(mean(finish[hyperShoe == 1]), 2)
plots[[length(plots) + 1]] <- tibble(finish, hyperShoe) |>
  filter(hyperShoe == 1) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = 'Treatment')) +
  geom_vline(aes(xintercept = mean_trt, linetype = paste('average finish time =', mean_trt)), col = 'black') +
  scale_linetype_manual(values = 1) +
  scale_fill_manual(values = 2) +
  #guides(linetype = guide_legend(order = 2)) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL, title = "Runners with the hyperShoe") +
  theme_classic() +
  theme(legend.position='top',
        legend.justification='left',
        legend.direction='horizontal')
plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# secound figure table to missing average PO

plots[[length(plots) + 1]] <- dplyr::tibble(`average time with hyperShoes` = mean_trt, `average time without hyperShoes` = '?', hyperShoe = 'Yes') |>
  reactable::reactable(columns = list(
    `average time with hyperShoes` = reactable::colDef(align = "center"),
    `average time without hyperShoes` = reactable::colDef(align = "center"),
    hyperShoe = reactable::colDef(align = "center")
  ))

path <- paste0('p', length(plots), '.rds')

readr::write_rds(plots[[length(plots)]], file = path)
