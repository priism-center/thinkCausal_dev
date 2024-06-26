library(ggplot2); theme_set(theme_classic() + theme(legend.text=element_text(size=14),
                                  axis.title = element_text(size=14),
                                  legend.position='top',
                                  legend.justification='left',
                                  legend.direction='horizontal'))
library(reactable)
library(dplyr)
setwd('inst/app/www/learn/confounder/plots/')
plots <- list()
set.seed(64)
hyperShoe <- c(rep(1, 300), rep(0, 724))
pro <- ifelse(hyperShoe == 1, rbinom(300, 1, .85), rbinom(724, 1, .15))
non_us <- c(rbinom(300, 1, .15), rbinom(724, 1, .85))
y0 <- 240 + pro*-70  +non_us*-15 + rnorm(1024, 0, 15)
y1 <- 240 + -5 + pro*-70  +non_us*-15 + rnorm(1024, 0, 15)
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
  guides(linetype = guide_legend(order = 2)) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL, title = "Runners with the hyperShoe")

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

# third figure average time of the control group
mean_ctl <- round(mean(finish[hyperShoe == 0]), 2)
plots[[length(plots) + 1]] <-
tibble(finish, hyperShoe) |>
  filter(hyperShoe == 0) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = 'Control')) +
  geom_vline(aes(xintercept = mean_ctl, linetype = paste('average finish time =', mean_ctl)), col = 'black') +
  scale_linetype_manual(values = 2) +
  scale_fill_manual(values = 4) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL, title = "Runners without the hyperShoe")
plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# fourth figure CO table for control
plots[[length(plots) + 1]] <- tibble(`average time with hyperShoes` = '?', `average time without hyperShoes` = mean_ctl, hyperShoe = 'No') |>
  reactable::reactable(columns = list(
    `average time with hyperShoes` = reactable::colDef(align = "center"),
    `average time without hyperShoes` = reactable::colDef(align = "center"),
    hyperShoe = reactable::colDef(align = "center")
  ))

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# fifth table with missing pos both conditions
plots[[length(plots) + 1]] <-
  tibble(`average time with hyperShoes` = c(mean_trt, '?'),
          `average time without hyperShoes` = c('?', mean_ctl), hyperShoe = c('Yes', 'No')) |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(align = "center"),
      `average time without hyperShoes` = colDef(align = "center"),
      hyperShoe = colDef(align = "center")
    ))

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# six table fill in treatment
tab_dat <- tibble(
  `average time with hyperShoes` = c(mean_trt, '?'),
  `average time without hyperShoes` = c(mean_ctl, mean_ctl),
  hyperShoe = c('Yes', 'No')
)
plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(align = "center"),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# figure 7 table fill in control
tab_dat <- tibble(
  `average time with hyperShoes` = c(mean_trt, mean_trt),
  `average time without hyperShoes` = c(mean_ctl, mean_ctl),
  hyperShoe = c('Yes', 'No')
)
plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
        ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# figure 8
plots[[length(plots) + 1]] <- tibble(finish, hyperShoe) |>
  mutate(hyperShoe = ifelse(hyperShoe == 1, 'Treatment', 'Control')) |>
  mutate(hyperShoe = factor(hyperShoe, levels = c('Treatment', 'Control'))) |>
  mutate(mean = ifelse(hyperShoe == 'Treatment', mean_trt, mean_ctl)) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = hyperShoe), position = 'identity') +
  geom_vline(aes(xintercept = mean, linetype = hyperShoe), col = 'black') +
  scale_fill_manual(values = c(2, 4),
                    labels = c(
                      paste('Treatment mean =', mean_trt),
                      paste('Control mean =', mean_ctl))) +
  scale_linetype_manual(values = c(1, 2),
                        labels = c(
                          paste('Treatment mean =', mean_trt),
                          paste('Control mean =', mean_ctl))
  ) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL,
       title = "Difference in Means",
       subtitle =
         paste('ATE =', mean_trt, '-', mean_ctl, '=', mean_trt - mean_ctl)) +
  facet_wrap(~hyperShoe, ncol = 1)

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# fi9 9 is repeat of filled in table
tab_dat <- tibble(
  `average time with hyperShoes` = c(mean_trt, mean_trt),
  `average time without hyperShoes` = c(mean_ctl, mean_ctl),
  hyperShoe = c('Yes', 'No'),
  ATE = c(-43.07, -43.07)
)
plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center"),
      ATE = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# fig 10 blank figure
# plots[[length(plots) + 1]] <-
#   ggplot() +
#   theme_void()
#
# plots[[length(plots)]]
# path <- paste0('p', length(plots), '.png')
# ggsave(path)

# fig 11 blank
plots[[length(plots) + 1]] <-
  ggplot() +
  theme_void()

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)
# fig 12 icon array treatment
grid <- tibble(hyperShoe, pro) |>
  arrange(hyperShoe, pro)

grid <- cbind.data.frame(grid, tidyr::crossing(x = 1:32, y = 1:32))
grid$x <- as.factor(grid$x)
grid$y <- as.factor(grid$y)

plots[[length(plots) + 1]] <- grid |>
  filter(hyperShoe == 1) |>
  ggplot(aes(x, y, shape = as.factor(pro))) +
  geom_point(col = 2) +
  scale_shape_manual(values = c(21, 19), labels = c('Amateur', 'Professional')) +
  theme_void() +
  theme(legend.text=element_text(size=14),
          legend.position='top',
          legend.justification='left',
          legend.direction='horizontal') +
  labs(title = 'The 300 runners that wore hyperShoes', shape = 'Professional Status:')

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# fig 13 icon array control
plots[[length(plots) + 1]] <- grid |>
  filter(hyperShoe == 0) |>
  ggplot(aes(x, y, shape = as.factor(pro))) +
  geom_point(col = 4) +
  scale_shape_manual(values = c(21, 19), labels = c('Amateur', 'Professional')) +
  theme_void() +
  theme(legend.text=element_text(size=14),
          legend.position='top',
          legend.justification='left',
          legend.direction='horizontal') +
  labs(title = 'The 724 runners that did not wear hyperShoes', shape = 'Professional Status:')

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# fig 14 icon array of everyone
plots[[length(plots) + 1]] <- grid |>
  ggplot(aes(x, y, shape = as.factor(pro))) +
  geom_point(aes(col = as.factor(hyperShoe))) +
  scale_shape_manual(values = c(21, 19), labels = c('Amateur', 'Professional')) +
  scale_color_manual(values = c(4, 2), labels = c('Control', 'Treatment')) +
  theme_void() +
  theme(legend.text=element_text(size=14),
          legend.position='top',
          legend.justification='left',
          legend.direction='horizontal')+
  labs(title = 'All runners', shape = NULL, col = NULL)

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# fig 15 outcome by pro status

plots[[length(plots) + 1]] <- tibble(pro, finish) |>
  mutate(pro = ifelse(pro == 1, 'Professional', 'Amateur')) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white') +
  facet_wrap(~pro, ncol = 1) +
  labs(x = 'Finishing time in minutes')

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# blank
plots[[length(plots) + 1]] <- ggplot() + theme_void()
plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# observed Y|Pro
mean_trt <- round(mean(finish[hyperShoe == 1 & pro == 1]), 2)
mean_ctl <- round(mean(finish[hyperShoe == 0 & pro == 1]), 2)

plots[[length(plots) + 1]] <- tibble(finish, hyperShoe, pro) |>
  filter(pro == 1)  |>
  mutate(hyperShoe = ifelse(hyperShoe == 1, 'Treatment', 'Control')) |>
  mutate(hyperShoe = factor(hyperShoe, levels = c('Treatment', 'Control'))) |>
  mutate(mean = ifelse(hyperShoe == 'Treatment', mean_trt, mean_ctl)) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = hyperShoe), position = 'identity') +
  geom_vline(aes(xintercept = mean, linetype = hyperShoe), col = 'black') +
  scale_fill_manual(values = c(2, 4),
                    labels = c(
                      paste('Treatment mean =', mean_trt),
                      paste('Control mean =', mean_ctl))) +
  scale_linetype_manual(values = c(1, 2),
                        labels = c(
                          paste('Treatment mean =', mean_trt),
                          paste('Control mean =', mean_ctl))
  ) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL,alpha = NULL,
       title = "Professional Runners") +
  facet_wrap(~hyperShoe, ncol = 1)

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# table for pros
plots[[length(plots) + 1]] <-
  tibble(`average time with hyperShoes` =
         c(mean_trt, '?'),
       `average time without hyperShoes` =
         c('?', mean_ctl),
       hyperShoe = c('Yes', 'No'),
       Professional = c('Yes', 'Yes')) |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(align = "center"),
      `average time without hyperShoes` = colDef(align = "center"),
      hyperShoe = colDef(align = "center"),
      Professional = colDef(align = "center")
    ))
path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# time|non-pro
mean_trt2 <- round(mean(finish[hyperShoe == 1 & pro == 0]), 2)
mean_ctl2 <- round(mean(finish[hyperShoe == 0 & pro == 0]), 2)
plots[[length(plots) + 1]] <- tibble(finish, hyperShoe, pro) |>
  mutate(pro = factor(pro, levels = c(1, 0))) |>
  filter(pro == 0)  |>
  mutate(hyperShoe = ifelse(hyperShoe == 1, 'Treatment', 'Control')) |>
  mutate(hyperShoe = factor(hyperShoe, levels = c('Treatment', 'Control'))) |>
  mutate(mean = ifelse(hyperShoe == 'Treatment', mean_trt2, mean_ctl2)) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = hyperShoe), position = 'identity') +
  geom_vline(aes(xintercept = mean, linetype = hyperShoe), col = 'black') +
  scale_fill_manual(values = c(2, 4),
                    labels = c(
                      paste('Treatment mean =', mean_trt2),
                      paste('Control mean =', mean_ctl2))) +
  scale_linetype_manual(values = c(1, 2),
                        labels = c(
                          paste('Treatment mean =', mean_trt2),
                          paste('Control mean =', mean_ctl2))
  ) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL,
       title = "Amateur runners only",
       alpha = NULL) +
  facet_wrap(~hyperShoe, ncol = 1)
plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# table add in non-pros
plots[[length(plots) + 1]] <-tibble(`average time with hyperShoes` =
           c(mean_trt, '?', mean_trt2, '?'),
         `average time without hyperShoes` =
           c('?', mean_ctl, '?', mean_ctl2),
         hyperShoe = c('Yes', 'No', 'Yes', 'No'),
         Professional = c('Yes', 'Yes', 'No', 'No')) |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(align = "center"),
      `average time without hyperShoes` = colDef(align = "center"),
      hyperShoe = colDef(align = "center"),
      Professional = colDef(align = "center")
    ))
path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# fill in pro|trt PO
tab_dat <- tibble(`average time with hyperShoes` =
         c(mean_trt, '?', mean_trt2, '?'),
       `average time without hyperShoes` =
         c(mean_ctl, mean_ctl, '?', mean_ctl2),
       hyperShoe = c('Yes', 'No', 'Yes', 'No'),
       Professional = c('Yes', 'Yes', 'No', 'No'))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center"
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes' & tab_dat$Professional[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)


# impute pro without trt PO
tab_dat <- tibble(`average time with hyperShoes` =
                    c(mean_trt, mean_trt, mean_trt2, '?'),
                  `average time without hyperShoes` =
                    c(mean_ctl, mean_ctl, '?', mean_ctl2),
                  hyperShoe = c('Yes', 'No', 'Yes', 'No'),
                  Professional = c('Yes', 'Yes', 'No', 'No'))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No' & tab_dat$Professional[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes' & tab_dat$Professional[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# impute non-pro with trt
tab_dat <- tibble(`average time with hyperShoes` =
                    c(mean_trt, mean_trt, mean_trt2, '?'),
                  `average time without hyperShoes` =
                    c(mean_ctl, mean_ctl, mean_ctl2, mean_ctl2),
                  hyperShoe = c('Yes', 'No', 'Yes', 'No'),
                  Professional = c('Yes', 'Yes', 'No', 'No'))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No'& tab_dat$Professional[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)


# impute non-pros without shoe
tab_dat <- tibble(`average time with hyperShoes` =
                    c(mean_trt, mean_trt, mean_trt2, mean_trt2),
                  `average time without hyperShoes` =
                    c(mean_ctl, mean_ctl, mean_ctl2, mean_ctl2),
                  hyperShoe = c('Yes', 'No', 'Yes', 'No'),
                  Professional = c('Yes', 'Yes', 'No', 'No'))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

## ate start
tab_dat <- tibble(`average time with hyperShoes` =
                    c(mean_trt, mean_trt, mean_trt2, mean_trt2),
                  `average time without hyperShoes` =
                    c(mean_ctl, mean_ctl, mean_ctl2, mean_ctl2),
                  hyperShoe = c('Yes', 'No', 'Yes', 'No'),
                  Pro = c('Yes', 'Yes', 'No', 'No'),
                  ATE = c('', '', '', ''))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center"),
      Pro = colDef(align = "center"),
      ATE = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)


## ate group 1
ate1 <- round(mean_trt - mean_ctl, 2)
tab_dat <- tibble(`average time with hyperShoes` =
                    c(mean_trt, mean_trt, mean_trt2, mean_trt2),
                  `average time without hyperShoes` =
                    c(mean_ctl, mean_ctl, mean_ctl2, mean_ctl2),
                  hyperShoe = c('Yes', 'No', 'Yes', 'No'),
                  Pro = c('Yes', 'Yes', 'No', 'No'),
                  ATE = c(ate1, '', '', ''))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center"),
      Pro = colDef(align = "center"),
      ATE = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# ate group 2
ate2 <- round(mean_trt - mean_ctl, 2)

tab_dat <- tibble(`average time with hyperShoes` =
                    c(mean_trt, mean_trt, mean_trt2, mean_trt2),
                  `average time without hyperShoes` =
                    c(mean_ctl, mean_ctl, mean_ctl2, mean_ctl2),
                  hyperShoe = c('Yes', 'No', 'Yes', 'No'),
                  Pro = c('Yes', 'Yes', 'No', 'No'),
                  ATE = c(ate1, ate2, '', ''))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center"),
      Pro = colDef(align = "center"),
      ATE = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# ate group 3
ate3 <- round(mean_trt2 - mean_ctl2, 2)
tab_dat <- tibble(`average time with hyperShoes` =
                    c(mean_trt, mean_trt, mean_trt2, mean_trt2),
                  `average time without hyperShoes` =
                    c(mean_ctl, mean_ctl, mean_ctl2, mean_ctl2),
                  hyperShoe = c('Yes', 'No', 'Yes', 'No'),
                  Pro = c('Yes', 'Yes', 'No', 'No'),
                  ATE = c(ate1, ate2, ate3, ''))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center"),
      Pro = colDef(align = "center"),
      ATE = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# ate group 4
ate4 <- round(mean_trt2 - mean_ctl2, 2)
tab_dat <- tibble(`average time with hyperShoes` =
                    c(mean_trt, mean_trt, mean_trt2, mean_trt2),
                  `average time without hyperShoes` =
                    c(mean_ctl, mean_ctl, mean_ctl2, mean_ctl2),
                  hyperShoe = c('Yes', 'No', 'Yes', 'No'),
                  Pro = c('Yes', 'Yes', 'No', 'No'),
                  ATE = c(ate1, ate2, ate3, ate4))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center"),
      Pro = colDef(align = "center"),
      ATE = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# repeat plot
plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    columns = list(
      `average time with hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      `average time without hyperShoes` = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$hyperShoe[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      hyperShoe = colDef(align = "center"),
      Pro = colDef(align = "center"),
      ATE = colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# show 4 groups
plots[[length(plots) + 1]] <- tibble(finish, hyperShoe, pro) |>
  #filter(pro == 0)  |>
  mutate(hyperShoe = ifelse(hyperShoe == 1, 'Treatment', 'Control')) |>
  mutate(hyperShoe = factor(hyperShoe, levels = c('Treatment', 'Control'))) |>
  mutate(pro = ifelse(pro == 1, 'Professional', 'Amateur')) |>
  mutate(pro = factor(pro, levels = c('Professional', 'Amateur'))) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = hyperShoe), position = 'identity') +
  scale_fill_manual(values = c(2, 4),
                    labels = c(
                      paste('Treatment'),
                      paste('Control'))) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL,
       title = "All four groups of runners",
       alpha = NULL) +
  facet_grid(hyperShoe~pro)

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# sample size table
tab_dat <- tibble(Y1 =
                    c(mean_trt, mean_trt, mean_trt2, mean_ctl2),
                  Y0 =
                    c(mean_ctl, mean_ctl, mean_ctl2, mean_ctl2),
                  Z = c('Yes', 'No', 'Yes', 'No'),
                  Pro = c('Yes', 'Yes', 'No', 'No'),
                  ATE = c(ate1, ate2, ate3, ate4),
                  n = c(250, 50, 112, 612))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      Pro = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

## filler (33)
plots[[length(plots) + 1]] <- ggplot() + theme_void()
plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

#icon us with shoe
grid <- tibble(hyperShoe, non_us) |>
  arrange(hyperShoe, non_us)
grid <- cbind.data.frame(grid, tidyr::crossing(x = 1:32, y = 1:32))
grid$x <- as.factor(grid$x)
grid$y <- as.factor(grid$y)


plots[[length(plots) + 1]] <- grid |>
  filter(hyperShoe == 1) |>
  ggplot(aes(x, y, shape = as.factor(non_us))) +
  geom_point(col = 2) +
  scale_shape_manual(values = c(15, 0), labels = c('Yes', 'No')) +
  theme_void() +
  theme(legend.text=element_text(size=14),
          legend.position='top',
          legend.justification='left',
          legend.direction='horizontal') +
  labs(title = 'The 300 runners that wore hyperShoes', shape = 'From the United States:')

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

#icon us with no shoe
plots[[length(plots) + 1]] <- grid |>
  filter(hyperShoe == 0) |>
  ggplot(aes(x, y, shape = as.factor(non_us))) +
  geom_point(col = 4) +
  scale_shape_manual(values = c(15, 0), labels = c('Yes', 'No')) +
  theme_void() +
  theme(legend.text=element_text(size=14),
          legend.position='top',
          legend.justification='left',
          legend.direction='horizontal') +
  labs(title = 'The 726 runners without hyperShoes', shape = 'From the United States:')

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# outcome with country
plots[[length(plots) + 1]] <- tibble(non_us, finish) |>
  mutate(non_us = ifelse(non_us == 1, 'Not from the United States', 'From the United States')) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white') +
  facet_wrap(~non_us, ncol = 1) +
  labs(x = 'Finishing time in minutes')

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

# filler
plots[[length(plots) + 1]] <- ggplot() + theme_void()
plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)


# group by pro and US
mean_trt1 <- round(mean(finish[hyperShoe == 1 & pro == 1 & non_us == 0]), 2)
mean_ctl1 <- round(mean(finish[hyperShoe == 0 & pro == 1 & non_us == 0]), 2)

plots[[length(plots) + 1]] <-
  tibble(finish, hyperShoe, pro, non_us) |>
  filter(pro == 1 & non_us == 0) |>
  group_by(hyperShoe) |>
  mutate(g_mean = mean(finish)) |>
  mutate(hyperShoe = ifelse(hyperShoe == 1, 'Treatment', 'Control')) |>
  mutate(hyperShoe = factor(hyperShoe, levels = c('Treatment', 'Control'))) |>
  mutate(pro = ifelse(pro == 1, 'Professional', 'Amateur')) |>
  mutate(non_us = ifelse(non_us == 1, 'Not from the U.S.', 'From the U.S.')) |>
  mutate(pro = factor(pro, levels = c('Professional', 'Amateur'))) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = hyperShoe), position = 'identity') +
  geom_vline(aes(xintercept = g_mean, linetype = hyperShoe), col = 'black') +
  scale_linetype_manual(values = c(1, 2),
                        labels = c(
                          paste('Treatment mean =', mean_trt1),
                          paste('Control mean = ', mean_ctl1))) +
  scale_fill_manual(values = c(2, 4),
                    labels = c(
                      paste('Treatment mean =', mean_trt1),
                      paste('Control mean = ', mean_ctl1))) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL,
       title = "Professional Runners from the United States") +
  facet_wrap(~hyperShoe, ncol = 1)

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)


tab_dat <- tibble(Y1 =
                    c(mean_trt1, '?'),
                  Y0 =
                    c('?', mean_ctl1),
                  Z = c('Yes', 'No'),
                  Pro = c('Yes', 'Yes'),
                  USA = c('Yes', 'Yes'),
                  ATE = c('', ''),
                  n = c(sum(pro == 1 & hyperShoe == 1 & non_us == 0),
                        sum(pro == 1 & hyperShoe == 0 & non_us == 0)))

plots[[length(plots)]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'white'
            t_color <- NULL
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'white'
            t_color <- NULL
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# fill in P0 for us pro
est_ate <- round(mean_trt1 - mean_ctl1, 2)
tab_dat <- tibble(Y1 =
                    c(mean_trt1, mean_trt1),
                  Y0 =
                    c(mean_ctl1, mean_ctl1),
                  Z = c('Yes', 'No'),
                  Pro = c('Yes', 'Yes'),
                  USA = c('Yes', 'Yes'),
                  ATE = c(est_ate, est_ate),
                  n = c(47, 89))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# group non-pro usa
mean_trt2 <- round(mean(finish[hyperShoe == 1 & pro == 0 & non_us == 0]), 2)
mean_ctl2 <- round(mean(finish[hyperShoe == 0 & pro == 0 & non_us == 0]), 2)

plots[[length(plots) + 1]] <-
  tibble(finish, hyperShoe, pro, non_us) |>
  filter(pro == 0 & non_us == 0) |>
  group_by(hyperShoe) |>
  mutate(g_mean = mean(finish)) |>
  mutate(hyperShoe = ifelse(hyperShoe == 1, 'Treatment', 'Control')) |>
  mutate(hyperShoe = factor(hyperShoe, levels = c('Treatment', 'Control'))) |>
  mutate(pro = ifelse(pro == 1, 'Professional', 'Amateur')) |>
  mutate(non_us = ifelse(non_us == 1, 'Not from the U.S.', 'From the U.S.')) |>
  mutate(pro = factor(pro, levels = c('Professional', 'Amateur'))) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = hyperShoe), position = 'identity') +
  geom_vline(aes(xintercept = g_mean, linetype = hyperShoe), col = 'black') +
  scale_linetype_manual(values = c(1, 2),
                        labels = c(
                          paste('Treatment mean =', mean_trt2),
                          paste('Control mean = ', mean_ctl2))) +
  scale_fill_manual(values = c(2, 4),
                    labels = c(
                      paste('Treatment mean =', mean_trt2),
                      paste('Control mean = ', mean_ctl2))) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL,
       title = "Non-professional Runners from the United States") +
  facet_wrap(~hyperShoe, ncol = 1)

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)


tab_dat <- tibble(Y1 =
                    c(mean_trt2, '?'),
                  Y0 =
                    c('?', mean_ctl2),
                  Z = c('Yes', 'No'),
                  Pro = c('No', 'No'),
                  USA = c('Yes', 'Yes'),
                  ATE = c('', ''),
                  n = c(sum(pro == 0 & hyperShoe == 1 & non_us == 0),
                        sum(pro == 0 & hyperShoe == 0 & non_us == 0)))

plots[[length(plots)]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'white'
            t_color <- NULL
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'white'
            t_color <- NULL
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# fill in P0 for us non-pro
est_ate <- round(mean_trt2 - mean_ctl2, 2)
tab_dat <- tibble(Y1 =
                    c(mean_trt2, mean_trt2),
                  Y0 =
                    c(mean_ctl2, mean_ctl2),
                  Z = c('Yes', 'No'),
                  Pro = c('No', 'No'),
                  USA = c('Yes', 'Yes'),
                  ATE = c(est_ate, est_ate),
                  n = c(sum(pro == 0 & non_us == 0 & hyperShoe == 1), sum(pro == 0 & non_us == 0 & hyperShoe == 0)))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# group pro non-usa
mean_trt3 <- round(mean(finish[hyperShoe == 1 & pro == 1 & non_us == 1]), 2)
mean_ctl3 <- round(mean(finish[hyperShoe == 0 & pro == 1 & non_us == 1]), 2)

plots[[length(plots) + 1]] <-
  tibble(finish, hyperShoe, pro, non_us) |>
  filter(pro == 1 & non_us == 1) |>
  group_by(hyperShoe) |>
  mutate(g_mean = mean(finish)) |>
  mutate(hyperShoe = ifelse(hyperShoe == 1, 'Treatment', 'Control')) |>
  mutate(hyperShoe = factor(hyperShoe, levels = c('Treatment', 'Control'))) |>
  mutate(pro = ifelse(pro == 1, 'Professional', 'Amateur')) |>
  mutate(non_us = ifelse(non_us == 1, 'Not from the U.S.', 'From the U.S.')) |>
  mutate(pro = factor(pro, levels = c('Professional', 'Amateur'))) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = hyperShoe), position = 'identity') +
  geom_vline(aes(xintercept = g_mean, linetype = hyperShoe), col = 'black') +
  scale_linetype_manual(values = c(1, 2),
                        labels = c(
                          paste('Treatment mean =', mean_trt3),
                          paste('Control mean = ', mean_ctl3))) +
  scale_fill_manual(values = c(2, 4),
                    labels = c(
                      paste('Treatment mean =', mean_trt3),
                      paste('Control mean = ', mean_ctl3))) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL,
       title = "Professional Runners not from the United States") +
  facet_wrap(~hyperShoe, ncol = 1)

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)


tab_dat <- tibble(Y1 =
                    c(mean_trt3, '?'),
                  Y0 =
                    c('?', mean_ctl3),
                  Z = c('Yes', 'No'),
                  Pro = c('Yes', 'Yes'),
                  USA = c('No', 'No'),
                  ATE = c('', ''),
                  n = c(sum(pro == 1 & hyperShoe == 1 & non_us == 1),
                        sum(pro == 1 & hyperShoe == 0 & non_us == 1)))

plots[[length(plots)]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'white'
            t_color <- NULL
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'white'
            t_color <- NULL
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# fill in P0 for non-us pro
est_ate <- round(mean_trt3 - mean_ctl3, 2)
tab_dat <- tibble(Y1 =
                    c(mean_trt3, mean_trt3),
                  Y0 =
                    c(mean_ctl3, mean_ctl3),
                  Z = c('Yes', 'No'),
                  Pro = c('Yes', 'Yes'),
                  USA = c('No', 'No'),
                  ATE = c(est_ate, est_ate),
                  n = c(sum(pro == 1 & non_us == 1 & hyperShoe == 1), sum(pro == 1 & non_us == 1 & hyperShoe == 0)))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# group non-pro non-usa
mean_trt4 <- round(mean(finish[hyperShoe == 1 & pro == 0 & non_us == 1]), 2)
mean_ctl4 <- round(mean(finish[hyperShoe == 0 & pro == 0 & non_us == 1]), 2)

plots[[length(plots) + 1]] <-
  tibble(finish, hyperShoe, pro, non_us) |>
  filter(pro == 0 & non_us == 1) |>
  group_by(hyperShoe) |>
  mutate(g_mean = mean(finish)) |>
  mutate(hyperShoe = ifelse(hyperShoe == 1, 'Treatment', 'Control')) |>
  mutate(hyperShoe = factor(hyperShoe, levels = c('Treatment', 'Control'))) |>
  mutate(pro = ifelse(pro == 1, 'Professional', 'Amateur')) |>
  mutate(non_us = ifelse(non_us == 1, 'Not from the U.S.', 'From the U.S.')) |>
  mutate(pro = factor(pro, levels = c('Professional', 'Amateur'))) |>
  ggplot(aes(finish)) +
  geom_histogram(col = 'white', aes(fill = hyperShoe), position = 'identity') +
  geom_vline(aes(xintercept = g_mean, linetype = hyperShoe), col = 'black') +
  scale_linetype_manual(values = c(1, 2),
                        labels = c(
                          paste('Treatment mean =', mean_trt4),
                          paste('Control mean = ', mean_ctl4))) +
  scale_fill_manual(values = c(2, 4),
                    labels = c(
                      paste('Treatment mean =', mean_trt4),
                      paste('Control mean = ', mean_ctl4))) +
  labs(x = 'Finishing time in minutes',fill = NULL, linetype = NULL,
       title = "Non-professional Runners not from the United States") +
  facet_wrap(~hyperShoe, ncol = 1)

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)


tab_dat <- tibble(Y1 =
                    c(mean_trt4, '?'),
                  Y0 =
                    c('?', mean_ctl4),
                  Z = c('Yes', 'No'),
                  Pro = c('No', 'No'),
                  USA = c('No', 'No'),
                  ATE = c('', ''),
                  n = c(sum(pro == 0 & hyperShoe == 1 & non_us == 1),
                        sum(pro == 0 & hyperShoe == 0 & non_us == 1)))

plots[[length(plots)]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'white'
            t_color <- NULL
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'white'
            t_color <- NULL
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# fill in P0 for non-us pro
est_ate <- round(mean_trt4 - mean_ctl4, 2)
tab_dat <- tibble(Y1 =
                    c(mean_trt4, mean_trt4),
                  Y0 =
                    c(mean_ctl4, mean_ctl4),
                  Z = c('Yes', 'No'),
                  Pro = c('No', 'No'),
                  USA = c('No', 'No'),
                  ATE = c(est_ate, est_ate),
                  n = c(sum(pro == 0 & non_us == 1 & hyperShoe == 1), sum(pro == 0 & non_us == 1 & hyperShoe == 0)))

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Y1 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'No') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Y0 = colDef(
        align = "center",
        style = function(value, index) {
          if (tab_dat$Z[index] == 'Yes') {
            b_color <- 'black'
            t_color <- 'white'
          } else {
            b_color <- NULL
            t_color <- NULL

          }
          list(background = b_color, color = t_color)
        }
      ),
      Z = colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# table for weighted average
tab_dat <- tibble(Z = rep(c('Yes', 'No'), 4),
                  Pro = c('Yes', 'Yes', 'No', 'No', 'Yes', 'Yes', "No", 'No'),
                  USA = c('Yes', 'Yes', 'Yes', 'Yes', 'No', 'No', 'No', 'No'),
                  ATE = c(rep(round(mean_trt1 - mean_ctl1, 2), 2), rep(round(mean_trt2 - mean_ctl2, 2), 2), rep(round(mean_trt3 - mean_ctl3, 2),2), rep(round(mean_trt4 - mean_ctl4, 2), 2)),
                  n = c(
                    sum(pro == 1 & non_us == 0 & hyperShoe == 1),
                    sum(pro == 1 & non_us == 0 & hyperShoe == 0),
                    sum(pro == 0 & non_us == 0 & hyperShoe == 1),
                    sum(pro == 0 & non_us == 0 & hyperShoe == 0),
                    sum(pro == 1 & non_us == 1 & hyperShoe == 1),
                    sum(pro == 1 & non_us == 1 & hyperShoe == 0),
                    sum(pro == 0 & non_us == 1 & hyperShoe == 1),
                    sum(pro == 0 & non_us == 1 & hyperShoe == 0)
                    )
                  )

plots[[length(plots) + 1]] <- tab_dat |>
  reactable::reactable(
    fullWidth = F,
    defaultColDef = colDef(minWidth = 75),
    columns = list(
      Pro = colDef(align = "center"),
      USA =  colDef(align = "center"),
      ATE = colDef(align = "center"),
      n =  colDef(align = "center")
    )
  )

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# filler
plots[[length(plots) + 1]] <-
  ggplot() +
  theme_void()

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)

plots[[length(plots) + 1]] <-
  ggplot() +
  theme_void()

plots[[length(plots)]]
path <- paste0('p', length(plots), '.png')
ggsave(path)


tab <- tibble(`adjusted for` = c('no confounders',
                  'professional status',
                  'professional status, usa'),
       `estimated ATE` = c(-43.07, 2.91,-11.35),
       icon = c(
         as.character(shiny::icon("remove", style = "color:red")),
         as.character(shiny::icon("remove", style = "color:red")),
         as.character(shiny::icon("question", style = "color:grey"))
         )
       )

plots[[length(plots) + 1]] <- reactable::reactable(
    data = tab[1,],
    columns = list(
      icon = colDef(
        html = T,
        show = T,
        name = ''
      )
    )
  ) |>
    htmltools::tagList(fontawesome::fa_html_dependency()) |>
    htmltools::browsable()

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

plots[[length(plots) + 1]] <- reactable::reactable(
  data = tab[1:2,],
  columns = list(
    icon = colDef(
      html = T,
      show = T,
      name = ''
    )
  )
) |>
  htmltools::tagList(fontawesome::fa_html_dependency()) |>
  htmltools::browsable()

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

plots[[length(plots) + 1]] <- reactable::reactable(
  data = tab[1:3,],
  columns = list(
    icon = colDef(
      html = T,
      show = T,
      name = ''
    )
  )
) |>
  htmltools::tagList(fontawesome::fa_html_dependency()) |>
  htmltools::browsable()

path <- paste0('p', length(plots), '.rds')
readr::write_rds(plots[[length(plots)]], file = path)

# reset path
setwd('../../../../../..')
