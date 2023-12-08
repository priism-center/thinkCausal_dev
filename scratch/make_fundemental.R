#library(tidyverse)
library(ggrepel)
library(ggdark)
library(ggplot2)
library(tidyr)
library(dplyr)

set.seed(62)
n <- 20
prior_race <- sample(c(0, 1, 2, 3), n, replace = T)

dat <- data.frame(prior_race)

for (i in 0:max(prior_race)) {
  dat[,i + 2] <-  ifelse(dat$prior_race == i, 1, 0)
  names(dat)[i + 2] <- paste0('prior_race', i)
}


dat <- dat[, 2:length(dat)]
Y1 <- as.matrix(cbind(rep(1, nrow(dat)), dat)) %*% c(230, 40, 40, 20, -5)
Y0 <- as.matrix(cbind(rep(1, nrow(dat)), dat)) %*% c(235, 45, 45, 25, 15)

Y1 <- round(Y1 + rnorm(n, 0, 2.5), 0)
Y0 <- round(Y0 + rnorm(n, 0, 2.5), 0)
Y1[Y1 == 223] <- 224
Y0[17] <- 279

Z <- rbinom(n, 1, .5)
Y <- ifelse(Z == 1, Y1, Y0)
fit <- lm(Y ~ Z*as.factor(prior_race))
imputed <- predict(fit)

Zcf <- abs(Z - 1)
cf_dat <- data.frame(Z = Zcf, prior_race)
imputed_cf <- predict(fit, newdata = cf_dat)
ite_true <- c(Y1 - Y0)
ite_est <- ifelse(Z == 1, c(Y1), imputed_cf) - ifelse(Z == 0, c(Y0), imputed_cf)

tibble(ite_true, ite_est, runner = 1:20) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(as.factor(runner), value, col = name, shape = name, size = name)) +
  scale_shape_manual(values = c(19, 21), guide = 'none') +
  scale_size_manual(values = c(1, 1.5), guide = 'none') +
  scale_color_manual(values = c('purple', 'white'),
                     labels = c('estimated ITE', 'true ITE'),
                     guide = guide_legend(
                       override.aes = list(shape = c(19, 21))
                     )
  ) +
  geom_point() +
  coord_cartesian(ylim = c(-35, -5)) +
  labs(color = NULL, x = 'runner', y = 'Individual Treatment Effect (ITE)') +
  dark_theme_grey()


ggsave('inst/app/www/learn/fundemental/plots/cf1.png', device = 'png', height = 5, width = 8)



tibble(ite_true, ite_est, runner = 1:20) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(as.factor(runner), value, col = name, shape = name, size = name)) +
  scale_shape_manual(values = c(19, 21), guide = 'none') +
  scale_size_manual(values = c(1, 1.5), guide = 'none') +
  geom_hline(aes(yintercept = mean(ite_true), linetype = 'true ATE = -12.75'), col = 'white') +
  geom_hline(aes(yintercept = mean(ite_est), linetype = 'estimated ATE = -12.14'), col = 'purple') +
  scale_color_manual(values = c('purple', 'white'),
                     labels = c('estimated ITE', 'true ITE'),
                     guide = guide_legend(
                       override.aes = list(shape = c(19, 21))
                     )
                     ) +
  geom_point() +
  scale_linetype_manual(values = c(1, 2),
                        guide = guide_legend(
                          override.aes = list(
                            linetype = c(1, 2),
                            color = c('purple', 'white')
                          ))
  )+
  coord_cartesian(ylim = c(-35, -5)) +
  labs(linetype = NULL, col = NULL, x = 'runner', y = 'Individual Treatment Effect (ITE)') +
  dark_theme_grey()
ggsave('inst/app/www/learn/fundemental/plots/cf2.png', device = 'png', height = 5, width = 8)



tibble(ite_true, ite_est, runner = 1:20) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(value, fill = name)) +
  geom_histogram(bins = 12, position = 'identity', col = 'black', alpha = .6) +
  geom_vline(aes(xintercept = mean(ite_true), col = 'true ATE = -12.75')) +
  geom_vline(aes(xintercept = mean(ite_est), col = 'estimated ATE = -12.14')) +
  scale_color_manual(values = c('purple', 'white')) +
  scale_fill_manual(values = c('purple', 'white'), labels = c('estimated ITE', 'True')) +
  dark_theme_gray()





data.frame(Y, prior_race, Z) %>%
  ggplot(aes(as.factor(prior_race), Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('no hyperShoe', 'hyperShoe')) +
  labs(x = 'number of prior races',
       y = 'observed running time (Y)',
       color = NULL,
       title = 'Observed Data') +
  theme_bw()


ggsave('inst/app/www/learn/fundemental/plots/p1.png', device = 'png', height = 5, width = 8)


data.frame(Y, prior_race, Z) %>%
  filter(prior_race == 0) %>%
  ggplot(aes(as.factor(prior_race), Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('no hyperShoe', 'hyperShoe')) +
  scale_x_discrete(limits = c('0', '1', '2', '3')) +
  labs(x = 'number of prior races',
       y = 'observed running time (Y)',
       color = NULL,
       title = 'Observed Data') +
  theme_bw()


ggsave('inst/app/www/learn/fundemental/plots/p2.png', device = 'png', height = 5, width = 8)


data.frame(Y, prior_race, Z,Y0) %>%
  filter(prior_race == 0) %>%
  ggplot(aes(as.factor(prior_race), Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('no hyperShoe', 'hyperShoe')) +
  geom_segment(x = .5, xend = 1.5, y = round(imputed[Z == 0 & prior_race == 0][1],0), yend = round(imputed[Z == 0 & prior_race == 0][1],0), aes(linetype = 'Average Y0 = 281'), col = 4) +
  scale_x_discrete(limits = c('0', '1', '2', '3')) +
  labs(x = 'number of prior races',
       y = 'observed running time (Y)',
       color = NULL,
       title = 'Using Information from Observed Y0s',
       subtitle = 'average of observed Y0 for runners with 0 prior races = 281',
       linetype = NULL
       ) +
  theme_bw()
ggsave('inst/app/www/learn/fundemental/plots/p3.png', device = 'png', height = 5, width = 8)


data.frame(Y, prior_race, Z,Y1) %>%
  filter(prior_race == 0) %>%
  ggplot(aes(as.factor(prior_race), Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('no hyperShoe', 'hyperShoe')) +
  geom_segment(aes(linetype = 'Average Y1 = 270'),
               x = .5, xend = 1.5,
               y = round(imputed[Z == 1 & prior_race == 0][1],0), yend = round(imputed[Z == 1 & prior_race == 0][1],0),
               col = 2) +
  scale_x_discrete(limits = c('0', '1', '2', '3')) +
  labs(x = 'number of prior races',
       y = 'observed running time (Y)',
       color = NULL,
       title = 'Using Information from Observed Y1s',
       subtitle = 'average of observed Y1 for runners with 0 prior races = 270',
       linetype = NULL
  ) +
  theme_bw()
ggsave('inst/app/www/learn/fundemental/plots/p4.png', device = 'png', height = 5, width = 8)


data.frame(Y, prior_race, Z,Y1, Y0) %>%
  filter(prior_race == 1) %>%
  ggplot(aes(as.factor(prior_race), Y, col = as.factor(Z))) +
  geom_point() +
  geom_segment(aes(linetype = 'Average Y1'),
               x = 1.5, xend = 2.5,
               y = round(imputed[Z == 1 & prior_race == 1][1],0), yend = round(imputed[Z == 1 & prior_race == 1][1],0),
               col = 2) +
  geom_segment(aes(linetype = 'Average Y0'),
               x = 1.5, xend = 2.5,
               y = round(imputed[Z == 0 & prior_race == 1][1],0), yend = round(imputed[Z == 0 & prior_race == 1][1],0),
               col = 4) +
  scale_linetype_manual(values = c(1, 1),
                        labels = c(paste(round(imputed[Z == 0 & prior_race == 1][1],1)), paste(round(imputed[Z == 1 & prior_race == 1][1],1))),
                        guide = guide_legend(
                          override.aes = list(
                            linetype = c(1, 1),
                            color = c(4, 2)
                          ))
                        ) +
  scale_color_manual(values = c(4, 2), labels = c('no hyperShoe', 'hyperShoe')) +
  scale_x_discrete(limits = c('0', '1', '2', '3')) +
  labs(x = 'number of prior races',
       y = 'observed running time (Y)',
       color = NULL,
       title = 'Runners with 1 prior race',
       subtitle = 'average of observed Y0 = 280\naverage of observed Y1 = 273',
       linetype = NULL
  ) +
  theme_bw()

ggsave('inst/app/www/learn/fundemental/plots/p5.png', device = 'png', height = 5, width = 8)


p6 <- data.frame(Y, prior_race, Z,Y1, Y0) %>%
  filter(prior_race == 2) %>%
  ggplot(aes(as.factor(prior_race), Y, col = as.factor(Z))) +
  geom_point() +
  geom_segment(aes(linetype = 'Average Y1'),
               x = 2.5, xend = 3.5,
               y = round(imputed[Z == 1 & prior_race == 2][1],0), yend = round(imputed[Z == 1 & prior_race == 2][1],0),
               col = 2) +
  geom_segment(aes(linetype = 'Average Y0'),
               x = 2.5, xend = 3.5,
               y = round(imputed[Z == 0 & prior_race == 2][1],0), yend = round(imputed[Z == 0 & prior_race == 2][1],0),
               col = 4) +
  scale_linetype_manual(values = c(1, 1),
                        labels = c(paste(round(imputed[Z == 0 & prior_race == 2][1],0)),
                                   paste(round(imputed[Z == 1 & prior_race == 2][1],0))),
                        guide = guide_legend(
                          override.aes = list(
                            linetype = c(1, 1),
                            color = c(4, 2)
                          ))
  ) +
  scale_color_manual(values = c(4, 2), labels = c('no hyperShoe', 'hyperShoe')) +
  scale_x_discrete(limits = c('0', '1', '2', '3')) +
  labs(x = 'number of prior races',
       y = 'observed running time (Y)',
       color = NULL,
       title = 'Runners with 2 prior race',
       subtitle = paste(paste('Average observed Y0 =', round(imputed[Z == 0 & prior_race == 2][1],0)),
       paste('Average observed Y1 =',round(imputed[Z == 1 & prior_race == 2][1],0)), sep = '\n'),
       linetype = NULL
  ) +
  theme_bw()

p6
readr::write_rds(p6, 'inst/app/www/learn/fundemental/plots/p6.rds')

ggsave('inst/app/www/learn/fundemental/plots/p6.png', device = 'png', height = 5, width = 8)



p7 <- data.frame(Y, prior_race, Z,Y1, Y0) %>%
  filter(prior_race == 3) %>%
  ggplot(aes(as.factor(prior_race), Y, col = as.factor(Z))) +
  geom_point() +
  geom_segment(aes(linetype = 'Average Y1'),
               x = 3.5, xend = 4.5,
               y = round(imputed[Z == 1 & prior_race == 3][1],0), yend = round(imputed[Z == 1 & prior_race == 3][1],0),
               col = 2) +
  geom_segment(aes(linetype = 'Average Y0'),
               x = 3.5, xend = 4.5,
               y = round(imputed[Z == 0 & prior_race == 3][1],0), yend = round(imputed[Z == 0 & prior_race == 3][1],0),
               col = 4) +
  scale_linetype_manual(values = c(1, 1),
                        labels = c(paste(round(imputed[Z == 0 & prior_race == 3][1],0)),
                                   paste(round(imputed[Z == 1 & prior_race == 3][1],0))),
                        guide = guide_legend(
                          override.aes = list(
                            linetype = c(1, 1),
                            color = c(4, 2)
                          ))
  ) +
  scale_color_manual(values = c(4, 2), labels = c('no hyperShoe', 'hyperShoe')) +
  scale_x_discrete(limits = c('0', '1', '2', '3')) +
  labs(x = 'number of prior races',
       y = 'observed running time (Y)',
       color = NULL,
       title = 'Runners with 3 prior race',
       subtitle = paste(paste('Average observed Y0 =', round(imputed[Z == 0 & prior_race == 3][1],0)),
                        paste('Average observed Y1 =',round(imputed[Z == 1 & prior_race == 3][1],0)), sep = '\n'),
       linetype = NULL
  ) +
  theme_bw()
p7
readr::write_rds(p7, 'inst/app/www/learn/fundemental/plots/p7.rds')

ggsave('inst/app/www/learn/fundemental/plots/p7.png', device = 'png', height = 5, width = 8)

truth <- tibble(runner = 1:20, prior_race, Z, Y0, Y1, Y) %>%
  rename(hyperShoe = Z,
         `prior races` = prior_race) %>%
  map_df(., function(x){round(x, 0)})

truth <- apply(truth, 2, as.vector)
truth <- as.data.frame(truth)

write_csv(truth, '~/Dropbox/thinkCausal_dev/thinkCausal/inst/extdata/truth.csv')


table1 <- tibble(runner = 1:20, prior_race, Z, Y0, Y1, Y) %>%
  rename(hyperShoe = Z,
         `prior races` = prior_race) %>%
  map_df(., function(x){round(x, 0)}) %>%
  mutate(Y0 = ifelse(hyperShoe == 1, NA, Y0),
         Y1 = ifelse(hyperShoe == 0, NA, Y1))

write_csv(table1, '~/Dropbox/thinkCausal_dev/thinkCausal/inst/extdata/fundamental_table1.csv')

table2 <- table1
table2$Y0[is.na(table2$Y0)] <- round(predict(fit, newdata = cf_dat), 0)[is.na(table2$Y0)]
table2$Y1[is.na(table2$Y1)] <- round(predict(fit, newdata = cf_dat), 0)[is.na(table2$Y1)]
readr::write_csv(table2, '~/Dropbox/thinkCausal_dev/thinkCausal/inst/extdata/fundamental_table2.csv')


table2 %>%
  mutate(ITE = Y1 - Y0) %>%
  summarise(mean(ITE
                 ))




# set.seed(21)
# Y1 <- floor(250 - 5 + rnorm(10, 0, 2))
# Y0 <- floor(250 + rnorm(10, 0, 2))
#
# dat.nf <- data.frame(runner = 1:10, `first race` = 'no', Y0, Y1)
#
# Y1 <- floor(265 - 5 + rnorm(20, 0, 2))
# Y0 <- floor(265 + rnorm(20, 0, 2))
#
#
# dat.f<- data.frame(runner = 11:30, `first race` = 'yes', Y0, Y1)
#
# dat <- rbind(dat.nf, dat.f)
# dat$Z <- rbinom(30, 1, .5)
# dat$Y <- ifelse(dat$Z == 1, dat$Y1, dat$Y0)
#
# dat <- dat[sample(1:nrow(dat), nrow(dat)), ]
# dat$runner <- 1:nrow(dat)
# rownames(dat) <- 1:nrow(dat)
# hold <- dat[2,]
# dat[2,] <- dat[23,]
# dat[23,] <- hold
# rownames(dat) <- 1:nrow(dat)
# dat$runner <- 1:nrow(dat)
# table1 <- dat %>% select(-first.race) %>% select(runner, Z, Y0, Y1, Y) %>% rename(`hyperShoe` = Z)
#
# write_csv(table1, '~/Dropbox/thinkCausal_dev/thinkCausal/inst/extdata/fundamental_table1.csv')
#
#
# write_csv(table1[1,], '~/Dropbox/thinkCausal_dev/thinkCausal/inst/extdata/fundamental_table2.csv')
#
#
# dat[1, ] %>%
#   pivot_longer(cols = c(Y0, Y1)) %>%
#   mutate(cf = ifelse(Z != str_sub(name, -1), T, F)) %>%
#   filter(name == 'Y1') %>%
#   ggplot(aes(name, value, col = name, label = runner)) +
#   geom_point(aes(shape = cf)) +
#   scale_shape_manual(values = c(19, 21), labels = c('observed', 'counterfactual')) +
#   scale_x_discrete(limits = c('Y0', 'Y1')) +
#   coord_cartesian(ylim = c(240, 270)) +
#   geom_text_repel(show.legend = FALSE, seed = 4) +
#   scale_color_manual(values = c(2)) +
#   theme_bw() +
#   labs(x = 'Potential Outcomes', col = NULL, y = 'Running Time', title = 'Runner 1', subtitle = 'observed outcome', shape = NULL)
#
# ggsave('inst/app/www/learn/fundemental/plots/p1.png', device = 'png', height = 5, width = 8)
#
#
#
#
# dat[1, ] %>%
#   pivot_longer(cols = c(Y0, Y1)) %>%
#   mutate(cf = ifelse(Z != str_sub(name, -1), T, F)) %>%
#   ggplot(aes(name, value, col = name, label = runner)) +
#   geom_point(aes(shape = cf)) +
#   scale_shape_manual(values = c(19, 21), labels = c('observed', 'counterfactual')) +
#   geom_text_repel(show.legend = FALSE, seed = 4) +
#   scale_color_manual(values = c(4, 2)) +
#   coord_cartesian(ylim = c(240, 270)) +
#   theme_bw() +
#   labs(x = 'Potential Outcomes', col = NULL, y = 'Running Time', title = 'Runner 1', subtitle = 'observed and counter-factual outcomes', shape = NULL)
#
# ggsave('inst/app/www/learn/fundemental/plots/p2.png', device = 'png', height = 5, width = 8)
#
#
#
# dat[1:2, ] %>%
#   pivot_longer(cols = c(Y0, Y1)) %>%
#   slice(c(1,2,3)) %>%
#   mutate(cf = ifelse(Z != str_sub(name, -1), T, F)) %>%
#   ggplot(aes(name, value, col = name, label = runner)) +
#   geom_point(aes(shape = cf)) +
#   scale_shape_manual(values = c(19, 21), labels = c('observed', 'counterfactual')) +
#   geom_text_repel(show.legend = FALSE, seed = 4) +
#   scale_color_manual(values = c(4, 2)) +
#   coord_cartesian(ylim = c(240, 270)) +
#   theme_bw() +
#   labs(x = 'Potential Outcomes', col = NULL, y = 'Running Time', title = 'Runners 1 & 2',subtitle = "consider runner 2's observed outcome", shape = NULL)
#
# ggsave('inst/app/www/learn/fundemental/plots/p3.png', device = 'png', height = 5, width = 8)
#
#
# dat[1:2, ] %>%
#   pivot_longer(cols = c(Y0, Y1)) %>%
#   mutate(cf = ifelse(Z != str_sub(name, -1), T, F)) %>%
#   ggplot(aes(name, value, col = name, label = runner)) +
#   geom_point(aes(shape = cf)) +
#   scale_shape_manual(values = c(19, 21), labels = c('observed', 'counterfactual')) +
#   geom_text_repel(show.legend = FALSE, seed = 4) +
#   scale_color_manual(values = c(4, 2)) +
#   coord_cartesian(ylim = c(240, 270)) +
#   theme_bw() +
#   labs(x = 'Potential Outcomes', col = NULL, y = 'Running Time', title = 'Runners 1 & 2',subtitle = 'observed and counter-factual outcomes', shape = NULL)
#
# ggsave('inst/app/www/learn/fundemental/plots/p4.png', device = 'png', height = 5, width = 8)
#
#
# pj <- position_jitter(width = .25, height = .45, seed = 23)
#
# dat %>%
#   pivot_longer(cols = c(Y0, Y1)) %>%
#   mutate(cf = ifelse(Z != str_sub(name, -1), T, F)) %>%
#   ggplot(aes(name, value, col = name, label = runner)) +
#   geom_point(aes(shape = cf), position = pj) +
#   scale_shape_manual(values = c(21, 19), labels = c('counterfactual', 'observed')) +
#   scale_color_manual(values = c(4, 2)) +
#   coord_cartesian(ylim = c(240, 270)) +
#   theme_bw() +
#   labs(x = 'Potential Outcomes', col = NULL, y = 'Running Time', title = "All Runners 'Jittered'", subtitle = 'observed and counterfactual outcomes', shape = NULL)
#
#
# ggsave('inst/app/www/learn/fundemental/plots/p5.png', device = 'png', height = 5, width = 8)
#
# Y1 <- dat$Y1
# Y0 <- dat$Y0
# dat %>%
#   pivot_longer(cols = c(Y0, Y1)) %>%
#   mutate(cf = ifelse(Z != str_sub(name, -1), T, F)) %>%
#   ggplot(aes(name, value, col = name, label = runner)) +
#   geom_point(aes(shape = cf), position = pj) +
#   geom_segment(aes( x = 1.7, xend = 2.3, y = round(mean(Y1), 0), yend = round(mean(Y1), 0)), col = 1, linetype = 2) +
#   geom_segment(aes( x = 0.7, xend = 1.3, y = mean(Y0), yend = mean(Y0)), col = 1, linetype = 2) +
#   scale_color_manual(values = c(4, 2)) +
#   scale_shape_manual(values = c(21, 19), labels = c('counterfactual Y', 'observed Y')) +
#   theme_bw() +
#   coord_cartesian(ylim = c(240, 270)) +
#   annotate(
#     geom = "curve", x = .65, y = 257, xend = .68, yend = 260,
#     curvature = -.8, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 1, y = 257, label = "average Y0 = 260", hjust = "center", size = 3.5) +
#   annotate(
#     geom = "curve", x = 2.35, y = 252, xend = 2.32, yend = 255,
#     curvature = .8, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 2, y = 252, label = "average Y1 = 255", hjust = "center", size = 3.5) +
#
#   labs(shape = NULL, color = NULL) +
#   labs(x = 'Potential Outcomes', col = NULL, y = 'Running Time', title = "True ATE = -5", subtitle = 'ATE = the average Y1 - the average Y0', shape = NULL)
#
#
# ggsave('inst/app/www/learn/fundemental/plots/p6.png', device = 'png', height = 5, width = 8)
#
#
#
#
# dat %>%
#   ggplot(aes(as.factor(Z), Y, col = as.factor(Z))) +
#   geom_point(position = pj) +
#   scale_color_manual(values = c(4, 2), labels = c('No hyperShoe', 'hyperShoe')) +
#   scale_x_discrete(labels = c('No hyperShoe (observed Y0)', 'hyperShoe (observed Y1)')) +
#   theme_bw() +
#   coord_cartesian(ylim = c(240, 270)) +
#   labs(x = NULL, y = 'Running time', col = NULL, title = 'Observed outcomes only')
#
# ggsave('inst/app/www/learn/fundemental/plots/p7.png', device = 'png', height = 5, width = 8)
#
#
# dat2 <- table1
# dat2$Y1 <- as.character(dat2$Y1)
# dat2$Y0 <- as.character(dat2$Y0)
# dat2 <- dat2 %>% mutate(Y1 = ifelse(hyperShoe == 1, Y1, NA),
#                         Y0 = ifelse(hyperShoe == 0, Y0, NA))
#
#
# write_csv(dat2,  '~/Dropbox/thinkCausal_dev/thinkCausal/inst/extdata/fundemental_table3.csv')
#
#
# dat %>%
#   ggplot(aes(as.factor(Z), Y, col = as.factor(Z))) +
#   geom_point(position = pj) +
#   geom_segment(aes( x = 1.7, xend = 2.3, y = round(mean(Y[Z == 1]), 0), yend = round(mean(Y[Z == 1]), 0)), col = 1, linetype = 1) +
#   geom_segment(aes( x = 0.7, xend = 1.3, y = round(mean(Y[Z == 0]), 0), yend = round(mean(Y[Z == 0]), 0)), col = 1, linetype = 1) +
#   scale_color_manual(values = c(4, 2), labels = c('No hyperShoe', 'hyperShoe')) +
#   scale_x_discrete(labels = c('No hyperShoe (Observed Y0)', 'hyperShoe (Observed Y1)')) +
#   theme_bw() +
#   labs(x = NULL, y = 'Running time', col = NULL, title = 'Averages of Observed Y0 and Observed Y1') +
#   annotate(
#     geom = "curve", x = .65, y = 260, xend = .68, yend = 258,
#     curvature = .8, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 1, y = 260.5, label = "average observed Y0 = 258", hjust = "center", size = 3.5) +
#   annotate(
#     geom = "curve", x = 2.35, y = 254, xend = 2.32, yend = 256,
#     curvature = .8, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 2, y = 253.5, label = "average observed Y1 = 256", hjust = "center", size = 3.5) +
#   coord_cartesian(ylim = c(240, 270))
#
#
# ggsave('inst/app/www/learn/fundemental/plots/p8.png', device = 'png', height = 5, width = 8)
#
#
# dat3 <- dat
# dat3 <- dat3 %>% mutate(Y1 = ifelse(Z == 1, Y1, 256),
#                         Y0 = ifelse(Z == 0, Y0, 258))
# dat3 <- dat3 %>% mutate(ITE = Y1 - Y0)
# dat3 <- dat3 %>% select(runner, first.race, Z, Y0, Y1, Y, ITE)
#
#
# write_csv(dat3 %>% select(1:6) %>% rename(hyperShoe = Z),  '~/Dropbox/thinkCausal_dev/thinkCausal/inst/extdata/fundemental_table4.csv')
#
#
#
# dat3$ITE_true <- with(dat, Y1 - Y0)
#
# dat3 %>%
#   ggplot(aes(runner, ITE, label = runner)) +
#   geom_point() +
#   geom_text_repel(show.legend = FALSE, seed = 4, size = 3) +
#   annotate(
#     geom = "curve", x = 2, y = 8.5, xend = 2, yend = 10.5,
#     curvature = 0, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 6, y = 8, label = "predicted treatment effect of 11", hjust = "center", size = 3) +
#   annotate(
#     geom = "curve", x = 4, y = -10.5, xend = 4, yend = -12.5,
#     curvature = 0, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 8, y = -10, label = "predicted treatment effect of -13", hjust = "center", size = 3) +
#   theme_bw() +
#   theme(legend.position = 'top', legend.justification='left') +
#   labs(x = 'Runner ID', y = 'Predicted Causal Effect of hyperShoes', linetype = NULL, title = 'Estimated Causal Effects')
# ggsave('inst/app/www/learn/fundemental/plots/p9.png', device = 'png', height = 5, width = 8)
#
#
# dat3 %>%
#   pivot_longer(contains('ITE')) %>%
#   ggplot(aes(runner, value, col = name, shape = name)) +
#   scale_color_manual(values = c(1, 6), labels = c('Predicted Causal Effect', 'True Causal Effect')) +
#   scale_shape_manual(values = c(19, 21), labels = c('Predicted Causal Effect', 'True Causal Effect')) +
#   geom_line(aes(group = runner,linetype = 'bias'), col = 'dark grey') +
#   scale_linetype_manual(values = 2) +
#   geom_point() +
#   #geom_hline(aes(yintercept = mean(value), col = name)) +
#   #geom_hline(yintercept = mean(dat3$ITE)) +
#   #geom_hline(yintercept = mean(dat3$ITE_true), col = 6, linetype = 2) +
#   theme_bw() +
#   theme(legend.position = 'top', legend.justification = 'left') +
#   labs(col = NULL, shape = NULL, linetype = NULL, x = 'Runner ID')
# ggsave('inst/app/www/learn/fundemental/plots/p10.png', device = 'png', height = 5, width = 8)
#
#
# dat3 %>%
#   pivot_longer(contains('ITE')) %>%
#   ggplot(aes(value, fill = name)) +
#   geom_histogram(col = 'black') +
#   facet_wrap(~name, ncol = 1) +
#   theme_bw()
#
#
#
# dat %>%
#   ggplot(aes(as.factor(Z), Y, col = as.factor(Z), shape = first.race)) +
#   geom_point(position = pj) +
#   scale_color_manual(values = c(4, 2), labels = c('No hyperShoe', 'hyperShoe')) +
#   scale_shape_manual(values = c(4, 19)) +
#   scale_x_discrete(labels = c('No hyperShoe (Observed Y0)', 'hyperShoe (Observed Y1)')) +
#   theme_bw() +
#   coord_cartesian(ylim = c(240, 270)) +
#   labs(x = NULL, y = 'Running time', col = NULL, title = 'Averages of Observed Y1 and Observed Y0')
#
# ggsave('inst/app/www/learn/fundemental/plots/p11.png', device = 'png', height = 5, width = 8)
#
#
#
# dat %>%
#   group_by(first.race, Z) %>%
#   summarise(mean(Y))
#
# dat %>%
#   ggplot(aes(as.factor(Z), Y, col = as.factor(Z), shape = first.race)) +
#   geom_point(position = pj) +
#   geom_segment(aes( x = 1.7, xend = 2.3, y = round(mean(Y[Z == 1& first.race == 'yes']), 0), yend = round(mean(Y[Z == 1 & first.race == 'yes']), 0)), col = 1, linetype = 1) +
#   geom_segment(aes( x = 0.7, xend = 1.3, y = round(mean(Y[Z == 0 & first.race == 'yes']), 0), yend = round(mean(Y[Z == 0 & first.race == 'yes']), 0)), col = 1, linetype = 1) +
#   annotate(
#     geom = "curve", x = .65, y = 269.5, xend = .65, yend = 265,
#     curvature = .8, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 1, y = 270, label = "average observed Y0 = 265", hjust = "center", size = 3) +
#   annotate(
#     geom = "curve", x = 2.35, y = 263.5, xend = 2.35, yend = 260,
#     curvature = -.8, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 2, y = 264, label = "average observed Y1 = 260", hjust = "center", size = 3) +
#   annotate(
#     geom = "curve", x = .65, y = 248, xend = .65, yend = 250,
#     curvature = -.8, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 1, y = 247.5, label = "average observed Y0 = 250", hjust = "center", size = 3) +
#   annotate(
#     geom = "curve", x = 2.35, y = 242.5, xend = 2.35, yend = 245,
#     curvature = .8, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(geom = "text", x = 2, y = 242, label = "average observed Y1 = 245", hjust = "center", size = 3) +
#   geom_segment(aes( x = 1.7, xend = 2.3, y = mean(Y[Z == 1 & first.race == 'no']), yend = mean(Y[Z == 1 & first.race == 'no'])), col = 1, linetype = 1) +
#   geom_segment(aes( x = 0.7, xend = 1.3, y = mean(Y[Z == 0 & first.race == 'no']), yend = mean(Y[Z == 0 & first.race == 'no'])), col = 1, linetype = 1) +
#   scale_color_manual(values = c(4, 2), labels = c('No hyperShoe', 'hyperShoe')) +
#   scale_shape_manual(values = c(17, 19)) +
#   scale_x_discrete(labels = c('No hyperShoe (Observed Y0)', 'hyperShoe (Observed Y1)')) +
#   theme_bw() +
#   coord_cartesian(ylim = c(240, 270)) +
#   labs(x = NULL, y = 'Running time', col = NULL, title = 'Averages of Observed Y0 and Observed Y1')
#
# ggsave('inst/app/www/learn/fundemental/plots/p12.png', device = 'png', height = 5, width = 8)
#
#
#
# dat4 <- dat
# dat4 <- dat4 %>% mutate(Y1 = case_when(
#   Z == 0 & first.race == 'yes' ~ 260,
#   Z == 0 & first.race == 'no' ~ 245,
#   TRUE ~ Y1
# ),
# Y0 = case_when(
#   Z == 1 & first.race == 'yes' ~ 265,
#   Z == 1 & first.race == 'no' ~ 250,
#   TRUE ~ Y0
# ))
# dat4 <- dat4 %>% mutate(ITE = Y1 - Y0)
# dat4 <- dat4 %>% select(runner, first.race, Z, Y0, Y1, Y, ITE)
#
#
#
# dat4$ITE_true <- with(dat, Y1 - Y0)
#
# dat4 %>%
#   ggplot(aes(runner, ITE, label = runner)) +
#   geom_point() +
#   geom_text_repel(show.legend = FALSE, seed = 4, size = 3) +
#   # annotate(
#   #   geom = "curve", x = 2, y = 8.5, xend = 2, yend = 10.5,
#   #   curvature = 0, arrow = arrow(length = unit(2, "mm"))
#   # ) +
#   # annotate(geom = "text", x = 6, y = 8, label = "predicted treatment effect of 11", hjust = "center", size = 3) +
#   # annotate(
#   #   geom = "curve", x = 4, y = -10.5, xend = 4, yend = -12.5,
#   #   curvature = 0, arrow = arrow(length = unit(2, "mm"))
#   # ) +
#   # annotate(geom = "text", x = 8, y = -10, label = "predicted treatment effect of -13", hjust = "center", size = 3) +
#   theme_bw() +
#   theme(legend.position = 'top', legend.justification='left') +
#   labs(x = 'Runner ID', y = 'Predicted Causal Effect of hyperShoes', linetype = NULL, title = 'Estimated Causal Effects')
# ggsave('inst/app/www/learn/fundemental/plots/p13.png', device = 'png', height = 5, width = 8)
#
#
#
#
# dat4 %>%
#   pivot_longer(contains('ITE')) %>%
#   ggplot(aes(runner, value, col = name, shape = name)) +
#   scale_color_manual(values = c(1, 6), labels = c('Predicted Causal Effect', 'True Causal Effect')) +
#   scale_shape_manual(values = c(19, 21), labels = c('Predicted Causal Effect', 'True Causal Effect')) +
#   geom_line(aes(group = runner,linetype = 'bias'), col = 'dark grey') +
#   scale_linetype_manual(values = 2) +
#   geom_point() +
#   #geom_hline(aes(yintercept = mean(value), col = name)) +
#   #geom_hline(yintercept = mean(dat3$ITE)) +
#   #geom_hline(yintercept = mean(dat3$ITE_true), col = 6, linetype = 2) +
#   theme_bw() +
#   theme(legend.position = 'top', legend.justification = 'left') +
#   labs(col = NULL, shape = NULL, linetype = NULL, x = 'Runner ID')
# ggsave('inst/app/www/learn/fundemental/plots/p14.png', device = 'png', height = 5, width = 8)
#
#
#
