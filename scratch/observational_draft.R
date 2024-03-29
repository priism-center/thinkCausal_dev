set.seed(2)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(bartCause)
library(stringr)

# set plot theme
theme_set(theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             text = element_text(size = 16)
))
# make dat 1
N <- 200
X <- runif(N, 18, 55)

dat <- data.frame(age = X, scaled_age = scale(X))

p.score <- if_else(X > 35, 0, .65)
z <- rbinom(N, 1, p.score)

y0 <- 180 + dat$age*.75 + rnorm(N)
y1 <- 180 + dat$age*.75 - 5 + rnorm(N)
y <- ifelse(z == 1, y1, y0)
z <- as.factor(z)


dat1 <- data.frame(y, y1, y0, z, age = dat$age)

bart <- bartCause::bartc(dat1$y, as.numeric(dat1$z) - 1, dat1$age, estimand =  'att')
dat1$mu.obs <- NA
dat1$mu.cf <- NA

dat1$mu.obs[dat1$z == 1] <- apply(bartCause::extract(bart, 'mu.obs'), 2, mean)
dat1$mu.cf[dat1$z == 1] <- apply(bartCause::extract(bart, 'mu.cf'), 2, mean)

ggplot(dat1, aes(age, y, col = z)) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  labs(title = 'Observed data',
       subtitle = 'These are all of our factual outcomes',
       y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p1.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

ggplot(dat1, aes(age, y, col = z)) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  ggplot2::labs(title = 'ATT has complete overlap',
                y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p2.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

(mean(dat1[dat1$z == 1, 'age']) - mean(dat1[dat1$z == 0, 'age']))

ggplot() +
  aes(x = -15:5, y = 'age') +
  geom_point(aes(x = -13.95844, y = 'age'), size = 4) +
  coord_cartesian(xlim = c(-15, 5)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_label(aes(x = 0, y = 1.5, label = '0 = Perfect Balance with Control Group')) +
  ggplot2::labs(title = 'Balance Plot',
                subtitle = 'the treatment and control groups are not balanced',
                y = element_blank(), x = 'balance')

ggsave('p3.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

dat1 %>%
  tidyr::pivot_longer(cols = c(y1, y0)) %>%
  mutate(po = if_else(str_sub(name, -1) == z, 'factual', 'counter factual')) %>%
  ggplot() +
  geom_point(aes(age, value, col = z, shape = po)) +
  geom_segment(data = dat1 %>% filter(z==1), aes(x = age, xend = age, y = y1, yend = y0)) +
  scale_shape_manual(values = c(21, 19)) +
  scale_color_manual(values = c(4, 2)) +
  labs(title = 'Access to all potential outcomes',
       subtitle = 'this is always impossible when doing reseach in practice',
       y = 'Y running times',
       shape = NULL) +
  ggplot2::theme(legend.position = 'top')

ggsave('p4.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)


dat1 %>%
  filter(z == 1) %>%
  mutate(ICE = y1 - y0) %>%
  arrange(ICE) %>%
  mutate(ord = row_number()) %>%
  ggplot() +
  geom_segment(aes(x = ord, xend = ord, y = 0, yend = ICE)) +
  ggplot2::geom_hline(aes(yintercept = mean(ICE))) +
  ggplot2::geom_label(aes(x = 55, y = mean(ICE), label = paste0('True ATT = ', round(mean(ICE), 1))), size = 5)  +
  labs(title = 'Calculating the true ATT',
       subtitle = 'We can never calculate this in the real world',
       y = 'Individual Causal Effects',
       x = 'Ordered from largest to smallest') +
  ggplot2::theme(axis.text.x = element_blank())

ggsave('p5.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)



dat1 %>%
  ggplot() +
  geom_point(data = dat1, aes(age, y, col = z)) +
  scale_color_manual(values = c(4, 2)) +
  geom_line(aes(age, 210.4182), col = 4) +
  geom_line(aes(age, 195.011), col = 2) +
  ggplot2::geom_segment(
    aes(x = 35,
        xend = 35,
        y = 210.4182 ),
    yend = 195.011, col = 1) +
  ggplot2::geom_label(
    aes(x = 35, y = 215,
        label = 'Estimated ATT = -15.4'), size = 5) +
  ggplot2::labs(title = 'Estimated ATT is -15.4 with 95% CI: (-17.5, -13.4)',
                subtitle = 'True ATT = -4.9',
                y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p6.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

reg <- lm(y ~ z + age, dat1)

dat1 %>%
  ggplot() +
  geom_point(aes(age, y, col = z)) +
  scale_color_manual(values = c(4, 2)) +
  geom_abline(intercept = 180.4494 , slope = 0.7422 , col = 4) +
  geom_abline(intercept = 175.4016, slope = 0.7422 , col = 2) +
  ggplot2::geom_segment(
    aes(x = 35,
        xend = 35,
        y = 180.4494 + 0.7422 *35),
    yend = sum(180.4494, -5.0478, 0.7422*35), col = 1) +
  ggplot2::geom_label(
    aes(x = 35, y = 215,
        label = 'Estimated ATT = -5.0'), size = 5) +
  ggplot2::labs(title = 'Estimated ATT is -5.0 with 95% CI: (-5.4, -4.7)',
                subtitle = 'True ATT = -4.9',
                y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p7.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

dat1 %>%
  ggplot() +
  geom_point(aes(age, y, col = z)) +
  scale_color_manual(values = c(4, 2)) +
  geom_segment(data = dat1 %>% filter(z == 1), aes(x = age, xend = age, y = mu.obs, yend = mu.cf)) +
  geom_line(data = dat1 %>% filter(z == 1), aes(x = age, y = mu.obs), col = 2) +
  geom_line(data = dat1 %>% filter(z == 1), aes(x = age, y = mu.cf), col = 4) +
  ggplot2::geom_label(
    aes(x = 35, y = 215,
        label = 'Estimated ATT = -4.9'), size = 5) +
  ggplot2::labs(title = 'Estimated ATT is -4.9 with 95% CI: (-5.3, -4.4)',
                subtitle = 'True ATT = -4.9',
                y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p8.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

comp1 <- data.frame(model = c('difference in means', 'regression with covariates', 'BART with covariates'),
                    estimate = c(-15.4, -5.0, -4.9),
                    lower.ci = c(-17.45981, -5.417691, -5.327),
                    upper.ci = c(-13.35439, -4.677999, -4.386))

comp1$`interval length` <-  with(comp1, upper.ci - lower.ci)
comp1$model <- factor(comp1$model, levels = comp1$model)

comp1 %>%
  ggplot(aes(estimate, reorder(model, dplyr::desc(model)))) +
  geom_point(size = 3) +
  ggplot2::geom_errorbarh(aes(xmin = lower.ci, xmax = upper.ci), height = .1) +
  geom_vline(xintercept = -4.9, linetype =2) +
  geom_label(x = -4.9, y = 3.3, label = 'True ATT = -4.9') +
  coord_cartesian(xlim = c(-17, -3)) +
  labs(y = element_blank()) +
  ggplot2::labs(title = 'Comparing statistical models',
                subtitle = 'In practice we never know the true ATT',
                x = 'Estimated ATT') +
  theme(legend.position = 'top')

ggsave('p9.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)


# make dat 2
set.seed(2)
N <- 150
X <- runif(N, 18, 55)
X <- c(X, runif(50, 18, 22))

dat <- data.frame(age = X, scaled_age = scale(X))

p.score <- if_else(X > 35, 0, .75)
z <- rbinom(200, 1, p.score)
dat <- tibble(age = X, z)
y1 <- 179 + sqrt(dat$age -18)*3 + rnorm(200)
y0 <- 190 + exp((.05*X)) + rnorm(200)
y = if_else(z == 1, y1, y0)
dat2 <- cbind.data.frame(y1, y0, y, dat)

bart <- bartCause::bartc(y, z, X, estimand = 'att')
dat2$mu.obs <- NA
dat2$mu.cf <- NA
dat2$mu.obs[dat2$z == 1] <- apply(bartCause::extract(bart, 'mu.obs'), 2, mean)
dat2$mu.cf[dat2$z == 1] <- apply(bartCause::extract(bart, 'mu.cf'), 2, mean)


ggplot(dat2, aes(age, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  ggplot2::labs(title = 'Non-linear response surface',
                subtitle = 'Only factual outcomes are shown',
                y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p10.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

dat2 %>%
  pivot_longer(cols = 1:2) %>%
  mutate(po = if_else(str_sub(name, -1) == z, 'factual outcome', 'counter-factual outcome')) %>%
  ggplot() +
  geom_point(aes(age, value, col = as.factor(z), shape = po)) +
  geom_line(aes(age, 179 + sqrt(age -18)*3 ), col = 1) +
  geom_line(aes(age, 190 + exp((.05*age))), col = 1) +
  geom_segment(data = dat2 %>% filter(z == 1), aes(x = age, xend = age, y = y1, yend = y0)) +
  scale_shape_manual(values = c(21, 19)) +
  scale_color_manual(values = c(4, 2)) +
  ggplot2::labs(title = 'All potential outcomes',
                subtitle = 'We can never calculate these in the real world',
                y = 'Y running times',
                color = 'Z',
                shape = NULL) + theme(legend.position = 'top')

ggsave('p11.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

dat2 %>%
  filter(z == 1) %>%
  mutate(ICE = y1 - y0) %>%
  arrange(ICE) %>%
  mutate(ord = row_number()) %>%
  ggplot() +
  geom_segment(aes(x = ord, xend = ord, y = 0, yend = ICE)) +
  ggplot2::geom_hline(aes(yintercept = mean(ICE))) +
  ggplot2::geom_label(aes(x = 55, y = mean(ICE), label = paste0('True ATT = ', round(mean(ICE), 1))), size = 5) +
  ggplot2::labs(title = 'Calculating the true ATT',
                subtitle = 'We can never calculate this in the real world',
                y = 'Individual Causal Effects',
                x = 'Ordered from largest to smallest') +
  ggplot2::theme(axis.text.x = element_blank())

ggsave('p12.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)
dat2 %>%
  ggplot() +
  geom_point(aes(age, y, col = as.factor(z))) +
  geom_line(aes(age, 198.3161), col = 4) +
  geom_line(aes(age,  185.7014), col = 2) +
  geom_segment(aes(x = 35, xend = 35, y = 198.3161, yend = 198.3161 -12.61)) +
  geom_label(aes(x = 35, y = 202, label = 'Estimated ATT = -12.6')) +
  scale_shape_manual(values = c(21, 19)) +
  scale_color_manual(values = c(4, 2)) +
  ggplot2::labs(title = 'Estimated ATT is -12.6 with 95% CI: (-13.7, -11.6)',
                subtitle = 'True ATT = -7.9',
                color = 'z',
                y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p13.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)


reg2 <- lm(y ~ z + age, dat2)
dat2 %>%
  ggplot() +
  geom_point(aes(age, y, col = as.factor(z))) +
  geom_abline(intercept = 183.3755 , slope =  0.3766, col = 4) +
  geom_abline(intercept = 183.3755  + -6.6367 , slope =  0.3766, col = 2) +
  geom_segment(aes(x = 35, xend = 35, y = 183.3755  + 35*0.3766, yend = 183.3755 + -6.6367 + 35*0.3766)) +
  geom_label(aes(x = 35, y = 202, label = 'Estimated ATT = -6.6')) +
  scale_shape_manual(values = c(21, 19)) +
  scale_color_manual(values = c(4, 2)) +
  ggplot2::labs(title = 'Estimated ATT is -6.6 with 95% CI: (-7.2, -6.0)',
                subtitle = 'True ATT = -7.9',
                color = 'z',
                y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p14.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)

dat2 %>%
  ggplot() +
  geom_segment(data = dat2 %>% dplyr::filter(z == 1), aes(x = age, xend = age, y = mu.obs, yend = mu.cf)) +
  geom_point(aes(age, y, col = as.factor(z))) +
  geom_line(data = dat2 %>% filter(z == 1), aes(age, mu.obs), col = 2) +
  geom_line(data = dat2 %>% filter(z == 1), aes(age, mu.cf), col = 4) +
  geom_label(aes(x = 35, y = 202, label = 'Estimated ATT = -8.0')) +
  scale_shape_manual(values = c(21, 19)) +
  scale_color_manual(values = c(4, 2)) +
  ggplot2::labs(title = 'Estimated ATT is -8.0 with 95% CI: (-8.6, -7.4)',
                subtitle = 'True ATT = -7.9',
                color = 'z',
                y = 'Y running times') +
  theme(legend.position = 'top')

ggsave('p15.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)


comp2 <- data.frame(model = c('difference in means', 'regression with all confounders', 'BART with all confounders'),
                    estimate = c(-12.6, -6.6, -8.0),
                    lower.ci = c(-13.65272, -7.231296, -8.573),
                    upper.ci = c(-11.57668, -5.968704, -7.444))

comp2$interval.length <- with(comp2, upper.ci - lower.ci)
comp2$model <- factor(comp2$model, levels = comp2$model)

comp2 %>%
  ggplot() +
  geom_point(aes(estimate, reorder(model, dplyr::desc(model))), size = 3) +
  geom_errorbarh(aes(xmin = lower.ci, xmax = upper.ci, y = model), height = .1) +
  geom_vline(xintercept = -7.9, linetype = 2) +
  geom_label(aes(x = -7.9, y = 3.3, label = 'True ATT = -7.9')) +
  labs(y = element_blank()) +
  ggplot2::labs(y = element_blank(),
                x = 'Estimated ATT',
                title = 'Comparing statistical model with non-linear data',
                subtitle = 'In practice we never know the true ATT')

ggsave('p16.png',
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/observational-analysis/plots/',
       width = 9,
       height = 7)


