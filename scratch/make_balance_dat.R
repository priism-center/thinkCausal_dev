dgp <- function(design = T, response = 'Linear', sample = 200){
  set.seed(21)
  if(design == 'Random') param <- 2 else param <- 1.3
  stat1_t <- rbeta(sample/2, 2, param)
  stat1_c <- rbeta(sample/2, param, 2)


  if(design == 'Random') prob_t <- .4 else prob_t <- .6
  if(design == 'Random') prob_c <- .4 else prob_c <- .2

  major_t <- rbinom(sample/2, 1, prob_t)
  major_c <- rbinom(sample/2, 1, prob_c)
  z <- c(rep('treatment', sample/2), rep('control', sample/2))

  dat <- data.frame(z,
                    major = c(major_t, major_c),
                    pre_test = c(stat1_t, stat1_c))

  if(resp == 'Linear'){
    y1 <- 75 + dat$pre_test*23 + dat$major*1 + rnorm(sample/2, 0, 2)
    y0 <- 70 + dat$pre_test*23 + dat$major*3 + rnorm(sample/2, 0, 2)
    y <- ifelse(dat$z == 'treatment', y1, y0)
  }else{
    y0 <- 70 + dat$pre_test*23 + dat$major*3 + rnorm(sample/2, 0, 2)
    y1 <- 82 + exp((.07*(dat$pre_test*40))) + dat$major*1 + rnorm(sample/2, 0, 2)
    y <- ifelse(z == 'treatment', y1, y0)

  }

  dat <- cbind.data.frame(y1, y0, y, dat)
  dat$pre_test <- (dat$pre_test*200)/2
  return(dat)
}
dat1 <- dgp(rct = T, resp = 'Linear')
dat2 <- dgp(rct = F, resp = 'Linear')
dat3 <- dgp(rct = T, resp = 'Non-linear')
dat4 <- dgp(rct = F, resp = 'Non-linear')

dat <- list(dat1, dat2, dat3, dat4)
readr::write_rds(dat, 'inst/extdata/balance_dat.rds')

library(ggplot2)
library(dplyr)
dat1 %>%
  ggplot(aes(pre_test, fill = as.factor(z))) +
  geom_histogram(bins = 10, alpha = .6, position = 'identity', col = 'black') +
  geom_vline(aes(xintercept = dat1 %>% filter(z == 'treatment') %>% summarise(mean(pre_test)) %>% purrr::as_vector()))

dat1 %>%
  mutate(pre_test = (pre_test*200)/2) %>%
  ggplot2::ggplot(aes(pre_test, y, col = z)) +
  geom_point()

dat[[1]] %>%
  mutate(standard = sd(pre_test)) %>%
  dplyr::group_by(z) %>%
  summarise(average = mean(pre_test), standard = mean(standard)) %>%
  tidyr::pivot_wider(names_from = z, values_from = average) %>%
  dplyr::mutate(balance = treatment - control,
                s.balance = (treatment - control)/standard)

0.0249/sd(dat1$pre_test)

ggplot2::ggplot(aes(pre_test, y, col = z)) +
  geom_point()


hist(dat[[1]]$pre_test[1:100])


with(dat4, mean(y1 - y0))
X <- dat4[, 5:7]
z <- c(rep(1, 100), rep(0, 100))
fit <- bartCause::bartc(dat4$y, z, X)

lm(y ~ z + placement_test + major + confidence, dat4)
