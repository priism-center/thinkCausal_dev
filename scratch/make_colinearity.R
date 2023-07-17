library(tidyverse)
library(bartCause)

set.seed(5)
# create X
n <- 500
R <- matrix(nrow = 2, ncol = 2)
diag(R) <- 1
R[lower.tri(R)] <- c(.85)
R[upper.tri(R)] <- t(R[lower.tri(R)])
X <- MASS::mvrnorm(n, mu = c(0, 0), R)
interact <- X[, 1]*X[, 2]
X <- cbind(X, interact)
#X <- apply(X, 2, function(i){(i*7) + 76})

p.score <- vector(length = n)


p.score <- pnorm(-1.4*X[, 1] + 1.4*X[, 2] + .2*X[,3])
hist(p.score)





z <- rbinom(n, 1, p.score)

dat <- cbind.data.frame(X, z)

# make y
y0 <- 10 + cbind(rep(1, nrow(X)),X)%*%c(0, 0, 0, .6) + rnorm(n, 0, 1)
y1 <- 10 + cbind(rep(1, nrow(X)),X)%*%c(0, 0, 0, .6) + -.25*sd(y0) + rnorm(n, 0, 1)
y <- ifelse(z == 1, y1, y0)

unscale <- function(x){
  (x*10) + 150
}

X <- apply(X, 2, unscale)
y <- unscale(y)
y1 <- unscale(y1)
y0 <- unscale(y0)
hist(y)

colnames(X)[1:2] <- c('qualify1', 'qualify2')
colinearity <- data.frame(X[, 1:2], hyperShoe = z,Y0 = y0,Y1 = y1, Y = y)
colinearity <- purrr::map_df(colinearity, function(x) round(x, 0))
with(colinearity, mean(Y1 - Y0))

readr::write_csv(colinearity, 'inst/extdata/colinearity.csv')

summary(lm(Y ~ hyperShoe + qualify1 + qualify2, data = colinearity))

# plots

colinearity %>%
  group_by(hyperShoe) %>%
  summarise(mean(qualify1), mean(qualify2))


ggplot(colinearity, aes(qualify2, fill = as.factor(hyperShoe))) +
  geom_boxplot()


ggplot(colinearity, aes(qualify1, qualify2)) +
  geom_point() +
  theme_bw() +
  labs(title = 'Correlation of qualifying times = .85')

ggsave('inst/app/www/learn/colinearity/plots/p1.png', device = 'png', height = 5, width = 8)



both <- bartc(Y, hyperShoe, ., data = colinearity, seed = 2)
one_only <- bartCause::bartc(Y, hyperShoe, qualify1, data = colinearity, seed = 2, method.trt = 'none')
two_only <- bartCause::bartc(Y, hyperShoe, qualify2, data = colinearity, seed = 2, method.trt = 'none')
estimates <- rbind(summary(both, 'cate')$estimates,
                   summary(one_only, 'cate')$estimates,
                   summary(two_only, 'cate')$estimates)
estimates
