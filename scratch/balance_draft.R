set.seed(2)
library(dplyr)
library(ggplot2)
X <- c(rbeta(100, 2, 2), rbeta(100, 2, 2))
major <- c(rbinom(100, 1, .6), rbinom(100, 1, .2))
z <- c(rep(1, 100), rep(0, 100))
y0 <- 70 + X*23 + major*3 + rnorm(100, 0, 2)
y1 <- 82 + exp((.07*(X*40))) + major*1 + rnorm(100, 0, 2)
y <- ifelse(z == 1, y1, y0)
tibble(X, y, z) %>%
  ggplot(aes(X, y, col = as.factor(z))) +
  geom_point()
mean(y1 - y0)



summary(lm(y ~ z + major + X))
summary(bartCause::bartc(y, z, cbind(major, X)))

hist(rbeta(100, 2, 1))
rbeta(100, 1, 2)*15


# imbalance with mean

z1 <- rbeta(10000, 3, 1)
z0 <- rbeta(10000, 1, 3)+ .5
library(dplyr)
library(tidyr)
library(ggplot2)

# equal mean variance  bias skew
z1 <- rbeta(10000, 3, 1)
z0 <- rbeta(10000, 1, 3)+ .5
tibble::tibble(z1, z0) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .7)

#equal mean bias skew and varriane
z1 <- rbeta(10000, 1, 1)
z0 <- rbeta(10000, 1, 3)+ .25
tibble::tibble(z1, z0) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .7)

# equal mean and skew bias varriance
z1 <- rbeta(10000, 1, 1)
z0 <- rbeta(10000, 3, 3)
tibble::tibble(z1, z0) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .7)


#


Article 1: What is it
fair comparion = balance + overlap
imbalance + lack of overlap = bad

# Intro basic idea (one example)
static image of balanced and overlaped data
Static of imbalance
Static of overlap problem (ATE)
Static of imbalance and lack overlap
quiz  guess the problems

# More nuance
balance equal mean variance and skew (swithches)
overlap can be ATE, ATT ATC or none (swithces)
balance and overlap are multivarite

# Diagnosing balance and overlap
what thinkCausal balance plots can tell you
what thinkCausal overlap can tell you

# Artilce 2
# Why do imbalance and lack of overlap cause bias?

# Article 3 What should you do:
#1) rct balanced in expectation and overlaped in expectation
#2) for balance you could model your way out
#3) for overlap you can estimand your way out (not always possible)

# Article 4
# BART overlap rules



sandbox





