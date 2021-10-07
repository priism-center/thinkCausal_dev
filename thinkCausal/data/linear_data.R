set.seed(2)
n <- 500
dat <- matrix(rnorm(20 * n), n, 20)
beta.z <- sample(c(.1, -.1, .2, -.2, -.5, .5,.25, -.25, .33, -.33, 0), size = 20, replace = T)
beta.y <- sample(c(.1, -.1, .2, -.2, -.5, .5,.25, -.25, .33, -.33, 0), size = 20, replace = T)

p.score <- exp(dat %*% beta.z * .8)/(1+ exp(dat %*% beta.z * .8))
z <- rbinom(n, 1, p.score)
sum(z)
y1 <- dat %*% beta.y + rnorm(n) + 5
y0 <- dat %*% beta.y + rnorm(n)
y <- ifelse(z ==1, y1, y0)
dat <- data.frame(y, z, dat)
mean(y1 - y0)
readr::write_csv(dat, '~/Dropbox/PRIISM/thinkCausal_dev/Development/data/linear_data.csv')
