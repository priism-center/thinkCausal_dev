# test one for data uplod
dat <- data.frame(z = rbinom(150, 1, .5), x = rnorm(150))
dat$y <- ifelse(dat$z ==1 , rnorm(150, 2, 1), rnorm(150, 1, 1))
write.csv(dat, '~/Dropbox/PRIISM/Navbar_split/tests/test1.csv')
