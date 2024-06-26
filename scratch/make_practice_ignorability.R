load(file="~/Dropbox/ucgs_a_10712097_sm0001/data/sim.data")
imp1$first <- imp1$first - 1
covs.cont.n=c("bw","b.head","preterm","birth.o","nnhealth","momage")
covs.cat.n=c("sex","twin","b.marr","mom.lths","mom.hs",	"mom.scoll","cig","first","booze","drugs","work.dur","prenatal","ark","ein","har","mia","pen","tex","was")
p=length(c(covs.cont.n,covs.cat.n))


covs.ols = c(covs.cont.n,covs.cat.n)
X = imp1[,covs.ols]
#X = na.omit(X)
# now standardize the continuous variables
X[,covs.cont.n]=as.data.frame(t((t(X[,covs.cont.n])-unlist(lapply(X[,covs.cont.n],mean)))/sqrt(unlist(lapply(X[covs.cont.n],var)))))
# record numbers of units and covariates
N = nrow(X)
dimx = ncol(X)
non_na_coef <- !is.na(lm(rnorm(nrow(imp1)) ~ .^2, data = X, x = TRUE)$coef)

#model matrix
X <- model.matrix(rnorm(nrow(imp1)) ~ .^2, data = X)[,non_na_coef]
set.seed(21)
zcoef <- c(.1, rnorm(15, 1, .2),rnorm(10, -1.5, .2),
  sample(
  x = c(0.5, .1, -.1, -.4, .4),
  size = ncol(X) - p - 1,
  replace = T,
  prob = c(.2, .2, .2, .2, .2)))
pscore <- plogis(X %*% zcoef)
hist(pscore)
ihdp <- rbinom(nrow(imp1), 1, pscore)

ycoef <- c(0, runif(p, -.5, .5))
Xlin <- as.matrix(imp1[,covs.ols])
y0hat <-  cbind(rep(1, nrow(Xlin)), Xlin) %*% ycoef
y1hat <-  (cbind(rep(1, nrow(Xlin)), Xlin) %*% ycoef) + 4
y0 <- y0hat + rnorm(nrow(imp1), 0, 6)
y1 <- y1hat + rnorm(nrow(imp1), 0, 6)
mean(y1 - y0)
y <- ifelse(ihdp == 1, y1, y0)
dat <- cbind.data.frame(y, ihdp, Xlin)
#readr::write_rds(dat, '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/confounder/ihdp_obs.rds')
lm(y~ ihdp,data = dat)

X <- lapply(1:18, function(i){
  rbinom(985, 1, runif(1, .1, .9))
})

X <- dplyr::bind_cols(X)
colnames(X) <- colnames(Xlin)
X

loggit <- as.matrix(X) %*% c(runif(9, -1.5, -1), runif(9, 1.5, 2))
pscore <- plogis(loggit)
ihdp <- rbinom(985, 1, pscore)
ycoef <- runif(18, 10, 15)
y0hat <-  as.matrix(X) %*% ycoef
y1hat <-  (as.matrix(X) %*% ycoef) + 4
y0 <- y0hat + rnorm(nrow(imp1), 0, 6)
y1 <- y1hat + rnorm(nrow(imp1), 0, 6)

y <- ifelse(ihdp == 1, y1, y0)
mean(y1 - y0)
dat <- cbind.data.frame(y, ihdp, X)
readr::write_rds(dat, '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/confounder/ihdp_obs.rds')
lm(y~ ihdp + bw + sex,data = dat)
