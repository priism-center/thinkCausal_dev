# observational 
confounders <- 200
R <- matrix(nrow = confounders , ncol = confounders )
diag(R) <- 1
set.seed(2)
R[lower.tri(R)] <- 0
R[upper.tri(R)] <- t(R[lower.tri(R)])
X <- MASS::mvrnorm(500, mu = rep(0, confounders), R)
beta <- rnorm(confounders , 0, .2)
p.score <- pnorm(X%*%beta)
z <- rbinom(500, 1, p.score)
beta <- rnorm(confounders , .5, .5)
y1 <- X%*%beta + 3 + rnorm(500)
y0 <- X%*%beta + rnorm(500)
y <- ifelse(z ==1, y1, y0)

dat2 <- cbind.data.frame(y, z, X)
mat <- cbind(z, X[, 1:100])
XtX = t(mat)%*%mat
C <- solve(XtX)
Betas <- C %*% t(mat) %*% y
resids = y-mat%*%Betas
df = nrow(mat)-ncol(mat)
s2 = (t(resids)%*%resids)/df
se = sqrt(c(s2)*diag(C))

out <- c(Betas[1],Betas[1] - 1.96*se[1], Betas[1] + 1.96*se[1])
names(out) <- c('est', 'lcl', 'ucl')
mean(y1 -y0);out

summary(lm(y ~ ., dat2))
mean(y1 -y0)

# random 
# observational 
covariates <- 200
R <- matrix(nrow = covariates , ncol = covariates )
diag(R) <- 1
set.seed(2)
R[lower.tri(R)] <- 0
R[upper.tri(R)] <- t(R[lower.tri(R)])
X <- MASS::mvrnorm(500, mu = rep(0, covariates), R)
z <- rbinom(500, 1, .5)
beta <- rnorm(covariates , .5, .5)
y1 <- X%*%beta + 3 + rnorm(500)
y0 <- X%*%beta + rnorm(500)
y <- ifelse(z ==1, y1, y0)

dat2 <- cbind.data.frame(y, z, X)
mat <- cbind(z, X[, 1:100])
XtX = t(mat)%*%mat
C <- solve(XtX)
Betas <- C %*% t(mat) %*% y
resids = y-mat%*%Betas
df = nrow(mat)-ncol(mat)
s2 = (t(resids)%*%resids)/df
se = sqrt(c(s2)*diag(C))

out <- c(Betas[1],se[1], Betas[1] - 1.96*se[1], Betas[1] + 1.96*se[1])
names(out) <- c('est','se', 'lcl', 'ucl')
full <- lm(y ~ ., dat2)
c(full$coef[2], sqrt(diag(vcov(full)))[2])
mean(y1 -y0);out


# observational non-linear
library(aciccomp2016)
data("input_2016")
dat <- input_2016
index <- sample(1:nrow(dat), n)
X <- dat[index, ]
oracle <- dgp_2016(input_2016, 2, 1)
y <- oracle$y[index]
z <- oracle$z[index]
ice <- oracle$y.1[index] - oracle$y.0[index]
att <- mean(ice[z == 1])

summary(bartc(y, z, X, estimand = 'att'))
lm.dat <- cbind(y, z, X)
lm.fit <- lm(y ~ ., lm.dat)
length(lm.fit$coefficients)
lm.fit$coefficients[2]

confounders <- n
R <- matrix(nrow = confounders , ncol = confounders )
diag(R) <- 1
set.seed(2)
R[lower.tri(R)] <- 0
R[upper.tri(R)] <- t(R[lower.tri(R)])
X <- MASS::mvrnorm(500, mu = rep(0, confounders), R)
beta <- rnorm(confounders , 0, .2)
p.score <- pnorm(X%*%beta)
z <- rbinom(500, 1, p.score)
beta <- rnorm(confounders , .5, .5)
y1 <- X%*%beta + 3 
y0 <- X%*%beta
y <- ifelse(z ==1, y1, y0)

dat2 <- cbind.data.frame(y, z, X)
mat <- cbind(z, X[, 1:100])
XtX = t(mat)%*%mat
C <- solve(XtX)
Betas <- C %*% t(mat) %*% y
resids = y-mat%*%Betas
df = nrow(mat)-ncol(mat)
s2 = (t(resids)%*%resids)/df
se = sqrt(c(s2)*diag(C))

out <- c(Betas[1],Betas[1] - 1.96*se[1], Betas[1] + 1.96*se[1])
names(out) <- c('est', 'lcl', 'ucl')
mean(y1 -y0);out

summary(lm(y ~ ., dat2))
mean(y1 -y0)



# shiny 
data("input_2016")
dat <- input_2016

sliderInput(inputId = 'n', min = 100, max = 2000, step = 100, value = 200, label = 'Sample Size')

output$many_x_plt <- renderPlot({
  index <- sample(1:nrow(dat), input$n)
  X <- dat[index, ]
  oracle <- dgp_2016(input_2016, 2, 1) # set to 2 or 3
  y <- oracle$y[index]
  z <- oracle$z[index]
  ice <- oracle$y.1[index] - oracle$y.0[index]
  att <- round(mean(ice[z == 1]), 2)
  all <- bartc(y, z, X, estimand = 'att')
  five <- bartc(y, z, X[, sample(1:length(X), 5)], estimand = 'att')
  ten <- bartc(y, z, X[, sample(1:length(X), 10)], estimand = 'att')
  twenty <- bartc(y, z, X[, sample(1:length(X), 20)], estimand = 'att')
  fourty <- bartc(y, z, X[, sample(1:length(X), 40)], estimand = 'att')
  
  est <- rbind(summary(all, target = 'sate')$estimates,
               summary(five, target = 'sate')$estimates,
               summary(ten, target = 'sate')$estimates,
               summary(twenty, target = 'sate')$estimates,
               summary(fourty, target = 'sate')$estimates
  )
  est$y <- c('all varibles', 'five predictor varibales', 'ten predictor variables', 'twnety predictor variables', 'fourty predictor variables')
  ggplot(est, aes(estimate, y)) + 
    geom_point(size = 3) + 
    geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height = .05) +
    geom_vline(xintercept = att, linetype =2) +
    geom_label(x = att, y = 1.3, label = paste('True ATT =', att)) +
    labs(y = element_blank()) +  
    coord_cartesian(xlim = c(2, 7)) + 
    theme_bw()
  
})

plotOutput('many_x_plt')
aciccomp2016::parameters_2016


