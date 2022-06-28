# TODO: use the dgp from the textbook
# https://github.com/gperrett/BART_example/blob/master/simulate_univariate.R

draw <- function(N = 400, seed = NULL){
  set.seed(seed)
  
  X <- rnorm(N, 35, 8)
  
  X[X < 18] = 18
  X[X > 55] = 55
  
  dat <- data.frame(age = X, scaled_age = scale(X))
  
  dat$z <- rbinom(N, 1, .5)
  
  dat$y1 <- with(dat, 
                 180 -7 +
                   .5*scaled_age + I((scaled_age-.1)^2)*2  + 
                   rnorm(nrow(dat), 0, 3)
  )
  
  
  dat$true.1 <- with(dat, 
                     180 -7 +.5*scaled_age + I((scaled_age-.1)^2)*2 
  )
  
  dat$y0 <- with(dat, 
                 176 
                 +.5*scaled_age + I((scaled_age+.3)^2)*3.2 + 
                   rnorm(nrow(dat), 0, 3)
                 
  )
  
  dat$true.0 <-  with(dat, 
                      176 +.5*scaled_age + I((scaled_age+.3)^2)*3.2 
  )
  
  dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)
  return(dat)
}





# data for scatter plot
library(dplyr)
library(ggplot2)
.data <- draw(N = 400, seed = 2)
true.0 <- .data$true.0
true.1 <- .data$true.1

.data %>%
  ggplot(aes(x = age, y = y, group = z, color = as.factor(z))) +
  geom_point()

mean0 <- mean(.data$y[.data$z == 0])
mean1 <- mean(.data$y[.data$z == 1])
meanDiff <- mean1 - mean0

regEstimate <- lm(y ~ age + as.logical(z) , data = .data)
treeEstimate <- rpart::rpart(y ~ age + as.logical(z), 
                             data = .data, control = rpart::rpart.control(minbucket = 3L, cp = 0.005))
bartEstimate <- bartCause::bartc(y, z, age, data = .data, 
                                 commonSup.rule = 'none',
                                 estimand = 'ate', 
                                 keepTrees = TRUE,
                                 seed = 2
)
runningTime <- .data$y
age = .data$age
lmFit0 <- predict(regEstimate, newdata = tibble(z = 0, age = age));
lmFit1 <- predict(regEstimate, newdata = tibble(z = 1, age = age));
bartFit0 = apply(bartCause::extract(bartEstimate, 'mu.0'), 2, mean);
bartFit1 = apply(bartCause::extract(bartEstimate, 'mu.1'), 2, mean);

obs.m <- apply(bartCause::extract(bartEstimate, 'mu.obs'), 2, mean)
obs.sd <- apply(bartCause::extract(bartEstimate, 'mu.obs'), 2,sd)
obs.lcl <- obs.m - 1.96*obs.sd 
obs.ucl <- obs.m + 1.96*obs.sd 

cf.m <- apply(bartCause::extract(bartEstimate, 'mu.cf'), 2, mean)
cf.sd <- apply(bartCause::extract(bartEstimate, 'mu.cf'), 2,sd)
cf.lcl <- cf.m - 1.96*cf.sd 
cf.ucl <- cf.m + 1.96*cf.sd 

extract_credible_intervals <- function(posterior){
  probs <- c(0.025, 0.1, 0.9, 0.975)
  cred_int <- apply(posterior, 2, function(x) quantile(x, probs = probs))
  cred_int <- cred_int %>% t() %>% as_tibble()
  # colnames(cred_int) <- stringr::str_remove_all(paste0('q_', probs * 100), '\\.')
  colnames(cred_int) <- c('q_250', 'q_10', 'q_90', 'q_975') 
  
  # add means
  cred_int$x_mean <- colMeans(posterior)
  cred_int$age <- .data$age
  
  return(cred_int)
}
icates <- bartCause::extract(bartEstimate, "icate")
cred_int_ate <- extract_credible_intervals(icates)

.data_fitted <- tibble(
  age = age,
  diffFit0 = mean0,
  diffFit1 = mean1,
  lmFit0 = lmFit0,
  lmFit1 = lmFit1,
  bartFit0 = bartFit0,
  bartFit1 = bartFit1,
  trueFit0 = true.0,
  trueFit1 = true.1
)


.data_fitted %>%
  tidyr::pivot_longer(-age) %>%
  ggplot(aes(x = age, y = value, color = name)) +
  geom_line() +
  geom_point(data = .data, 
             aes(x = age, y = y, group = z, color = NULL)) +
  labs(title = 'Response surface modeled by diff means, lm, and bart',
       x = 'Calories consumed',
       y = 'Running time',
       color = 'Model')

z <- .data$z
cred_int_y0 <- tibble(q_250 = if_else(z == 0, obs.lcl, cf.lcl), 
                      q_975 = if_else(z == 0, obs.ucl, cf.ucl), 
                      x_mean = if_else(z == 0, obs.m, cf.m), 
                      age = age)

cred_int_y1 <- tibble(q_250 = if_else(z == 1, obs.lcl, cf.lcl), 
                      q_975 = if_else(z == 1, obs.ucl, cf.ucl), 
                      x_mean = if_else(z == 1, obs.m, cf.m), 
                      age = age)

# starting positions for draggable points
x <- seq(0, 1000, length.out = 8)
preds <- colMeans(predict(bartEstimate, tibble(z = 1, age = x)));
draggable_points <- tibble(runningTime = preds, age = x)


.data <- tibble(z, age, runningTime)
# additional data for overlap plot
# TODO: generate a duplicate version of bart.data for each overlap position and then rebuild the overlap plot

readr::write_csv(.data, 'd3/bart/data/observations.csv')
readr::write_csv(.data_fitted, 'd3/bart/data/fits.csv')
readr::write_csv(cred_int_ate, 'd3/bart/data/credible_intervals_ate.csv')
readr::write_csv(as_tibble(cred_int_y0), 'd3/bart/data/credible_intervals_y0.csv')
readr::write_csv(as_tibble(cred_int_y1), 'd3/bart/data/credible_intervals_y1.csv')
readr::write_csv(draggable_points, 'd3/bart/data/draggable_points.csv')

# 
efficency <- lapply(1:1000, function(i){
  .data <- draw(N = 400, seed = i)
  diff <- coef(lm(y ~ z, data = .data))[2]
  reg <- coef(lm(y ~ z + age, data = .data))[2]
  bart <-  bartCause::bartc(y, z, age, data = .data, seed = 2, method.trt = 'none', verbose = FALSE)
  bart <- summary(bart)$estimates[1]
  out <- list(diff, bart, reg)
  if(i%%100 == 0) message(paste0(i/1000*100, '% complete'))
  return(out)
})

means <- lapply(efficency, '[[', 1) %>% 
  bind_rows()
bart <- lapply(efficency, '[[', 2) %>% 
  bind_rows()
regression <- lapply(efficency, '[[', 3) %>% 
  bind_rows()

efficency_df <- cbind(bart, regression, means) 
names(efficency_df) <- c('bart', 'regression', 'mean')
 efficency_df %>% 
 pivot_longer(1:3) %>% 
 ggplot(aes(value, fill = name)) + 
 geom_histogram(position = 'identity', alpha = .7, bins = 20, color = 'black') +
   facet_wrap(~name, ncol = 1)
