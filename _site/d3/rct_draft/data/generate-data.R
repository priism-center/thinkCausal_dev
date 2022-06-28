# TODO: use the dgp from the textbook
# https://github.com/gperrett/BART_example/blob/master/simulate_univariate.R

# data for scatter plot
library(dplyr)
library(ggplot2)

draw <- function(N = 200, seed = NULL){
  set.seed(seed)
  
  X <- rnorm(N, 35, 8)
  
  X[X < 18] = 18
  X[X > 55] = 55
  
  dat <- data.frame(age = X, scaled_age = scale(X))
  
  dat$z <- rbinom(N, 1, .5)
  
  dat$y1 <- with(dat, 
                 180 -7 +
                   .5*scaled_age + I((scaled_age+.3)^2)*2 + 
                   rnorm(nrow(dat), 0, 2)
  )
  
  
  dat$true.1 <- with(dat, 
                     180 -7 +
                       .5*scaled_age + I((scaled_age+.3)^2)*2
  )
  
  dat$y0 <- with(dat, 
                 176 +.5*scaled_age + I((scaled_age+.9)^2)*3.2 + 
                   rnorm(nrow(dat), 0, 2)
                 
  )
  
  dat$true.0 <-  with(dat, 
                      176 +.5*scaled_age + I((scaled_age+.9)^2)*3.2 
  )
  
  dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)
  return(dat)
}
.data <- draw(seed = 2)
calories_consumed <- .data$age
z <- .data$z
running_time <- .data$y
true.0 <- .data$true.0
true.1 <- .data$true.1
# true ATE
.data <- tibble::tibble(
  z = z,
  caloriesConsumed = calories_consumed,
  runningTime = running_time)
.data %>%
  ggplot(aes(x = caloriesConsumed, y = runningTime, group = z, color = as.factor(z))) +
  geom_point()

mean0 <- mean(.data$runningTime[.data$z == 0])
mean1 <- mean(.data$runningTime[.data$z == 1])
meanDiff <- mean1 - mean0
regEstimate <- lm(runningTime ~ caloriesConsumed + as.logical(z), data = .data)
treeEstimate <- rpart::rpart(runningTime ~ caloriesConsumed + as.logical(z), 
                             data = .data, control = rpart::rpart.control(minbucket = 3L, cp = 0.005))
bartEstimate <- bartCause::bartc(
        response = runningTime, 
        treatment = z, 
        confounders = caloriesConsumed, 
        data = .data,
        estimand = 'ate', 
        keepTrees = TRUE,
        seed = 2
      )

# TODO: replace this with original data?
caloriesConsumed <- .data$caloriesConsumed
lmFit0 <- predict(regEstimate, newdata = tibble(z = 0, caloriesConsumed = caloriesConsumed));
lmFit1 <- predict(regEstimate, newdata = tibble(z = 1, caloriesConsumed = caloriesConsumed));
bartFit0 = colMeans(predict(bartEstimate, tibble(z = 0, caloriesConsumed = caloriesConsumed)));
bartFit1 = colMeans(predict(bartEstimate, tibble(z = 1, caloriesConsumed = caloriesConsumed)));

.data_fitted <- tibble(
  caloriesConsumed = caloriesConsumed,
  diffFit0 = mean0,
  diffFit1 = mean1,
  lmFit0 = lmFit0,
  lmFit1 = lmFit1,
  bartFit0 = bartFit0,
  bartFit1 = bartFit1,
  trueFit0 = true.0,
  trueFit1 = true.1)

.data_fitted <- .data_fitted %>% arrange(caloriesConsumed)
.data_fitted %>%
  tidyr::pivot_longer(-caloriesConsumed) %>%
  ggplot(aes(x = caloriesConsumed, y = value, color = name)) +
  geom_line() +
  geom_point(data = .data, 
    aes(x = caloriesConsumed, y = runningTime, group = z, color = NULL)) +
  labs(title = 'Response surface modeled by diff means, lm, and bart',
    x = 'Calories consumed',
    y = 'Running time',
    color = 'Model')


# plotBart::plot_moderator_c_pd(bartEstimate, .data$caloriesConsumed)
# plotBart::plot_CATE(bartEstimate)
# plotBart::plot_ICATE(bartEstimate, n_bins = 15)
# plotBart::plot_moderator_search(bartEstimate)

# credible intervals for ATE
# TODO: this uses observations, not fitted values
extract_credible_intervals <- function(posterior){
  probs <- c(0.025, 0.1, 0.9, 0.975)
  cred_int <- apply(posterior, 2, function(x) quantile(x, probs = probs))
  cred_int <- cred_int %>% t() %>% as_tibble()
  # colnames(cred_int) <- stringr::str_remove_all(paste0('q_', probs * 100), '\\.')
  colnames(cred_int) <- c('q_250', 'q_10', 'q_90', 'q_975') 

  # add means
  cred_int$x_mean <- colMeans(posterior)
  cred_int$index <- rank(.data$caloriesConsumed, ties.method = 'first') # TODO is this right?
  cred_int$caloriesConsumed <- .data$caloriesConsumed

  return(cred_int)
}
icates <- bartCause::extract(bartEstimate, "icate")
cred_int_ate <- extract_credible_intervals(icates)

# credible intervals for observed data
y0 <- bartCause::extract(bartEstimate, "y.0") # TODO: this is the wrong argument
y1 <- bartCause::extract(bartEstimate, "y.1") # TODO: this is the wrong argument
cred_int_y0 <- extract_credible_intervals(y0)
cred_int_y1 <- extract_credible_intervals(y1)

# # starting positions for draggable points
# x <- seq(0, 1000, length.out = 8)
# preds <- colMeans(predict(bartEstimate, tibble(zz = 1, caloriesConsumed = x)));
# draggable_points <- tibble(runningTime = preds, caloriesConsumed = x)
# 
# # additional data for overlap plot
# # TODO: generate a duplicate version of bart.data for each overlap position and then rebuild the overlap plot

 readr::write_csv(.data, 'd3/rct_draft/data/observations.csv')
 readr::write_csv(.data_fitted, 'd3/rct_draft/data/fits.csv')
 readr::write_csv(cred_int_ate, 'd3/rct_draft/data/credible_intervals_ate.csv')
 readr::write_csv(cred_int_y0, 'd3/rct_draft/data/credible_intervals_y0.csv')
 readr::write_csv(cred_int_y1, 'd3/rct_draft/data/credible_intervals_y1.csv')
 #readr::write_csv(draggable_points, 'd3/rct_draft/data/draggable_points.csv')

