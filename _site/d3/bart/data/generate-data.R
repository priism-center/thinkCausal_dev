# data for scatter plot
library(dplyr)
library(ggplot2)
set.seed(44)

n <- 100
calories_consumed <- runif(n, min = 0, max = 1000)
z <- sample(c(0, 1), size = n, replace = TRUE)
decreasing_fn <- function(x) (-x^(1/15) + 1 ) * 1000
u_fn <- function(x) ((x - 500) ^2 / 1000) - 500
plot(u_fn(1:1000))
running_time <- 5000 + (z * u_fn(calories_consumed)) + rnorm(n, mean = 0, sd = 50)

# true ATE
mean(u_fn(calories_consumed))

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
        response = .data[, 3], 
        treatment = .data[, 1], 
        confounders = .data[, 2], 
        commonSup.rule = 'none',
        estimand = 'ate', 
        keepTrees = TRUE,
        seed = 2
      )

# TODO: replace this with original data?
caloriesConsumedTest <- seq(0, 1000, length.out = 1000);
lmFit0 <- predict(regEstimate, newdata = tibble(z = 0, caloriesConsumed = caloriesConsumedTest));
lmFit1 <- predict(regEstimate, newdata = tibble(z = 1, caloriesConsumed = caloriesConsumedTest));
treeFit0 <- predict(treeEstimate, newdata = tibble(z = 0, caloriesConsumed = caloriesConsumedTest));
treeFit1 <- predict(treeEstimate, newdata = tibble(z = 1, caloriesConsumed = caloriesConsumedTest));
bartFit0 = colMeans(predict(bartEstimate, tibble(zz = 0, caloriesConsumed = caloriesConsumedTest)));
bartFit1 = colMeans(predict(bartEstimate, tibble(zz = 1, caloriesConsumed = caloriesConsumedTest)));

.data_fitted <- tibble(
  caloriesConsumed = caloriesConsumedTest,
  diffFit0 = mean0,
  diffFit1 = mean1,
  lmFit0 = lmFit0,
  lmFit1 = lmFit1,
  treeFit0 = treeFit0,
  treeFit1 = treeFit1,
  bartFit0 = bartFit0,
  bartFit1 = bartFit1,
  trueFit0 = 5000,
  trueFit1 = 5000 + u_fn(caloriesConsumedTest)
)

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

# starting positions for draggable points
x <- seq(0, 1000, length.out = 8)
preds <- colMeans(predict(bartEstimate, tibble(zz = 1, caloriesConsumed = x)));
draggable_points <- tibble(runningTime = preds, caloriesConsumed = x)

# readr::write_csv(.data, 'd3/bart/data/observations.csv')
# readr::write_csv(.data_fitted, 'd3/bart/data/fits.csv')
# readr::write_csv(cred_int_ate, 'd3/bart/data/credible_intervals_ate.csv')
# readr::write_csv(cred_int_y0, 'd3/bart/data/credible_intervals_y0.csv')
# readr::write_csv(cred_int_y1, 'd3/bart/data/credible_intervals_y1.csv')
# readr::write_csv(draggable_points, 'd3/bart/data/draggable_points.csv')

