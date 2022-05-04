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
bartFit0 = colMeans(predict(bartEstimate, tibble(zz = 0, caloriesConsumed = caloriesConsumedTest)));
bartFit1 = colMeans(predict(bartEstimate, tibble(zz = 1, caloriesConsumed = caloriesConsumedTest)));

.data_fitted <- tibble(
  caloriesConsumed = caloriesConsumedTest,
  diffFit0 = mean0,
  diffFit1 = mean1,
  lmFit0 = lmFit0,
  lmFit1 = lmFit1,
  bartFit0 = bartFit0,
  bartFit1 = bartFit1
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
  

# readr::write_csv(.data, 'd3/bart/data/observations.csv')
# readr::write_csv(.data_fitted, 'd3/bart/data/fits.csv')

# plotBart::plot_moderator_c_pd(bartEstimate, .data$caloriesConsumed)
# plotBart::plot_CATE(bartEstimate)
# plotBart::plot_ICATE(bartEstimate, nbins = 15)
# plotBart::plot_moderator_search(bartEstimate)
