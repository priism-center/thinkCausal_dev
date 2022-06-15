library(dplyr)
library(ggplot2)
set.seed(44)

n <- 100
ATE <- 15
z <- sample(0:1, n, replace = TRUE)
pretest <- runif(n, 60, 80)
y0 <- (pretest * 1.05) + rnorm(n, 0, 2)
y1 <- (pretest * 1.05) + ATE + rnorm(n, 0, 2)
y <- (z * y1) + ((1 - z) * y0)

tibble(
  z = z,
  pretest = pretest,
  y0 = y0,
  y1 = y1,
  y = y
) %>%
  ggplot(aes(x = pretest, y = y, color = z)) +
  geom_point() +
  lims(x = c(60, 80), y = c(60, 100))


# readr::write_csv(.data, 'd3/bart/data/observations.csv')
# readr::write_csv(.data_fitted, 'd3/bart/data/fits.csv')
# readr::write_csv(cred_int_ate, 'd3/bart/data/credible_intervals_ate.csv')
# readr::write_csv(cred_int_y0, 'd3/bart/data/credible_intervals_y0.csv')
# readr::write_csv(cred_int_y1, 'd3/bart/data/credible_intervals_y1.csv')
# readr::write_csv(draggable_points, 'd3/bart/data/draggable_points.csv')

