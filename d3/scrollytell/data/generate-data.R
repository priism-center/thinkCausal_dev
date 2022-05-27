# data for scatter plot
# this is just placeholder data for testing
# TODO: replace data with proper sim

library(dplyr)

n <- 10
y0 <- rnorm(n, 3, 0.5)
y1 <- rnorm(n, 2, 0.5)

z <- rep(0:1, n)
# factual <- sort(rep(0:1, n))
pair_id <- rep(seq_len(n), 2)

.data <- data.frame(
    xName = sort(rep(0:1, n)), #jitter(sort(rep(0:1, n))),
    yName = c(y0, y1),
    y = c(rep("y0", n), rep("y1", n)),
    treatment = z,
    factual = as.integer(z == sort(rep(0:1, n))),
    pair_id = pair_id
)
ggplot2::ggplot(
  .data, 
  ggplot2::aes(x = xName, y = yName, color = as.factor(treatment), 
               shape = as.factor(factual), group = factual)) + 
  ggplot2::geom_point()

# readr::write_csv(.data, '../_site/d3/pairing/data/point-data.csv')
# readr::write_csv(.data, 'd3/scrollytell/data/point-data.csv')



.dataLines <- .data %>%
  select(xName, yName, y, pair_id, treatment) %>% 
  tidyr::pivot_wider(names_from = y, values_from = c(xName, yName))

dat <- data.frame(
pair_id = 1:10, 
treatment = rep(c(0,1), 5), 
xName_y0 = rep(0, 10), 
xName_y1 = rep(1, 10)
)

dat$yName_y0 <- ifelse(dat$treatment == 1, rnorm(5, 140, 5), rnorm(5, 165, 5))
dat$yName_y1 <- ifelse(dat$treatment == 1, rnorm(5, 135, 5), rnorm(5, 165, 5))
.dataLines <- dat


readr::write_csv(.dataLines, '../_site/d3/pairing/data/line-data.csv')
readr::write_csv(.dataLines, 'd3/scrollytell/data/line-data.csv')
