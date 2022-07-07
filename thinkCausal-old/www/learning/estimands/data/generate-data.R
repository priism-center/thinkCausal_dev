# data for scatter plot

library(dplyr)

n <- 10
y0 <- c(155, 142, 153, 147, 154, 145, 151, 148, 149, 143)
y1 <- c(153, 136, 155, 139, 154, 135, 152, 138, 148, 137)

z <- rep(0:1, n)
# factual <- sort(rep(0:1, n))
pair_id <- rep(seq_len(n), 2)

.data <- data.frame(
  xName = sort(rep(0:1, n)), #jitter(sort(rep(0:1, n))),
  yName = c(y0, y1),
  y = c(rep("y0", n), rep("y1", n)),
  treatment = z,
  #factual = c(rep(1, 5), rep(0, 5), rep(0, 5), rep(1, 5)),
  factual = as.integer(z == sort(rep(0:1, n))),
  pair_id = pair_id
)

ggplot2::ggplot(
  .data, 
  ggplot2::aes(x = xName, y = yName, color = as.factor(treatment), 
               shape = as.factor(factual), group = factual
  )) + 
  ggplot2::geom_point()

# readr::write_csv(.data, '../_site/d3/pairing/data/point-data.csv')
 readr::write_csv(.data, 'd3/scrollytell/data/point-data.csv')



.dataLines <- .data %>%
  select(xName, yName, y, pair_id, treatment) %>% 
  tidyr::pivot_wider(names_from = y, values_from = c(xName, yName))
readr::write_csv(.dataLines, 'd3/scrollytell/data/line-data.csv')

