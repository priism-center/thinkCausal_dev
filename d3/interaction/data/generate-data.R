# data for scatter plot
# this is just placeholder data for testing
# TODO: replace data with proper sim

scale_bt <- function(x, start, end) ((end - start) * (x - min(x))) / (max(x) - min(x))

n <- 100
x_z0 <- sort(rnorm(n, 9, 3))
x_z1 <- sort(rnorm(n, 13, 4))
y_z0 <- rnorm(n, 10, 3) + scale_bt(x_z0, -1, 1) * 1.5
y_z1 <- rnorm(n, 18, 4) + scale_bt(y_z0, -1, 1) * 1.5
z <- sort(rep(0:1, n))

.data <- data.frame(
    xName = c(x_z0, x_z1),
    yName = c(y_z0, y_z1),
    treatment = z
)
ggplot2::ggplot(
  .data, 
  ggplot2::aes(x = xName, y = yName, color = as.factor(treatment), group = treatment)) + 
  ggplot2::geom_point()

readr::write_csv(.data, '~/Desktop/point-data.csv')# '../student_work/Joe/d3/animations/data/point-data.csv')

.reg <- data.frame(
  x0Name = x_z0,
  x1Name = x_z1,
  y0Name = lm(y_z0 ~ x_z0)$fitted.values,
  y1Name = lm(y_z1 ~ x_z1)$fitted.values
)

readr::write_csv(.reg, '~/Desktop/line-data.csv')# '../student_work/Joe/d3/animations/data/line-data.csv')
