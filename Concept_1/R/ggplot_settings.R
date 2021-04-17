# default theme is set on load by input$settings_options_ggplot_theme

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

# set default discrete colors
scale_colour_discrete <- function(...) {
  viridis::scale_color_viridis(..., discrete = TRUE)
}
scale_color_discrete <- function(...) {
  viridis::scale_color_viridis(..., discrete = TRUE)
}
scale_fill_discrete <- function(...) {
  viridis::scale_fill_viridis(..., discrete = TRUE)
}
