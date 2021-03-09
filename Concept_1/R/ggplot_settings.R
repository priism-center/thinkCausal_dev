
# build custom theme
theme_custom <- function()
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(
      fill = "gray95",
      color = 'white'),
    strip.text = ggplot2::element_text(
      color = "gray30",
      size = 11,
      face = "bold"
    ),
    text = ggplot2::element_text(family = "Helvetica",
                        color = "gray30"),
    legend.position = "bottom",
    plot.caption = ggplot2::element_text(face = "italic",
                                size = 8,
                                color = 'grey50')
  )

# set custom theme
# should allow the user to also choose theme_bw() for publication plots
ggplot2::theme_set(theme_custom())

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

# set default discrete colors
scale_colour_discrete <- function(...) {
  ggplot2::scale_color_viridis(..., discrete = TRUE)
}
scale_color_discrete <- function(...) {
  ggplot2::scale_color_viridis(..., discrete = TRUE)
}
scale_fill_discrete <- function(...) {
  ggplot2::scale_fill_viridis(..., discrete = TRUE)
}
