# default theme is set on load by input$settings_options_ggplot_theme

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = function() ggplot2::scale_colour_viridis_d(),
  ggplot2.discrete.fill = function() ggplot2::scale_fill_viridis_d()
)
