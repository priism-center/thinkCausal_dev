# default theme is set on load by input$settings_options_ggplot_theme

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = function() ggplot2::scale_colour_viridis_d(),
  ggplot2.discrete.fill = function() ggplot2::scale_fill_viridis_d()
)

#' ggplot2 minimal theme without transparent background
#'
#' This mimics ggplot2::theme_minimal but excludes the transparent background so downloaded plots are legible.
#'
#' @param base_size See ggplot2::theme_bw
#' @param base_family See ggplot2::theme_bw
#' @param base_line_size See ggplot2::theme_bw
#' @param base_rect_size See ggplot2::theme_bw
#'
#' @author Joe Marlo
#'
#' @return ggplot2 theme
#' @export
#'
#' @import ggplot2
#'
#' @noRd
theme_minimal_no_transparency <- function(base_size = 11, base_family =  "", base_line_size = base_size/22, base_rect_size = base_size/22){
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(axis.ticks = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          # plot.background = element_blank(),
          complete = TRUE)
}
