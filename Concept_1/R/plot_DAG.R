
#' Plot a simple DAG
#'
#' @param Z_col string
#' @param Y_col string
#' @param X_cols string or list of strings
#'
#' @return ggplot object
#' @export
plot_DAG <- function(Z_col, Y_col, X_cols){

  validate_Z <- is.character(Z_col) & length(Z_col) == 1
  validate_Y <- is.character(Y_col) & length(Y_col) == 1
  validate_X <- is.character(Y_col)
  if (isFALSE(all(c(validate_Z, validate_Y, validate_X)))) stop("Z, Y, X must be characters. Z and Y must be length 1")
  
  X_cols_label <- paste0(X_cols, collapse = '\n')
  
  font_size <- min(4, (1 / length(X_cols)) * 90)
  height_rectangle <- min(1.5, ((length(X_cols) / 10) * 0.85))
  height_min_max <- c(0 - (height_rectangle/2), 0 + (height_rectangle/2))
  coordinates_rectangle <- tibble(
    xmin = -0.90,
    xmax = -0.60,
    ymin = height_min_max[1],
    ymax = height_min_max[2]
  )
  
  coordinates_circles <- tibble(
    x = c(0, 0.75),
    y = c(0, 0)
  )
  
  coordinates_arrows <- tibble(
    x = c(-0.50, 0.25), 
    xend = c(-0.25, 0.50),
    y = c(0, 0), 
    yend = c(0, 0)
  )
  
  coordinates_curve <- tibble(
    x = -0.50,
    xend = 0.50,
    y = 0.25, 
    yend = 0.25
  )
  
  coordinates_labels <- tibble(
    label = c(Z_col, Y_col),
    x = c(0, 0.75),
    y = c(0, 0)
  )
  
  coordinates_labels_X <- tibble(
    label = X_cols_label,
    x = -0.75,
    y = 0
  )
  
  
  p <- ggplot() +
    geom_segment(data = coordinates_arrows,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 lineend = 'round', linejoin = 'mitre',
                 size = 1.2, color = 'grey30',
                 arrow = arrow(length = unit(0.04, "npc"))) +
    geom_curve(data = coordinates_curve,
               aes(x = x, xend = xend, y = y, yend = yend),
               lineend = 'round', 
               size = 1.2, color = 'grey30',
               curvature = -0.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
    geom_point(data = coordinates_circles,
               aes(x = x, y = y),
               shape = 21, size = 40, stroke = 1,
               color = 'grey30', fill = NA) +
    geom_rect(data = coordinates_rectangle,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              color = 'grey30', fill = NA) +
    geom_text(data = coordinates_labels,
              aes(x = x, y = y, label = label),
              size = 4) +
    geom_text(data = coordinates_labels_X,
              aes(x = x, y = y, label = label),
              size = font_size) +
    coord_cartesian(xlim = c(-1, 1), ylim = c(-0.75, 0.75)) +
    theme_void()
  
  return(p)
}
