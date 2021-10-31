#' Create a progress bar
#' 
#' Create a bootstrap HTML progress bar filled. `progress` specifies the fill percentage of the bar.
#'
#' @param progress integer between 0 and 100 representing the fill percentage
#' @author Joe Marlo
#'
#' @return HTML
#' @export
#'
#'
#' @examples
#' create_progress_bar(80)
create_progress_bar <- function(progress) {
  # returns the html to create a bootstrap progress bar filled to the progress amount
  #https://getbootstrap.com/docs/4.4/components/progress/
  tags$div(
    class = 'progress',
    tags$div(
      class = "progress-bar striped",
      role = "progressbar",
      style = paste0("width: ", progress, "%;"),
      'aria-valuenow' = as.character(progress),
      'aria-valuemin' = "0",
      'aria-valuemax' = "100",
      paste0(round(progress, 0), '%')
    )
  )
}
