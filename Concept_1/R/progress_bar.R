progress_bar <- function(progress) {
  # returns the html to create a bootsrap progress bar filled to the progress amount
  # TODO: change fill color
  #https://getbootstrap.com/docs/4.4/components/progress/
  tags$div(
    class = 'progress',
    tags$div(
      class = "progress-bar progress-bar-striped progress-bar-animated bg-info",
      role = "progressbar",
      style = paste0("width: ", progress, "%"),
      'aria-valuenow' = as.character(progress),
      'aria-valuemin' = "0",
      'aria-valuemax' = "100"
    )
  )
}
