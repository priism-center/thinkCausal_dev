#' Add an blurry overlay and message to user
#'
#' Blur out an element and add a loading message. This makes it obvious to the user that an element is updating.
#'
#' @param div div ID of the parent
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # shiny server
#' output$analysis_plot_overlap_plot <- renderPlot({
#'   
#'   # add overlay
#'   show_message_updating('analysis_plot_overlap_plot')
#'   
#'   # build plot
#'   p <- overlap_plot()
#'   
#'   # remove overlay
#'   close_message_updating('analysis_plot_overlap_plot')
#'   
#'   return(p)
#' })
#' }
show_message_updating <- function(div){
  # this adds a gray, blurry overlay and a message to the user notifying it is updating
  
  # add overlay and show message
  js_disable_div(div)
  show_message('Updating...', id = paste0(div, '-message'), closeButton = FALSE)
}

close_message_updating <- function(div){
  # remove overlay and close message
  Sys.sleep(0.5)
  js_enable_div(div)
  close_message(paste0(div, '-message'))
}

show_message <- function(content, id = NULL, closeButton = TRUE){
  # this is a smaller message that doesn't take over the whole screen
  # style and position controlled in CSS with class .shiny-notification
  shiny::showNotification(
    ui = content,
    duration = NULL,
    closeButton = closeButton,
    id = id,
    type = 'default')
}

close_message <- function(id){
  shiny::removeNotification(id = id)
}

js_disable_div <- function(div){
  # turn off pointer events
  pointer <- paste0('document.getElementById("', div, '").style.pointerEvents = "none";')
  
  # add transparent gray overlay div
  transparency <- paste0("$('#", div, "').wrap(\"<div class='dimmed'></div>\");")
  
  # run js code
  js_code <- paste0(pointer, transparency, collapse = '')
  shinyjs::runjs(js_code)
}

js_enable_div <- function(div){
  # turn on pointer events
  pointer <- paste0('document.getElementById("', div, '").style.pointerEvents = "auto";')
  
  # remove transparent gray overlay div
  transparency <- paste0("$('#", div, "').parent().replaceWith($('#", div, "'));")
  
  # run js code
  js_code <- paste0(pointer, transparency, collapse = '')
  shinyjs::runjs(js_code)
}
