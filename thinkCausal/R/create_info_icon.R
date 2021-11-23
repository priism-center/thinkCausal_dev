#' Create hover-able information icon
#'
#' Returns HTML code to create a little (i) icon that shows text when you hover. 
#'
#' @param label any text to display next to the icon
#' @param text text to display when hovering over the icon
#'
#' @author Joe Marlo
#'
#' @return
#' @export
#'
#' @examples
#' # within shiny UI pages
#' create_info_icon('Average Treatment Effect', 'The ATE is ...')
create_info_icon <- function(label, text){
  html <- paste0(
    '<div class="infoToolTip">',
    label, 
    ' <a>&#9432;</a>',
    '<span class="infoToolTipText">',
    text,
    '</span></div>'
  )
  
  html <- HTML(html)
  return(html)
}
