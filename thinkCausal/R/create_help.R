
#' Create a link to the help section
#'
#' Creates a link to open the help slide-over to the header provided. Defaults to returning a button to be pressed.
#'
#' @param header The h3 header text within help.md to jump to
#' @param button_label Label text to included on the button
#' @param type one of c('button', 'run_js', 'js_code'). 'button' returns the html for a button. 'js_code' returns just the string of JavaScript code. 'run_js' runs the JavaScript.
#'
#' @return
#' @export
#'
#' @examples
#' # within shiny UI
#' create_link_to_help('Data')
#' 
#' # within shiny server
#' create_link_to_help('Data', type = 'run_js')
create_link_to_help <- function(header, button_label = 'Help me', type = c('button', 'run_js', 'js_code')){
  
  type <- match.arg(type)
  
  # create js code
  # cleaning should match cleaning in helpSlideOver.js
  header_cleaned <- base::tolower(header)
  header_cleaned <- stringr::str_remove_all(header_cleaned, " ")
  id <- glue::glue("help-{header_cleaned}")
  js_code <- glue::glue("openHelpSection('{id}')")
  
  if (type == 'js_code') return(js_code)
  if (type == 'run_js') return(shinyjs::runjs(js_code))
  
  button <- tags$button(type = 'button',
                        class = 'btn btn-default help',
                        onclick = js_code,
                        button_label)
  
  return(button)
}



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
