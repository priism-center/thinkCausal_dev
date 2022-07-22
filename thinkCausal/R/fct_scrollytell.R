#' Create a scrollytell
#'
#' @description See mod_learn_scrolly_example.R for an example. Requires scrollytell.css and scrollytell.js
#'
#' @return html for the UI
#'
#' @noRd
scroll_ui_container <- function(ns, ...){
  htmltools::div(
    id = glue::glue('{ns(NULL)}-scroll-container'),
    class = 'scroll-container',
    ...
  )
}

#' @describeIn scroll_ui_container
scroll_ui_text <- function(ns, ...){
  htmltools::div(
    id = glue::glue('{ns(NULL)}-scroll-text'),
    class = 'scroll-text',
    ...
  )
}

#' @describeIn scroll_ui_container
scroll_ui_text_section <- function(ns, position, ...){
  id <- ns(glue::glue('text-{position}'))
  htmltools::div(
    id = glue::glue('{id}-scroll-text-section'),
    class = 'scroll-text-section',
    class = ns('scroll-text-section'),
    ...
  )
}

#' @describeIn scroll_ui_container
scroll_ui_visual <- function(ns){
  id <- ns('scroll_visual')
  htmltools::div(
    id = glue::glue('{id}-container'),
    class = 'scroll-visual-container',
    shiny::uiOutput(outputId = id)
  )
}

#' @describeIn scroll_ui_container runs the JS. For use on the UI side
use_scrollytell <- function(ns){
  # this adds the javascript listener for the module
  moduleId <- ns(NULL)
  js_code <- glue::glue(
    .open = '<<',
    .close = '>>',
    'scrolly.plotState<<moduleId>> = 1; ',
    '$(document).scroll(function() {
      scrolly.scroll("<<moduleId>>")
    }); '
  )
  js_tag <- shiny::tags$script(htmltools::HTML(js_code))
  return(js_tag)
}
