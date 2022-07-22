#' Create a scrollytell
#'
#' @ description See mod_learn_scrolly_example.R for an example. Requires scrollytell.css and scrollytell.js
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
scroll_ui_text_section <- function(ns, order, ...){
  id <- ns(glue::glue('text-{order}'))
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

# TODO: known issue with the content initially displaying everything when there are multiple modules
#' @describeIn scroll_ui_container runs the JS. For use on the server side
activate_scrollytell <- function(ns){
  # this adds the javascript listener for the module
  moduleId <- ns(NULL)
  shinyjs::runjs(
    glue::glue(
      .open = '<<',
      .close = '>>',
      'scrolly.plotState<<moduleId>> = 1; ',
      '$(document).scroll(function() {
        scrolly.scroll("<<moduleId>>")
      }); '
    )
  )
}

