#' Create a scrollytell
#'
#' See mod_learn_test.R for an example. Requires scrollytell.css and scrollytell.js
#'
#' @description A fct function
#'
#' @return html for the UI
#'
#' @noRd
scroll_ui_container <- function(inputId, ns, ...){
  htmltools::div(
    id = glue::glue('{ns(inputId)}-scroll-container'),
    class = 'scroll-container',
    ...
  )
}

#' @describeIn scroll_ui_container
scroll_ui_text <- function(inputId, ns, ...){
  htmltools::div(
    id = glue::glue('{ns(inputId)}-scroll-text'),
    class = 'scroll-text',
    ...
  )
}

#' @describeIn scroll_ui_container
scroll_ui_text_section <- function(inputId, ns, ...){
  htmltools::div(
    id = glue::glue('{ns(inputId)}-scroll-text-section'),
    class = 'scroll-text-section',
    class = ns('scroll-text-section'),
    ...
  )
}

#' @describeIn scroll_ui_container
scroll_ui_visual <- function(outputId, ns){
  htmltools::div(
    id = glue::glue('{ns(outputId)}-scroll-visual'),
    class = 'scroll-visual',
    shiny::uiOutput(outputId = ns(outputId))
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

