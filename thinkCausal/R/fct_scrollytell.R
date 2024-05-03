#' @title Create a scrollytell
#'
#' @description See mod_learn_scrolly_example.R for an example. Requires scrollytell.css and scrollytell.js. This custom implementation is necessary because we need a R-based API (re: non JS) and current R packages do not support modules.
#'
#' @return html for the UI
#' @author Joe Marlo
#'
#' @noRd
scroll_ui_container <- function(ns, ...){
  htmltools::div(
    id = glue::glue('{ns(NULL)}scroll-container'),
    class = 'scroll-container',
    ...
  )
}

#' @noRd
#' @describeIn scroll_ui_container UI container for the text sections
scroll_ui_text <- function(ns, ...){
  htmltools::div(
    id = glue::glue('{ns(NULL)}scroll-text'),
    class = 'scroll-text',
    ...
  )
}


#' @noRd
#' @describeIn scroll_ui_container UI for establishing an individual scrollytell text section
scroll_ui_text_section <- function(ns, position, ...){
  id <- ns(glue::glue('text-{position}'))
  htmltools::div(
    id = glue::glue('{id}-scroll-text-section'),
    class = 'scroll-text-section',
    class = ns('scroll-text-section'),
    style = 'display: block;',
    ...
  )
}

#' @noRd
#' @describeIn scroll_ui_container UI for establishing an individual scrollytell text section
scroll_ui_text_wide_section <- function(ns, position, ...){
  id <- ns(glue::glue('text-{position}'))
  htmltools::div(
    id = glue::glue('{id}-scroll-text-wide-section'),
    class = 'learning-content',
    class = ns('learning-content'),
    ...
  )
}

#' @noRd
#' @describeIn scroll_ui_container UI for establishing an individual scrollytell quiz section
#' @description This requires the ui visual container to have style="pointer-events: none"; otherwise it will prevent clicks on the right half of the quiz.
scroll_ui_quiz_section <- function(ns, position, ...){
  id <- ns(glue::glue('text-{position}'))
  htmltools::div(
    id = glue::glue('{id}-scroll-text-section'),
    class = 'scroll-text-section',
    class = ns('scroll-text-section'),
    style = 'width: 300%',
    ...
  )
}

#' @noRd
#' @describeIn scroll_ui_container UI output for the visual (plots, tables, etc.)
#' @description clickable = FALSE is neccessary for when there is a quiz in a UI text section
scroll_ui_visual <- function(ns, clickable = TRUE){
  id <- ns('scroll_visual')
  htmltools::div(
    id = glue::glue('{id}-container'),
    class = 'scroll-visual-container',
    style = ifelse(isFALSE(clickable), "pointer-events: none", ''),
    shiny::uiOutput(outputId = id)
  )
}

#' @noRd
#' @describeIn scroll_ui_container runs the JS. For use on the UI side
use_scrollytell <- function(ns){
  # this adds the javascript listener for the module
  moduleId <- ns(NULL)
  js_code <- glue::glue(
    .open = '<<',
    .close = '>>',
    'scrolly.plotState.<<moduleId>> = 1; ',
    '$(document).scroll(function() {
      scrolly.scroll("<<moduleId>>")
    }); '
  )
  js_tag <- shiny::tags$script(htmltools::HTML(js_code))
  return(js_tag)
}

#' @title Create a secound scrollytell
#'
#' @description See mod_learn_scrolly_example.R for an example. Requires scrollytell.css and scrollytell.js. This custom implementation is necessary because we need a R-based API (re: non JS) and current R packages do not support modules.
#'
#' @return html for the UI
#' @author Joseph Marlo, George Perrett
#'
#' @noRd
scroll_ui_container2 <- function(ns, ...){
  htmltools::div(
    id = glue::glue('{ns(NULL)}scroll-container2'),
    class = 'scroll-container',
    ...
  )
}

#' @noRd
#' @describeIn scroll_ui_container UI output for the visual (plots, tables, etc.)
scroll_ui_visual2 <- function(ns){
  id <- ns('scroll_visual2')
  htmltools::div(
    id = glue::glue('{id}-container'),
    class = 'scroll-visual-container',
    shiny::uiOutput(outputId = id)
  )
}

#' @noRd
#' @describeIn scroll_ui_container runs the JS. For use on the UI side
use_scrollytell2 <- function(ns){
  # this adds the javascript listener for the module
  moduleId <- ns(NULL)
  js_code <- glue::glue(
    .open = '<<',
    .close = '>>',
    'scrolly.plotState.<<moduleId>> = 1; ',
    '$(document).scroll(function() {
      scrolly.scroll("<<moduleId>>")
    }); '
  )
  js_tag <- shiny::tags$script(htmltools::HTML(js_code))
  return(js_tag)
}

