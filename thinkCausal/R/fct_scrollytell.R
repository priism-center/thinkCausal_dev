#' scrollytell
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

scroll_ui_container <- function(inputId, ...){
  htmltools::div(
    id = glue::glue('scroll-container-{inputId}'),
    class = 'scroll-container',
    ...
  )
}

scroll_ui_text <- function(inputId, ...){
  htmltools::div(
    id = glue::glue('scroll-text-{inputId}'),
    class = 'scroll-text',
    ...
  )
}

scroll_ui_text_section <- function(inputId, ...){
  htmltools::div(
    id = glue::glue('scroll-text-section-{inputId}'),
    class = 'scroll-text-section',
    ...
  )
}

scroll_ui_visual <- function(outputId){
  htmltools::div(
    id = glue::glue('scroll-visual-{outputId}'),
    class = 'scroll-visual',
    shiny::htmlOutput(outputId = outputId)
  )
}

# scroll_server_sticky_content <- function(outputId, width = "100%", height = NULL){
#   htmlwidgets::shinyWidgetOutput(
#     outputId = outputId,
#     name = 'scrollytell',
#     width = width,
#     height = height
#   )
# }

# scroll_render <- function(...){
#   shiny::renderUI(...)
# }

# scroll_render <- function(expr, env = parent.frame()){
#
#   htmlwidgets::shinyRenderWidget(
#     expr = expr,
#     outputFunction = scroll_output,
#     env = env,
#     quoted = FALSE
#   )
# }
