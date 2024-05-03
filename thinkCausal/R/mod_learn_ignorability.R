#' learn_ignorability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_learn_ignorability_ui <- function(id){
  ns <- NS(id)

  tagList(
    use_scrollytell(ns = ns),
    div(
      class = 'learning-page',

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',
        includeMarkdown(app_sys("app", "www", "learn", "ignorability", "markdowns", 'intro.md')),
        br(),br(),br(),br()
      ),

      scroll_ui_container(
        ns = ns,
        scroll_ui_text(
          ns = ns,
          scroll_ui_text_section(
            ns = ns,
            position = 1,
            includeMarkdown(app_sys("app", "www", "learn", "ignorability", "markdowns", 'section1.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 2,
            includeMarkdown(app_sys("app", "www", "learn", "ignorability", "markdowns", 'section2.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 3,
            includeMarkdown(app_sys("app", "www", "learn", "ignorability", "markdowns", 'section3.md'))
          ),
        ),
        scroll_ui_visual(ns = ns, clickable = T)
      )
    )
  )
}

#' learn_ignorability Server Functions
#'
#' @noRd
#'
mod_learn_ignorability_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$scroll_visual <- renderUI({
      items <- list()

      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        renderImage({
          list(src = app_sys('app', 'www/learn/ignorability/img/p1.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 2
      items$position2 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/ignorability/img/p2.rds'))
        })
      )


      return(items)
    })


  })
}


