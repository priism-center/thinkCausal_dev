#' rct_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_obs_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(

    # shinyjs::useShinyjs(),
    use_scrollytell(ns = ns),

    div(
      class = 'learning-page',

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',

        h1('Analyzing Data from Observational Studies'),
        includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis1.md')),
        br(),br()
      ),

      scroll_ui_container(
        ns = ns,
        scroll_ui_text(
          ns = ns,
          scroll_ui_text_section(
            ns = ns,
            position = 1,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis2.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 2,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis2.2.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 3,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis3.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 4,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis4.md')),

          ),
          scroll_ui_text_section(
            ns = ns,
            position = 5,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis5.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 6,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis6.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 7,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis7.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 8,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis8.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 9,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis9.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 10,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis10.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 11,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis11.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 12,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis12.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 13,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis13.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 14,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis14.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 15,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis15.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 16,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis16.md')),
          )
        ),
        scroll_ui_visual(ns = ns)
      ),
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',
        includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational_learn_more.md')),
        includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational_citations.md'))
      )
    )
  )
}


#' learn_scrolly_example Server Functions
#'
#' @noRd
mod_learn_obs_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$scroll_visual <- renderUI({

      items <- list()

      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p1.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position2 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p2.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position3 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p3.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position4 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p4.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position5 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p5.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position6 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p6.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position7 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p7.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position8 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p8.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position9 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p9.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position10 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p10.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position11 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p11.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position12 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p12.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position13 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p13.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position14 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p14.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position15 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p15.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position16 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/observational-analysis/plots/p16.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )
      return(items)
    })
  })
}

