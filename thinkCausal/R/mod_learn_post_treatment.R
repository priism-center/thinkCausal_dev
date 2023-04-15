#' learn_post_treatment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_post_treatment_ui <- function(id){
  ns <- NS(id)
  tagList(
    use_scrollytell(ns = ns),

    div(
      class = 'learning-page',

      shiny::withMathJax(),

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content', # required
        style = 'display: block;',
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_1.md')),
        br(),
        br()
      ),

      scroll_ui_container(
        ns = ns,
        scroll_ui_text(
          ns = ns,
          scroll_ui_text_section(
            ns = ns,
            position = 1,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_2.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 2,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_3.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 3,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_4.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 4,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_5.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 5,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_6.md')),
          )
        ),
        scroll_ui_visual(ns = ns)
      ),

      div(
        class = ns('learning-content'), # required
        class = 'learning-content', # required
        style = 'display: block;',
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_7.md')),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_8.md')),
        # the quiz UI
        h2('Practice'),
        mod_quiz_ui(id = ns('quiz')),
        br(),
        br(),
        br(),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_citations.md')),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_learn_more.md'))
      )
    )

  )
}

#' learn_post_treatment Server Functions
#'
#' @noRd
mod_learn_post_treatment_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # run the quiz
    mod_quiz_server(
      id = "quiz", # this should always be quiz
      id_parent = module_ids$learn$post_treatment,
      question_texts = quiz_content_post_treatment$question_texts,
      question_prompts = quiz_content_post_treatment$question_prompts,
      correct_answers = quiz_content_post_treatment$correct_answers,
      graders = quiz_content_post_treatment$graders,
      message_correct = quiz_content_post_treatment$message_correct,
      message_wrong = quiz_content_post_treatment$message_wrong,
      message_skipped = quiz_content_post_treatment$message_skipped,
      embed_quiz = TRUE
    )

    output$scroll_visual <- renderUI({
      items <- list()

      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        renderImage({
          list(src = app_sys('app', 'www/learn/post-treatment/p1.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position2 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/post-treatment/p2.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position3 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/post-treatment/p3.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position4 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/post-treatment/p4.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      items$position5 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/post-treatment/p5.png'),
               contentType = 'image/png',
               width = 600,
               height = 500)
        }, deleteFile = F)
      )

      # browser()
      return(items)
    })


  })
}

## To be copied in the UI
# mod_learn_post_treatment_ui("learn_post_treatment_1")

## To be copied in the server
# mod_learn_post_treatment_server("learn_post_treatment_1")
