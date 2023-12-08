#' learn_estimands2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_learn_estimands2_ui<- function(id){
  ns <- NS(id)
  tagList(
    use_scrollytell(ns = ns),
    use_scrollytell2(ns = ns),
    div(
      class = 'learning-page',

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',
        includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'intro.md')),
        br(),br(),br(),br()
      ),

      scroll_ui_container(
        ns = ns,
        scroll_ui_text(
          ns = ns,
          scroll_ui_text_section(
            ns = ns,
            position = 1,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section1.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 2,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section2.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 3,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section3.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 4,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section4.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 5,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section5.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 6,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section6.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 7,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section7.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 8,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section8.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 9,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section9.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 10,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section10.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 11,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section11.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 12,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section12.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 13,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section13.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 14,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section14.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 15,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section15.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 16,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section16.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 17,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section17.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 18,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section18.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 19,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section19.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 20,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section20.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 21,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section21.md'))
          ),
          scroll_ui_quiz_section(
            ns = ns,
            position = 22,
            div(
              mod_quiz_ui(id = ns('quiz')),
              br(),
              br(),
              br()
              #includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_citations.md')),
              #includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_learn_more.md'))
            )
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 23,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section1.md'))
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 24,
            includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section2.md'))
          ),

        ),
        scroll_ui_visual(ns = ns, clickable = T)
      )
      #, scroll_ui_container2(
      #   ns = ns,
      #   scroll_ui_text(
      #     ns = ns,
      #     scroll_ui_text_section(
      #       ns = ns,
      #       position = 22,
      #       includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section1.md'))
      #     ),
      #     scroll_ui_text_section(
      #       ns = ns,
      #       position = 23,
      #       includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section2.md'))
      #     ),
      #     scroll_ui_text_section(
      #       ns = ns,
      #       position = 24,
      #       includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'section3.md'))
      #     )
      #   ),
      #   scroll_ui_visual2(ns = ns)
      # )

    )
  )
}

#' learn_estimands2 Server Functions
#'
#' @noRd
#'
mod_learn_estimands2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # run the quiz
    shinyQuiz::quiz_server(quiz_content_estimands2)

    # mod_quiz_server(
    #   id = "quiz", # this should always be quiz
    #   id_parent = module_ids$learn$estimands2,
    #   questions = quiz_content_estimands2$questions,
    #   # question_texts = quiz_content_post_treatment$question_texts,
    #   # question_prompts = quiz_content_post_treatment$question_prompts,
    #   # correct_answers = quiz_content_post_treatment$correct_answers,
    #   # graders = quiz_content_post_treatment$graders,
    #   message_correct = quiz_content_estimands2$message_correct,
    #   message_wrong = quiz_content_estimands2$message_wrong,
    #   message_skipped = quiz_content_estimands2$message_skipped,
    #   embed_quiz = TRUE,
    #   sandbox_mode = FALSE # TODO: test
    # )
    # mod_quiz_server(
    #   id = "quiz", # this should always be quiz
    #   id_parent = module_ids$learn$estimands2,
    #   question_texts = quiz_content_estimands2$question_texts,
    #   question_prompts = quiz_content_estimands2$question_prompts,
    #   correct_answers = quiz_content_estimands2$correct_answers,
    #   message_correct = quiz_content_estimands2$message_correct,
    #   message_wrong = quiz_content_estimands2$message_wrong,
    #   message_skipped = quiz_content_estimands2$message_skipped,
    #   embed_quiz = TRUE,
    #   sandbox_mode = TRUE #TODO: change to false. Currently just for testing
    # )

    output$scroll_visual <- renderUI({
      items <- list()
      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p1.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 2
      items$position2 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p2.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 3
      items$position3 <- div(
        style = 'visibility: hidden;',
        # TODO: note this is not clickable b/c scroll_ui_visual(clickable = FALSE)
        reactable::renderReactable({
          readr::read_csv('inst/extdata/estimands2_table1.csv') %>%
            reactable::reactable()
        })
      )

      # item 4
      items$position4 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p3.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 5
      items$position5 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p4.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 6
      items$position6 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p5.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 7
      items$position7 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p6.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 8
      items$position8 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p7.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 9
      items$position9 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_csv('inst/extdata/estimands2_table2.csv') %>%
            reactable::reactable()
        })
      )

      # item 10
      items$position10 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p10.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 11
      items$position11 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p11.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 12
      items$position12 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p12.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 13
      items$position13 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p13.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 14
      items$position14 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p14.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 15
      items$position15 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p15.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 16
      items$position16 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p16.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 17
      items$position17 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p17.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 18
      items$position18 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p18.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 19
      items$position19 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p19.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 20
      items$position20 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_csv('inst/extdata/estimands2_table3.csv') %>%
            reactable::reactable()
        })
      )

      # item 21
      items$position21 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p15.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 22
      items$position22 <- div(
        style = 'visibility: hidden;',
        NULL # b/c this is a quiz chunk
      )

      # item 21
      items$position23 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/no_overlap.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      # item 21
      items$position24 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/estimands2/plots/p2.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )


      return(items)
    })

    # output$scroll_visual2 <- renderUI({
    #   items <- list()
    #   # item 1
    #   items$position22 <- div(
    #     style = 'visibility: hidden;',
    #     renderImage({
    #       list(src = app_sys('app', 'www/learn/estimands2/plots/p1.png'),
    #            contentType = 'image/png',
    #            width = 800,
    #            height = 500)
    #     }, deleteFile = F)
    #   )
    #
    #   # item 2
    #   items$position23 <- div(
    #     style = 'visibility: hidden;',
    #     renderImage({
    #       list(src = app_sys('app', 'www/learn/estimands2/plots/p2.png'),
    #            contentType = 'image/png',
    #            width = 800,
    #            height = 500)
    #     }, deleteFile = F)
    #   )
    #
    #   # item 3
    #   items$position24 <- div(
    #     style = 'visibility: hidden;',
    #     reactable::renderReactable({
    #       readr::read_csv('inst/extdata/estimands2_table1.csv') %>%
    #         reactable::reactable()
    #     })
    #   )
    #
    #   return(items)
    #
    # })

  })
}

