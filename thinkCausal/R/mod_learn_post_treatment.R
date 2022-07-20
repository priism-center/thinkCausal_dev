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

    div(
      class = 'learning-page',

      # the quiz UI
      mod_quiz_ui(id = ns('quiz')),

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content', # required
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_1.md')),
        br(),
        radioButtons(inputId = ns('include_pt'),
                     label = h4('Control for post-treatment variable bugs?'),
                     choices = c('Do not control for bugs', 'Control for bugs'),
                     selected = 'Do not control for bugs',
                     inline = TRUE
        ),
        plotOutput(outputId = ns('posttreatment_plot'), height = 500),
        wellPanel(
          conditionalPanel(
            condition = "input.include_pt == 'Do not control for bugs'",
            ns = ns,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_2.md'))
          ),
          conditionalPanel(
            condition = "input.include_pt == 'Control for bugs'",
            ns = ns,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_3.md'))
          )
        ),
        br(),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_4.md')),
        br(),
        plotOutput(ns('measured')),
        plotOutput(ns('observed')),
        br(), br(),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_5.md')),
        br(),
        wellPanel(includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_related.md'))),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_citations.md'))
      )
    )

  )
}

#' learn_post_treatment Server Functions
#'
#' @noRd
mod_learn_post_treatment_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # run the quiz
    mod_quiz_server(
      id = "quiz", # this should always be quiz
      id_parent = module_ids$learn$post_treatment,
      question_texts = quiz_content_post_treatment$question_texts,
      question_prompts = quiz_content_post_treatment$question_prompts,
      correct_answers = quiz_content_post_treatment$correct_answers,
      message_correct = quiz_content_post_treatment$message_correct,
      message_wrong = quiz_content_post_treatment$message_wrong,
      message_skipped = quiz_content_post_treatment$message_skipped,
      embed_quiz = FALSE
    )

    # TODO: plots
    # data: https://github.com/ThinkR-open/golem/issues/127


  })
}

## To be copied in the UI
# mod_learn_post_treatment_ui("learn_post_treatment_1")

## To be copied in the server
# mod_learn_post_treatment_server("learn_post_treatment_1")
