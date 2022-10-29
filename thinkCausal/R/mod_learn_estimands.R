#' learn_estimands UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_estimands_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(
      class = 'learning-page',

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        includeMarkdown(app_sys("app", "www", "learn", "estimands", "markdowns", 'estimands_1.md')),
        br(),br(),br(),br(),br(),br(),
      ),

      div(
        class = 'scrollytell-container',

        # text content
        div(
          class = 'estimands-text-along-d3',
          includeMarkdown(app_sys("app", "www", "learn", "estimands", "markdowns", 'estimands_ate.md'))
        ),

        # d3js content
        div(id = 'estimands-plot-container', # TODO: should prefix the div ids
            class = 'estimands-d3-container',
            div(id = 'estimands-plot-ATE')
        )
      ),

      br(),br(),br(),br(),br(),

      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        br(),br(),br(), br(),
        includeMarkdown(app_sys("app", "www", "learn", "estimands", "markdowns", 'estimands_attatc.md')),
        br(),br(),br(),
        # the quiz UI
        mod_quiz_ui(id = ns('quiz')),
      ),
      br(),
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',
        # includeMarkdown(app_sys("app", "www", "learn", "estimands", "markdowns", 'estimands_related.md')),
        includeMarkdown(app_sys("app", "www", "learn", "estimands", "markdowns", 'estimands_citations.md'))
      )
    )
  )
}

#' learn_estimands Server Functions
#'
#' @noRd
mod_learn_estimands_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # run the quiz
    mod_quiz_server(
      id = "quiz", # this should always be quiz
      id_parent = module_ids$learn$estimands,
      question_texts = quiz_content_estimands$question_texts,
      question_prompts = quiz_content_estimands$question_prompts,
      correct_answers = quiz_content_estimands$correct_answers,
      message_correct = quiz_content_estimands$message_correct,
      message_wrong = quiz_content_estimands$message_wrong,
      message_skipped = quiz_content_estimands$message_skipped,
      embed_quiz = TRUE
    )


  })
}
