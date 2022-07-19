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
        br(),br(),br(),
        includeMarkdown(app_sys("app", "www", "learn", "estimands", "markdowns", 'estimands_2.md')),
        br()
      ),

      # the quiz UI
      mod_quiz_ui(id = ns('quiz')),

      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        class = ns('learning-content-blur'), # required for blur
        class = 'learning-content-blur', # required for blur
        br(),
        includeMarkdown(app_sys("app", "www", "learn", "estimands", "markdowns", 'estimands_attatc.md')),
        br(),br(),br(),br(),br(),
        wellPanel(includeMarkdown(app_sys("app", "www", "learn", "estimands", "markdowns", 'estimands_related.md'))),
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
      question_texts = store_l_estimands$question_texts,
      question_prompts = store_l_estimands$question_prompts,
      correct_answers = store_l_estimands$correct_answers,
      message_correct = store_l_estimands$message_correct,
      message_wrong = store_l_estimands$message_wrong,
      message_skipped = store_l_estimands$message_skipped,
      embed_quiz = TRUE
    )

  })
}
