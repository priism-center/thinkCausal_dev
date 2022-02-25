# this defines the potential_outcomes module
# this script was created by `create_learning_module()`
# the article name is 'Potential outcomes' and the short name is 'potential_outcomes'

require(shiny)
# require(tidyr)
# require(tibble)
# require(ggplot2)


# objects -----------------------------------------------------------------

# list to store all post-treatment objects in
store_l_potential_outcomes <- list()

# set path
store_l_potential_outcomes$path_to_here <- file.path('modules', 'learning', 'potential-outcomes')

# read in randomization df
# store_l_potential_outcomes$potential_outcomes_df <- readr::read_csv(
#   file.path(store_l_potential_outcomes$path_to_here, 'data', 'potential_outcomes_df.csv'),
#   col_types = readr::cols(
#     h1 = readr::col_double(),
#     z = readr::col_logical(),
#     bugs = readr::col_logical()
#   )
# )
# store_l_potential_outcomes$potential_outcomes_df$z_as_text <- ifelse(store_l_potential_outcomes$potential_outcomes_df$z, 'Treatment', 'Control')


# namespace ---------------------------------------------------------------

# pay close attention to how the namespace is managed. Since the quiz module
# is inside the post-treatment module, its namespace is a 'child' of the post-treatment
# module. The post-treatment module namespace id is set within module_ids object in global.R.
# The quiz module id is always 'quiz' which then becomes 'learning_post_treatment-quiz'
# UI and server elements outside of the quiz can be treated as you would normal modules

# ns id is set as 'learning_post_treatment' per module_ids$learning$post_treatment

# set namespace for UI quiz elements
store_l_potential_outcomes$ns_quiz <- NS(NS(module_ids$learning$potential_outcomes)('quiz'))


# source quiz and R functions ---------------------------------------------

purrr::walk(list.files(file.path(store_l_potential_outcomes$path_to_here, 'R'), full.names = TRUE), source)


# UI ----------------------------------------------------------------------

ui_learning_potential_outcomes <- function(id) {
  ns <- NS(id)
  tagList(
    ui <- fluidPage(
      
      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes_1.md')),
        br(),
        # radioButtons(inputId = ns('include_pt'),
        #              label = 'Include post-treatment variable bugs?',
        #              choices = c('Include bugs', 'Do not include bugs'),
        #              selected = 'Do not include bugs'),
        # plotOutput(outputId = ns('posttreatment_plot'), 
        #            height = 500),
        br()
      ),
      
      # the quiz UI
      # can be moved within the UI; recommended to place at top of UI if not embedding
      ui_quiz(id = ns('quiz')), # comment out this line if quiz is not required
      
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        class = ns('learning-content-blur'), # required for blur (comment out this line if blur is not required)
        class = 'learning-content-blur', # required for blur (comment out this line remove if blur is not required)
        br(),
        includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes_2.md')),
        br(),
        wellPanel(includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes_related.md'))),
        includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes_citations.md'))
      )
    )
  )
}


# server ------------------------------------------------------------------

server_learning_potential_outcomes <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # run the quiz
      server_quiz(
        id = "quiz", # this should always be quiz
        id_parent = module_ids$learning$potential_outcomes,
        question_texts = store_l_potential_outcomes$quiz()$question_texts,
        question_prompts = store_l_potential_outcomes$quiz()$question_prompts,
        correct_answers = store_l_potential_outcomes$quiz()$correct_answers,
        message_correct = store_l_potential_outcomes$quiz()$message_correct,
        message_wrong = store_l_potential_outcomes$quiz()$message_wrong,
        message_skipped = store_l_potential_outcomes$quiz()$message_skipped,
        embed_quiz = TRUE
      )
      
      # example plot
      # output$posttreatment_plot <- renderPlot({
      # 
      #   if (input$include_pt == 'Include bugs') {
      #     
      #   }
      # 
      #   # add theme
      #   p <- p + plot_theme()
      # 
      #   return(p)
      # })
    }
  )
}


# clean up ----------------------------------------------------------------


