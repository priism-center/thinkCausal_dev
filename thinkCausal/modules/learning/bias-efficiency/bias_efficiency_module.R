# this defines the bias-efficiency learning module under Learning

require(shiny)
require(tidyr)
require(tibble)
require(ggplot2)
require(DT)
set.seed(44)


# objects -----------------------------------------------------------------

# list to store all bias-efficiency objects in
store_l_bias_efficiency <- list()

# set path
store_l_bias_efficiency$path_to_here <- file.path('modules', 'learning', 'bias-efficiency')

# read in plants df
# store_l_bias_efficiency$plants_df <- readr::read_csv(
#   file.path(store_l_bias_efficiency$path_to_here, 'data', 'plants_df.csv'),
#   col_types = readr::cols(
#     height = readr::col_double(),
#     h1 = readr::col_double(),
#     h0 = readr::col_double(),
#     bugs = readr::col_logical(),
#     bugs1 = readr::col_logical(),
#     bugs0 = readr::col_logical(),
#     pest_control = readr::col_logical()
#   )
# )


# add vars for plotting
# store_l_bias_efficiency$plants_df <- store_l_bias_efficiency$plants_df %>% 
#   mutate(pest_control_as_jitter = pest_control + runif(n(), -0.25, 0.25),
#          pest_control_as_text = ifelse(pest_control, 'Pest control', 'No pest control'),
#          bugs_as_text = ifelse(bugs, 'Has bugs', 'No bugs'))


# namespace ---------------------------------------------------------------

# pay close attention to how the namespace is managed. Since the quiz module
# is inside the post-treatment module, its namespace is a 'child' of the post-treatment
# module. The post-treatment module namespace id is set within module_ids object in global.R.
# The quiz module id is always 'quiz' which then becomes 'learning_post_treatment-quiz'
# UI and server elements outside of the quiz can be treated as you would normal modules

# ns id is set as 'learning_post_treatment' per module_ids$learning$post_treatment

# set namespace for UI quiz elements
store_l_bias_efficiency$ns_quiz <- NS(NS(module_ids$learning$bias_efficiency)('quiz'))


# quiz structure ----------------------------------------------------------

source(file.path(store_l_bias_efficiency$path_to_here , 'R', 'bias_efficiency_quiz.R'))
store_l_bias_efficiency$question_texts <- list(question_1, question_2)
store_l_bias_efficiency$question_prompts <- list(question_prompt_1, question_prompt_2)
store_l_bias_efficiency$correct_answers <- list(correct_answer_1, correct_answer_2)
store_l_bias_efficiency$message_correct <- "Well done! You got all of them correct. Please read on below if you'd like to learn more about post-treatment variables."
store_l_bias_efficiency$message_wrong <- "Hmmm, bummer! You got at least one wrong. Please take a minute to learn more about post-treatment variables below."

# clean up otherwise may have conflicts with other learning modules
rm(question_1, question_2, question_prompt_1, question_prompt_2, correct_answer_1, correct_answer_2)


# UI ----------------------------------------------------------------------

ui_learning_bias_efficiency <- function(id) {
  ns <- NS(id)
  tagList(
    ui <- fluidPage(
      
      # the quiz UI
      # ui_quiz(id = ns('quiz')),
      
      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content', # required
        includeMarkdown(file.path(store_l_bias_efficiency$path_to_here, "markdowns", 'bias_efficiency_1.md')),
      
        # d3 plot
        wellPanel(
          tags$iframe(src = 'learning/bias-efficiency/d3/index.html', 
                      height = 600, width = '100%',
                      style = 'border-width: 0;', scrolling = "no")
        ),

        includeMarkdown(file.path(store_l_bias_efficiency$path_to_here, "markdowns", 'bias_efficiency_2.md')),
        br(),
        wellPanel(includeMarkdown(file.path(store_l_bias_efficiency$path_to_here, "markdowns", 'bias_efficiency_related.md'))),
        includeMarkdown(file.path(store_l_bias_efficiency$path_to_here, "markdowns", 'bias_efficiency_citations.md'))
      )
    )
  )
}


# server ------------------------------------------------------------------

server_learning_bias_efficiency <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # run the quiz
      server_quiz(
        id = "quiz", # this should always be quiz
        id_parent = module_ids$learning$bias_efficiency,
        question_texts = store_l_bias_efficiency$question_texts,
        question_prompts = store_l_bias_efficiency$question_prompts,
        correct_answers = store_l_bias_efficiency$correct_answers,
        message_correct = store_l_bias_efficiency$message_correct,
        message_wrong = store_l_bias_efficiency$message_wrong,
        embed_quiz = TRUE
      )
      
    }
  )
}


# clean up ----------------------------------------------------------------


