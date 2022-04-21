# this defines the post-treatment learning module under Learning

require(shiny)
require(tidyr)
require(tibble)
require(ggplot2)

### requires d3 to be loaded in the main UI ###


# objects -----------------------------------------------------------------

# list to store all post-treatment objects in
store_l_estimands <- list()

# set path
store_l_estimands$path_to_here <- file.path('modules', 'learning', 'estimands')

# read in randomization df
store_l_estimands$plants_df <- readr::read_csv(
  file.path(store_l_estimands$path_to_here, 'data', 'plants_df.csv'),
  col_types = readr::cols(
    h1 = readr::col_double(),
    z = readr::col_logical(),
    bugs = readr::col_logical()
  )
)
store_l_estimands$plants_df$z_as_text <- ifelse(store_l_estimands$plants_df$z, 'Treatment', 'Control')


# namespace ---------------------------------------------------------------

# pay close attention to how the namespace is managed. Since the quiz module
# is inside the post-treatment module, its namespace is a 'child' of the post-treatment
# module. The post-treatment module namespace id is set within module_ids object in global.R.
# The quiz module id is always 'quiz' which then becomes 'learning_post_treatment-quiz'
# UI and server elements outside of the quiz can be treated as you would normal modules

# ns id is set as 'learning_post_treatment' per module_ids$learning$post_treatment

# set namespace for UI quiz elements
store_l_estimands$ns_quiz <- NS(NS(module_ids$learning$estimands)('quiz'))


# set quiz structure ------------------------------------------------------

# set the text for question 1
question_1 <- tagList(
  h4("A quick knowledge check"),
  hr(),
  h3("Question 1"), # h3 required for checkmark/red x placement
  p("You’re tasked with determining if omega-3 fish oil supplements cause a decrease in blood pressure over a 6 month period. You have data from an experiment where participants were randomly assigned to take omega-3 fish oil supplements or a placebo supplement for 6 months. Besides the treatment variable (fish_oil) and the outcome variable (bp_6month) you have the following covariates: "),
  tags$ul(
    tags$li('Blood pressure measured at the start of the study (bp_baseline)'),
    tags$li('Blood pressure measured  3 months into the study (bp_3month)'),
    tags$li('Sex measured at the start of the study (sex)'),
    tags$li('Height measured at the start of the study (height)')
  ),
  p("Which covariates would you include in your analysis? Use the drag-drop below to move variables into the include or exclude bins.")
)

# set the UI elements for question 1
question_prompt_1 <- sortable::bucket_list(
  header = "Drag the variables to their respective roles",
  group_name = store_l_estimands$ns_quiz('answers'),
  orientation = "horizontal",
  class = 'default-sortable sortable-wide',
  sortable::add_rank_list(
    input_id = store_l_estimands$ns_quiz('answers_variables'),
    text = strong("Available"),
    labels = c('bp_3month', 'sex', 'height'),
    options = sortable::sortable_options(multiDrag = TRUE)
  ),
  sortable::add_rank_list(
    input_id = store_l_estimands$ns_quiz('answers_include'),
    text = strong("Control for"),
    labels = NULL,
    options = sortable::sortable_options(multiDrag = TRUE)
  ),
  sortable::add_rank_list(
    input_id = store_l_estimands$ns_quiz('answers_treatment'),
    text = strong("Treatment"),
    labels = c('fish_oil'),
    options = sortable::sortable_options(disabled = TRUE)
  ),
  sortable::add_rank_list(
    input_id = store_l_estimands$ns_quiz('answers_outcome'),
    text = strong("Outcome"),
    labels = c('bp_6month'),
    options = sortable::sortable_options(disabled = TRUE)
  )
)

# set the correct answers for question 1
# answer structure must match structure provided by input$answers
correct_answer_1 <- list(c('bp_3month'),
                         c('sex', 'height'),
                         c('fish_oil'),
                         c('bp_6month'))


# question 2 --------------------------------------------------------------

# set the text for question 2
question_2 <- tagList(
  h4("A quick knowledge check"),
  hr(),
  h3("Question 2"), # h3 required for checkmark/red x placement
  p("A middle school offers an optional meditation class to 8th grade students and you’re tasked with determining if the meditation class causes higher grades at the end of 8th grade. Besides the treatment variable (meditation) and the outcome variable (grades), the school provided you with several other covariates. All covariates were pulled from administrative data at the end of 8th grade. Each covariate is show below:  "),
)

# set the UI elements for question 2
question_prompt_2 <- radioButtons(
  inputId = store_l_estimands$ns_quiz('answers'),
  label = "Chose the right variable",
  choices = c('A', 'B')
)

# set the correct answers for question 2
correct_answer_2 <- list(c('A'))


# final quiz structure ----------------------------------------------------

store_l_estimands$question_texts <- list(question_1, question_2)
store_l_estimands$question_prompts <- list(question_prompt_1, question_prompt_2)
store_l_estimands$correct_answers <- list(correct_answer_1, correct_answer_2)
store_l_estimands$message_correct <- "Well done! You got all of them correct. Please read on to learn about the next topic."
store_l_estimands$message_wrong <- "Hmmm, bummer! You got at least one wrong. Please take a minute to review the above content."
store_l_estimands$message_skipped <- 'Quiz skipped. You can restart it using the button below.'

# clean up otherwise may have conflicts with other learning modules
rm(question_1, question_2, question_prompt_1, question_prompt_2, correct_answer_1, correct_answer_2)


# UI ----------------------------------------------------------------------

ui_learning_estimands <- function(id) {
  ns <- NS(id)
  tagList(
    ui <- fluidPage(
      class = 'learning-page',
      
      # load custom css
      includeCSS(file.path('www', 'learning', 'estimands', 'css', 'estimands.css')),
      
      # load custom javascript
      tags$script(src = file.path('js', 'libraries', 'd3.v5.js')), 
      tags$script(src = file.path('js', 'libraries', 'jstat.min.js')), 
      
      tags$script(src = file.path('learning', 'estimands', 'js', 'namespace.js')),
      tags$script(src = file.path('learning', 'estimands', 'js', 'helpers.js')),
      tags$script(src = file.path('learning', 'estimands', 'js', 'scrollPlot.js')),
      tags$script(src = file.path('learning', 'estimands', 'js', 'buildTable.js')),
      tags$script(src = file.path('learning', 'estimands', 'js', 'buildPlot.js')),
      tags$script(src = file.path('learning', 'estimands', 'js', 'init.js')),
      
      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_1.md')),
        br(),br(),br(),br(),br(),br(),
      ),
      
      div(
        class = 'scrollytell-container',
        
        # text content
        div(
          class = 'estimands-text-along-d3',
          includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_ate.md'))
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
        br(),
        includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_2.md')),
        br()
      ),
      
      # the quiz UI
      ui_quiz(id = ns('quiz')),
      
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        class = ns('learning-content-blur'), # required for blur
        class = 'learning-content-blur', # required for blur
        br(),
        includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_attatc.md')),
        br(),br(),br(),br(),br(),
        wellPanel(includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_related.md'))),
        includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_citations.md'))
      )
    )
  )
}


# server ------------------------------------------------------------------

server_learning_estimands <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # run the quiz
      server_quiz(
        id = "quiz", # this should always be quiz
        id_parent = module_ids$learning$estimands,
        question_texts = store_l_estimands$question_texts,
        question_prompts = store_l_estimands$question_prompts,
        correct_answers = store_l_estimands$correct_answers,
        message_correct = store_l_estimands$message_correct,
        message_wrong = store_l_estimands$message_wrong,
        message_skipped = store_l_estimands$message_skipped,
        embed_quiz = TRUE
      )
      
    }
  )
}


# clean up ----------------------------------------------------------------


