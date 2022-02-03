# this defines the post-treatment learning module under Learning

require(shiny)
require(tidyr)
require(tibble)
require(ggplot2)


# notes -------------------------------------------------------------------

# ns id is set as 'concepts_post_treatment'
# the quiz UI elements exist inside another namespace


# objects -----------------------------------------------------------------

# list to store all post-treatment objects in
store_l_post_treatment <- list()

# set path
store_l_post_treatment$path_to_here <- file.path('modules', 'learning', 'post-treatment')

# read in randomization df
store_l_post_treatment$plants_df <- readr::read_csv(
  file.path(store_l_post_treatment$path_to_here, 'data', 'plants_df.csv'),
  col_types = readr::cols(
    h1 = readr::col_double(),
    z = readr::col_logical(),
    bugs = readr::col_logical()
  )
)
store_l_post_treatment$plants_df$z_as_text <- ifelse(store_l_post_treatment$plants_df$z, 'Treatment', 'Control')


# set quiz structure ------------------------------------------------------

# set the text for question 1
question_1 <- tagList(
  h3("Question 1"),
  p("You’re tasked with determining if omega-3 fish oil supplements cause a decrease in blood pressure over a 6 month period. You have data from an experiment where participants were randomly assigned to take omega-3 fish oil supplements or a placebo supplement for 6 months. Besides the treatment variable (fish_oil) and the outcome variable (bp_6month) you have the following covariates: "),
  tags$ul(
    tags$li('Blood pressure measured at the start of the study (bp_baseline)'),
    tags$li('Blood pressure measured  3 months into the study (bp_3month)'),
    tags$li('Sex measured `at the start of the study (sex)'),
    tags$li('Height measured at the start of the study (height)')
  ),
  p("Which covariates would you include in your analysis? Use the drag-drop below to move variables into the include or exclude bins.")
)

# set the UI elements for question 1
question_prompt_1 <- sortable::bucket_list(
  header = "Drag the variables to their respective roles",
  group_name = NS(NS('concepts_post_treatment')('quiz'))('answers'),
  orientation = "horizontal",
  class = 'default-sortable sortable-wide',
  sortable::add_rank_list(
    input_id = NS(NS('concepts_post_treatment')('quiz'))('answers_variables'),
    text = strong("Available"),
    labels = c('bp_3month', 'sex', 'height'),
    options = sortable::sortable_options(multiDrag = TRUE)
  ),
  sortable::add_rank_list(
    input_id = NS(NS('concepts_post_treatment')('quiz'))('answers_include'),
    text = strong("Control for"),
    labels = NULL,
    options = sortable::sortable_options(multiDrag = TRUE)
  ),
  sortable::add_rank_list(
    input_id = NS(NS('concepts_post_treatment')('quiz'))('answers_treatment'),
    text = strong("Treatment"),
    labels = c('fish_oil'),
    options = sortable::sortable_options(disabled = TRUE)
  ),
  sortable::add_rank_list(
    input_id = NS(NS('concepts_post_treatment')('quiz'))('answers_outcome'),
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
  h3("Question 2"),
  p("A middle school offers an optional meditation class to 8th grade students and you’re tasked with determining if the meditation class causes higher grades at the end of 8th grade. Besides the treatment variable (meditation) and the outcome variable (grades), the school provided you with several other covariates. All covariates were pulled from administrative data at the end of 8th grade. Each covariate is show below:  "),
  # tags$ul(
  #   tags$li('6th grade grades  measured'),
  #   tags$li('7th grade grades'),
  #   tags$li('number of detentions during 8th grade '),
  #   tags$li('Race'),
  #   tags$li('Parent income at the end of 8th grade')
  # ),
  # p("Lorem ipsum")
)

# set the UI elements for question 2
question_prompt_2 <- sortable::bucket_list(
  header = "Drag the variables to their respective roles",
  group_name = NS(NS('concepts_post_treatment')('quiz'))('answers'),
  orientation = "horizontal",
  class = 'default-sortable sortable-wide',
  sortable::add_rank_list(
    input_id = NS(NS('concepts_post_treatment')('quiz'))('answers_variables'),
    text = strong("Available"),
    labels = c('6th grade grades', '7th grade grades', 'Detentions during 8th grade', 'Race'),
    options = sortable::sortable_options(multiDrag = TRUE)
  ),
  sortable::add_rank_list(
    input_id = NS(NS('concepts_post_treatment')('quiz'))('answers_include'),
    text = strong("Control for"),
    labels = NULL,
    options = sortable::sortable_options(multiDrag = TRUE)
  ),
  sortable::add_rank_list(
    input_id = NS(NS('concepts_post_treatment')('quiz'))('answers_treatment'),
    text = strong("Treatment"),
    labels = c('Meditation'),
    options = sortable::sortable_options(disabled = TRUE)
  ),
  sortable::add_rank_list(
    input_id = NS(NS('concepts_post_treatment')('quiz'))('answers_outcome'),
    text = strong("Outcome"),
    labels = c('Grades'),
    options = sortable::sortable_options(disabled = TRUE)
  )
)

# set the correct answers for question 2
correct_answer_2 <- list(c('Detentions during 8th grade'),
                         c('6th grade grades', '7th grade grades', 'Race'),
                         c('Meditation'),
                         c('Grades')
)
# use character(0) if any rank lists should be empty


# final quiz structure ----------------------------------------------------

store_l_post_treatment$question_texts <- list(question_1, question_2)
store_l_post_treatment$question_prompts <- list(question_prompt_1, question_prompt_2)
store_l_post_treatment$correct_answers <- list(correct_answer_1, correct_answer_2)
store_l_post_treatment$message_correct <- "You got all of them correct"
store_l_post_treatment$message_wrong <- "You got at least one wrong"

# clean up otherwise may have conflicts with other learning modules
rm(question_1, question_2, question_prompt_1, question_prompt_2, correct_answer_1, correct_answer_2)


# UI ----------------------------------------------------------------------

ui_post_treatment <- function(id) {
  ns <- NS(id)
  tagList(
    ui <- fluidPage(
      
      # the quiz UI
      ui_quiz(id = ns('quiz')),
      
      # UI content for the learning module
      div(
        class = 'learning-content', # required
        h3('Post-treatment variables'),
        includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_1.md')),
        br(),
        plotOutput(ns('posttreatment_plot'), height = 500, width = 700),
        br(),
        includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_2.md'))

        # p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
        # sliderInput(inputId = ns('slider_test'),
        #             label = 'Another UI element',
        #             min = 0,
        #             max = 1,
        #             value = 0.5),
        # textOutput(outputId = ns('text_test'))
      )
    )
  )
}


# server ------------------------------------------------------------------

server_post_treatment <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # run the quiz
      server_quiz(
        id = "quiz",
        id_parent = 'concepts_post_treatment',
        question_texts = store_l_post_treatment$question_texts,
        question_prompts = store_l_post_treatment$question_prompts,
        correct_answers = store_l_post_treatment$correct_answers,
        message_correct = store_l_post_treatment$message_correct,
        message_wrong = store_l_post_treatment$message_wrong
      )
      
      # example server to run outside of the quiz
      # output$text_test <- renderText({
      #   input$slider_test
      # })
      
      # plot jitter
      output$posttreatment_plot <- renderPlot({

        # create plot
        p <- ggplot(data = store_l_post_treatment$plants_df,
                    aes(x = jitter(as.numeric(z)), y = h1, col = z_as_text)) +
          geom_point() +
          scale_color_manual(values = c(4, 2)) +
          scale_x_continuous(labels = c('Control', 'Treatment'), breaks = c(0, 1)) +
          labs(title = 'TBD',
               subtitle = 'TBD',
               x = 'Treatment status',
               y = 'Plant height',
               color = NULL)

        # add theme
        p <- p + plot_theme()

        return(p)
      })
    }
  )
}


# clean up ----------------------------------------------------------------


