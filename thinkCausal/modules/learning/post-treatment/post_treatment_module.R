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
  h4("We'll start with two practice questions to test your current knowledge. It's okay if you do not get them correct."),
  hr(),
  h3("Question 1"),
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
  h4("We'll start with a couple practice questions to test your current knowledge. It's okay if you do not get them all correct."),
  hr(),
  h3("Question 2"),
  p("A middle school offers an optional meditation class to 8th grade students and you’re tasked with determining if the meditation class causes higher grades at the end of 8th grade. Besides the treatment variable (meditation) and the outcome variable (grades), the school provided you with several other covariates. All covariates were pulled from administrative data at the end of 8th grade. Each covariate is show below:  "),
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
store_l_post_treatment$message_correct <- "Well done! You got all of them correct. Please read on below if you'd like to learn more about post-treatment variables."
store_l_post_treatment$message_wrong <- "Hmmm, bummer! You got at least one wrong. Please take a minute to learn more about post-treatment variables below."

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
        style = "max-width: 800px",
        includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_1.md')),
        br(),
        radioButtons(inputId = ns('include_pt'),
                     label = 'Include post-treatment variable bugs?',
                     choices = c('Include bugs', 'Do not include bugs'),
                     selected = 'Do not include bugs'),
        plotOutput(outputId = ns('posttreatment_plot'), 
                   height = 500),
        br(),
        includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_2.md'))
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
      
      # plot jitter
      output$posttreatment_plot <- renderPlot({

        if (input$include_pt == 'Include bugs') {
          
          # create plot with post-treatment variable
          p <- store_l_post_treatment$plants_df %>% 
            group_by(bugs, z) %>% 
            mutate(mean = mean(h1)) %>% 
            ggplot(aes(x = jitter(as.numeric(z)), y = h1, 
                       group = bugs, shape = bugs, col = z_as_text)) +
            geom_point() +
            # TODO: fix this
            geom_hline(aes(yintercept = mean)) +
            scale_color_manual(values = c(4, 2)) +
            scale_x_continuous(labels = c('Control', 'Treatment'), breaks = c(0, 1)) +
            scale_shape_manual(values = c(19, 1)) + 
            labs(title = 'TBD',
                 subtitle = 'TBD',
                 x = 'Treatment status',
                 y = 'Plant height',
                 color = NULL)
          
        } else {
          
          # create plot without post-treatment variable
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
        }

        # add theme
        p <- p + plot_theme()

        return(p)
      })
    }
  )
}


# clean up ----------------------------------------------------------------


