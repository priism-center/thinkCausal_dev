#' Content for the post treatment quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
quiz_content_post_treatment <- local({

  content <- list()
  content$ns_quiz <- NS(NS('learning_post_treatment')('quiz'))

  # set quiz structure ------------------------------------------------------

  # set the text for question 1
  question_1 <- tagList(
    h4("We'll start with two practice questions to test your current knowledge. It's okay if you do not get them correct."),
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
    group_name = content$ns_quiz('answers'),
    orientation = "horizontal",
    class = 'default-sortable sortable-wide',
    sortable::add_rank_list(
      input_id = content$ns_quiz('answers_variables'),
      text = strong("Available"),
      labels = c('bp_3month', 'sex', 'height'),
      options = sortable::sortable_options(multiDrag = TRUE)
    ),
    sortable::add_rank_list(
      input_id = content$ns_quiz('answers_include'),
      text = strong("Control for"),
      labels = NULL,
      options = sortable::sortable_options(multiDrag = TRUE)
    ),
    sortable::add_rank_list(
      input_id = content$ns_quiz('answers_treatment'),
      text = strong("Treatment"),
      labels = c('fish_oil'),
      options = sortable::sortable_options(disabled = TRUE)
    ),
    sortable::add_rank_list(
      input_id = content$ns_quiz('answers_outcome'),
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
    h3("Question 2"), # h3 required for checkmark/red x placement
    p("A middle school offers an optional meditation class to 8th grade students and you’re tasked with determining if the meditation class causes higher grades at the end of 8th grade. Besides the treatment variable (meditation) and the outcome variable (grades), the school provided you with several other covariates. All covariates were pulled from administrative data at the end of 8th grade. Each covariate is show below:  "),
  )

  # set the UI elements for question 2
  question_prompt_2 <- sortable::bucket_list(
    header = "Drag the variables to their respective roles",
    group_name = content$ns_quiz('answers'),
    orientation = "horizontal",
    class = 'default-sortable sortable-wide',
    sortable::add_rank_list(
      input_id = content$ns_quiz('answers_variables'),
      text = strong("Available"),
      labels = c('6th grade grades', '7th grade grades', 'Detentions during 8th grade', 'Race'),
      options = sortable::sortable_options(multiDrag = TRUE)
    ),
    sortable::add_rank_list(
      input_id = content$ns_quiz('answers_include'),
      text = strong("Control for"),
      labels = NULL,
      options = sortable::sortable_options(multiDrag = TRUE)
    ),
    sortable::add_rank_list(
      input_id = content$ns_quiz('answers_treatment'),
      text = strong("Treatment"),
      labels = c('Meditation'),
      options = sortable::sortable_options(disabled = TRUE)
    ),
    sortable::add_rank_list(
      input_id = content$ns_quiz('answers_outcome'),
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

  content$question_texts <- list(question_1, question_2)
  content$question_prompts <- list(question_prompt_1, question_prompt_2)
  content$correct_answers <- list(correct_answer_1, correct_answer_2)
  content$message_correct <- "Well done! You got all of them correct. Please read on below if you'd like to learn more about post-treatment variables."
  content$message_wrong <- "Hmmm, bummer! You got at least one wrong. Please take a minute to learn more about post-treatment variables below."
  content$message_skipped <- "Quiz skipped. You can restart it using the button below."

  return(content)
})


theme_timeline <- function(){
  theme_classic() +
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line.x =element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "top"
    )
}
