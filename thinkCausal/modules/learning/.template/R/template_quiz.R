# function defines the quiz for just this module
# should return a list as outlined in this template
# you can add as many questions as you like

store_l_template$quiz <- function(){
  
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
    group_name = store_l_template$ns_quiz('answers'),
    orientation = "horizontal",
    class = 'default-sortable sortable-wide',
    sortable::add_rank_list(
      input_id = store_l_template$ns_quiz('answers_variables'),
      text = strong("Available"),
      labels = c('bp_3month', 'sex', 'height'),
      options = sortable::sortable_options(multiDrag = TRUE)
    ),
    sortable::add_rank_list(
      input_id = store_l_template$ns_quiz('answers_include'),
      text = strong("Control for"),
      labels = NULL,
      options = sortable::sortable_options(multiDrag = TRUE)
    ),
    sortable::add_rank_list(
      input_id = store_l_template$ns_quiz('answers_treatment'),
      text = strong("Treatment"),
      labels = c('fish_oil'),
      options = sortable::sortable_options(disabled = TRUE)
    ),
    sortable::add_rank_list(
      input_id = store_l_template$ns_quiz('answers_outcome'),
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
    inputId = store_l_template$ns_quiz('answers'),
    label = "Chose the right variable",
    choices = c('A', 'B')
  )
  
  # set the correct answers for question 2
  correct_answer_2 <- list(c('A'))
  
  
  # final quiz structure ----------------------------------------------------
  
  quiz <- list(
    question_texts = list(question_1, question_2),
    question_prompts = list(question_prompt_1, question_prompt_2),
    correct_answers = list(correct_answer_1, correct_answer_2),
    message_correct = "Well done! You got all of them correct. Please read on to learn about the next topic.",
    message_wrong = "Hmmm, bummer! You got at least one wrong. Please take a minute to review the above content.",
    message_skipped = "Quiz skipped. You can restart it using the button below."
  )

  return(quiz)
}
