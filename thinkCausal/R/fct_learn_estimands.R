#' learn_estimands
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

store_l_estimands <- list()
store_l_estimands$ns_quiz <- NS(NS('learning_estimands')('quiz'))

question_1 <- tagList(
  h4("Calculating the ATE"),
  hr(),
  h3("Question 1"), # h3 required for checkmark/red x placement
  p("Imagine your asked to determnie the average treatment effect of wearing HyperShoes (Z = 1) on 5k running times. The potential outcomes for a sample of 6 runners are shown below. Use this data to calculate the average treatment effect (ATE)."),
  # HTML(create_table(ate = -5, y_min = 30, y_max = 50, po_question = FALSE, ite_question = FALSE, id_unit = 'Runner', button = FALSE)),
  '< PO TABLE GOES HERE >',
  br(),
  p("The Average Treatment Effect (ATE) for the sample of runners is:")
)
question_prompt_1 <- numericInput(
  inputId = store_l_estimands$ns_quiz('answers'),
  label = NULL,
  value = NULL,
  step = 0.1
)

correct_answer_1 <- list(c(-5.3))

store_l_estimands$question_texts <- list(question_1)
store_l_estimands$question_prompts <- list(question_prompt_1)
store_l_estimands$correct_answers <- list(correct_answer_1)
store_l_estimands$message_correct <- "Well done! You got all of them correct. Please read on to learn about the next topic."
store_l_estimands$message_wrong <- "Good attempt but you got at least one wrong. Take another try!"
store_l_estimands$message_skipped <- 'Quiz skipped. You can restart it using the button below.'
