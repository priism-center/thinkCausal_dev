#' Content for the estimands quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
quiz_content_estimands <- local({

  content <- list()
  content$ns_quiz <- NS(NS('learning_estimands')('quiz'))
  po <- create_table(ate = -5, y_min = 30, y_max = 50, po_question = FALSE, ite_question = FALSE, id_unit = 'Runner', button = FALSE, )
  # questions
  question_1 <- tagList(
    h4("Calculating the ATE"),
    hr(),
    h3("Question 1"), # h3 required for checkmark/red x placement
    p("Imagine you're asked to determine the average treatment effect of wearing HyperShoes (Z = 1) on 5k running times. The potential outcomes for a sample of 6 runners are shown below. Use this data to calculate the average treatment effect (ATE)."),
    HTML(po[[1]]),
    br(),
    p("The Average Treatment Effect (ATE) for the sample of runners is:")
  )
  question_prompt_1 <- numericInput(
    inputId = content$ns_quiz('answers'),
    label = NULL,
    value = NULL,
    step = 0.1
  )

  # answer
  correct_answer_1 <- list(c(mean(as.numeric(po[[2]]$ITE))))
  # place in a single list
  content$question_texts <- list(question_1)
  content$question_prompts <- list(question_prompt_1)
  content$correct_answers <- list(correct_answer_1)
  content$message_correct <- "Well done! You got all of them correct. Please read on to learn about the next topic."
  content$message_wrong <- "Good attempt but you got at least one wrong. Take another try!"
  content$message_skipped <- 'Quiz skipped. You can restart it using the button below.'

  return(content)
})
