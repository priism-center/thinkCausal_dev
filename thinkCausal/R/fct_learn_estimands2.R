#' Content for the estimands quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
quiz_content_estimands2 <- local({

  content <- list()
  content$ns_quiz <- NS(NS('learning_estimands2')('quiz'))
  # questions

# question 1 --------------------------------------------------------------
  question_1 <- tagList(
      p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
      p("Filler text 1"),

    renderImage({
      list(src = app_sys('app', 'www/learn/estimands2/plots/p21.png'),
           contentType = 'image/png',
           width = 600,
           height = 400)
    }, deleteFile = F)
  )
  question_prompt_1 <- selectInput(
    inputId = content$ns_quiz('answers'),
    label = NULL,
    choices = c('', 'ATE', 'ATT', 'ATC'),
    selected = NULL
  )

  # answer
  correct_answer_1 <- list(c('ATC'))


# question 2 --------------------------------------------------------------
  question_2 <- tagList(
    p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
    p("Filler text 1"),

    renderImage({
      list(src = app_sys('app', 'www/learn/estimands2/plots/p21.png'),
           contentType = 'image/png',
           width = 600,
           height = 400)
    }, deleteFile = F)
  )
  question_prompt_2 <- selectInput(
    inputId = content$ns_quiz('answers'),
    label = NULL,
    choices = c('', 'ATE', 'ATT', 'ATC'),
    selected = NULL
  )

  # answer
  correct_answer_2 <- list(c('ATE'))


# question 3 --------------------------------------------------------------
  question_3 <- tagList(
    # h4("Practice choosing an estimand based on overlap with these X practice questions:"),
    # hr(),
    # h3("Question 3"), # h3 required for checkmark/red x placement
    p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
    p("Filler text 1"),

    renderImage({
      list(src = app_sys('app', 'www/learn/estimands2/plots/p22.png'),
           contentType = 'image/png',
           width = 600,
           height = 400)
    }, deleteFile = F)
  )

  question_prompt_3 <- selectInput(
    inputId = content$ns_quiz('answers'),
    label = NULL,
    choices = c('', 'ATE', 'ATT', 'ATC'),
    selected = NULL
  )

  # answer
  correct_answer_3 <- list(c('ATC'))



  # place in a single list
  content$question_texts <- list(question_1, question_2, question_3)
  content$question_prompts <- list(question_prompt_1, question_prompt_2, question_prompt_3)
  content$correct_answers <- list(correct_answer_1, correct_answer_2, correct_answer_3)
  content$message_correct <- "Well done! You got all of them correct. Please read on to learn about the next topic."
  content$message_wrong <- "Good attempt but you got at least one wrong. Take another try!"
  content$message_skipped <- 'Quiz skipped. You can restart it using the button below.'

  return(content)
})

