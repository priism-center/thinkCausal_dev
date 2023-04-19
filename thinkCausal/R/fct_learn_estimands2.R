#' Content for the estimands quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
quiz_content_estimands2 <- local({

  content <- list()
  content$ns_quiz <- shiny::NS(shiny::NS('learning_estimands2')('quiz'))
  # questions

# question 1 --------------------------------------------------------------
  question_text_1 <- htmltools::div(
    htmltools::p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
    htmltools::p("Filler text 1"),

    renderImage({
      list(src = app_sys('app', 'www/learn/estimands2/plots/quiz1.png'),
           contentType = 'image/png',
           width = 600,
           height = 400)
    }, deleteFile = F),

    shiny::checkboxGroupInput(
      inputId = content$ns_quiz('answers'),
      label = NULL,
      inline = TRUE,
      choices = c('ATE', 'ATT', 'ATC'),
      selected = NULL
    )
  )

  # answer
  correct_answer_1 <- list(c('ATE', 'ATT', 'ATC'))

  # TODO: move to a permanent home
  setClass('quizQuestion', slots = list(
    question = 'shiny.tag',
    answerUser = 'list',
    answerUserDisplay = 'function', # how to print the user answer in the report
    answerCorrectDisplay = 'character', # how to print the correct answer in the report
    grader = 'function' # function that compares user answer to the correct answer
  ))
  verify_question_structure <- function(question){

    if (!isTRUE(isS4(question))) cli::cli_abort('Must be an S4 object')
    if (!isTRUE(inherits(question, 'quizQuestion'))) cli::cli_abort('Must be an S4 object with class quizQuestion')

    if (!isTRUE(inherits(question@question, 'shiny.tag'))) cli::cli_abort('`question` must be of class shiny.tag. Preferably generated from htmltools::div().')

    if (!isTRUE(inherits(question@answerUserDisplay, 'function'))) cli::cli_abort('`answerUserDisplay` must be a function that accepts one argument and returns a character.')
    if (!isTRUE(inherits(question@answerCorrectDisplay, 'character'))) cli::cli_abort('`answerCorrectDisplay` must be a character.')
    if (!isTRUE(inherits(question@grader, 'function'))) cli::cli_abort('`grader` must be a function that accepts one argument and returns a boolean')

    # verify args


    return(invisible(TRUE))
  }


  # create the formal quizQuestion
  question_1 <- new('quizQuestion')
  question_1@question <- question_text_1
  question_1@answerUser = list(c('ATE', 'ATT', 'ATC'))
  question_1@answerUserDisplay <- function(x) {
    tryCatch(
      paste0(x[[2]], collapse = ', '),
      error = function(e) 'Cannot print user response'
    )
  }
  question_1@answerCorrectDisplay <- 'test1'
  question_1@grader <- function(x) TRUE

  verify_question_structure(question_1)


# question 2 --------------------------------------------------------------
  question_text_2 <- htmltools::div(
    htmltools::p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
    htmltools::p("Filler text 1"),

    renderImage({
      list(src = app_sys('app', 'www/learn/estimands2/plots/quiz2.png'),
           contentType = 'image/png',
           width = 600,
           height = 400)
    }, deleteFile = F),

    shiny::checkboxGroupInput(
      inputId = content$ns_quiz('answers'),
      label = NULL,
      inline = TRUE,
      choices = c('ATE', 'ATT', 'ATC'),
      selected = NULL
    )
  )

  # answer
  correct_answer_2 <- list(c('ATC'))

  # create the formal quizQuestion
  question_2 <- new('quizQuestion')
  question_2@question <- question_text_2
  question_2@answerUser = list(NA)
  question_2@answerUserDisplay <- function(x) {
    tryCatch(
      paste0(x[[2]], collapse = ', '),
      error = function(e) 'Cannot print user response'
    )
  }
  question_2@answerCorrectDisplay <- 'test2'
  question_2@grader <- function(x) TRUE

  verify_question_structure(question_2)



# question 3 --------------------------------------------------------------
  question_text_3 <- htmltools::div(
    # h4("Practice choosing an estimand based on overlap with these X practice questions:"),
    # hr(),
    # h3("Question 3"), # h3 required for checkmark/red x placement
    htmltools::p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
    htmltools::p("Filler text 1"),

    shiny::renderImage({
      list(src = app_sys('app', 'www/learn/estimands2/plots/quiz3.png'),
           contentType = 'image/png',
           width = 600,
           height = 400)
    }, deleteFile = F),

    shiny::checkboxGroupInput(
      inputId = content$ns_quiz('answers'),
      label = NULL,
      inline = TRUE,
      choices = c('ATE', 'ATT', 'ATC'),
      selected = NULL
    )
  )

  # create the formal quizQuestion
  question_3 <- new('quizQuestion')
  question_3@question <- question_text_3
  question_3@answerUser = list(NA)
  question_3@answerUserDisplay <- function(x) {
    tryCatch(
      paste0(x[[2]], collapse = ', '),
      error = function(e) 'Cannot print user response'
    )
  }
  question_3@answerCorrectDisplay <- 'test2'
  question_3@grader <- function(x) TRUE

  verify_question_structure(question_3)



  # place in a single list
  # content$question_texts <- list(question_1, question_2, question_3)
  # content$question_prompts <- list(question_prompt_1, question_prompt_2, question_prompt_3)
  # content$correct_answers <- list(correct_answer_1, correct_answer_2, correct_answer_3)
  # content$message_correct <- "Well done! You got all of them correct. Please read on to learn about the next topic."
  # content$message_wrong <- "Good attempt but you got at least one wrong. Take another try!"
  # content$message_skipped <- 'Quiz skipped. You can restart it using the button below.'

  # use character(0) if any rank lists should be empty
  content$questions <- list(question_1, question_2, question_3)
  # content$question_prompts <- list(question_prompt_1, question_prompt_2)
  # content$correct_answers <- list(correct_answer_1, correct_answer_2)
  content$message_correct <- "Well done! You got all of them correct. Please read on to learn about the next topic."
  content$message_wrong <- "Hmmm, bummer! You got at least one wrong."
  content$message_skipped <- "Quiz skipped. You can restart it using the button below."

  return(content)
})

