#' Content for the post treatment quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
#' @importFrom shiny NS
quiz_content_post_treatment <- local({
  content <- list()
  content$ns_quiz <- shiny::NS(shiny::NS('learning_post_treatment')('quiz'))

  # set quiz structure ------------------------------------------------------

  # set the text for question 1
  question_text_1 <- htmltools::div(
    htmltools::p("You’re tasked with determining if omega-3 fish oil supplements cause a decrease in blood pressure over a 6 month period. You have data from an experiment where participants were randomly assigned to take omega-3 fish oil supplements or a placebo supplement for 6 months. Besides the treatment variable (fish_oil) and the outcome variable (bp_6month) you have the following covariates: "),
    htmltools::tags$ul(
      htmltools::tags$li('Blood pressure measured at the start of the study (bp_baseline)'),
      htmltools::tags$li('Blood pressure measured  3 months into the study (bp_3month)'),
      htmltools::tags$li('Sex measured at the start of the study (sex)'),
      htmltools::tags$li('Height measured at the start of the study (height)')
    ),
    htmltools::p("Which covariates would you include in your analysis? Use the drag-drop below to move variables into the include or exclude bins."),

    # set the UI elements for question 1
    sortable::bucket_list(
      header = "Drag the variables to their respective roles",
      group_name = content$ns_quiz('answers'), # NOTE: this should be 'answers' for mod_quiz to recognize it
      orientation = "horizontal",
      class = 'default-sortable sortable-wide',
      sortable::add_rank_list(
        input_id = content$ns_quiz('answers_variables'),
        text = "Available",
        labels = c('bp_baseline','bp_3month', 'sex', 'height'),
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = content$ns_quiz('answers_include'),
        text = "Control for",
        labels = NULL,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = content$ns_quiz('answers_treatment'),
        text = "Treatment",
        labels = c('fish_oil'),
        options = sortable::sortable_options(disabled = TRUE)
      ),
      sortable::add_rank_list(
        input_id = content$ns_quiz('answers_outcome'),
        text = "Outcome",
        labels = c('bp_6month'),
        options = sortable::sortable_options(disabled = TRUE)
      )
    )
  )
  # preview: htmltools::html_print(question_text_1)

  # function to check the answers
  grader_1 <- function(user_response){

    # its best to catch any errors in these graders
    is_correct <- tryCatch({
      # set the correct answers here
      correct_answers <- list(
        c('bp_3month'),
        c('bp_baseline', 'sex', 'height'),
        c('fish_oil'),
        c('bp_6month')
      )

      # this structure is a result of input$'answers' where sortable returns 4 lists
      all_true <- all(
        setequal(user_response[[1]], correct_answers[[1]]),
        setequal(user_response[[2]], correct_answers[[2]]),
        setequal(user_response[[3]], correct_answers[[3]]),
        setequal(user_response[[4]], correct_answers[[4]])
      )

      if (isTRUE(all_true)){
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    error = function(e) return(FALSE)
    )

    return(is_correct)
  }

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
  question_1@answerUser = list(NA)
  question_1@answerUserDisplay <- function(x) {
    tryCatch(
      paste0(x[[2]], collapse = ', '),
      error = function(e) 'Cannot print user response'
    )
  }
  question_1@answerCorrectDisplay <- paste0(c('bp_baseline', 'sex', 'height'), collapse = ', ')
  question_1@grader <- grader_1

  verify_question_structure(question_1)


  # question 2 --------------------------------------------------------------

  # set the text for question 2
  question_text_2 <- htmltools::div(
    htmltools::p("A middle school offers an optional meditation class to 8th grade students at the beginning of their 8th grade year. You’re tasked with determining if the meditation class caused higher grades at the end of 8th grade. Besides the treatment variable (meditation) and the outcome variable (grades), the school provided you with several other covariates. All covariates were pulled from administrative data at the end of 8th grade. Each covariate is show below:"),

    # set the UI elements for question 2
    question_prompt_2 <- sortable::bucket_list(
      header = "Drag the variables to their respective roles",
      group_name = content$ns_quiz('answers'),
      orientation = "horizontal",
      class = 'default-sortable sortable-wide',
      sortable::add_rank_list(
        input_id = content$ns_quiz('answers_variables'),
        text = "Available",
        labels = c('6th grade grades', '7th grade grades', 'Detentions during 8th grade', 'Race'),
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = content$ns_quiz('answers_include'),
        text = "Control for",
        labels = NULL,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = content$ns_quiz('answers_treatment'),
        text = "Treatment",
        labels = c('Meditation'),
        options = sortable::sortable_options(disabled = TRUE)
      ),
      sortable::add_rank_list(
        input_id = content$ns_quiz('answers_outcome'),
        text = "Outcome",
        labels = c('Grades'),
        options = sortable::sortable_options(disabled = TRUE)
      )
    )
  )

  # function to check the answers
  grader_2 <- function(user_response){

    # its best to catch any errors in these graders
    is_correct <- tryCatch({
      # set the correct answers here
      correct_answers <- list(
        c('Detentions during 8th grade'),
        c('6th grade grades', '7th grade grades', 'Race'),
        c('Meditation'),
        c('Grades')
      )

      # this structure is a result of input$'answers' where sortable returns 4 lists
      all_true <- all(
        setequal(user_response[[1]], correct_answers[[1]]),
        setequal(user_response[[2]], correct_answers[[2]]),
        setequal(user_response[[3]], correct_answers[[3]]),
        setequal(user_response[[4]], correct_answers[[4]])
      )

      if (isTRUE(all_true)){
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    error = function(e) return(FALSE)
    )

    return(is_correct)
  }

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
  question_2@answerCorrectDisplay <- paste0(c('6th grade grades', '7th grade grades', 'Race'), collapse = ', ')
  question_2@grader <- grader_2

  verify_question_structure(question_2)


  # put it all in a list ----------------------------------------------------

  # use character(0) if any rank lists should be empty
  content$questions <- list(question_1, question_2)
  # content$question_prompts <- list(question_prompt_1, question_prompt_2)
  # content$correct_answers <- list(correct_answer_1, correct_answer_2)
  content$message_correct <- "Well done! You got all of them correct."
  content$message_wrong <- "Hmmm, bummer! You got at least one wrong."
  content$message_skipped <- "Quiz skipped. You can restart it using the button below."
  # content$graders <- list(grader_1, grader_2)

  return(content)
})
