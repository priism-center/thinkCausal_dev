### this is the module that wraps a learning module with a quiz ###
### see post-treatment learning module for an example ###
### namespace issues **should be** taken care of ###
### constraints for questions are: question must return a list of user inputs that can be checked against ##
### you can have as many questions as you want ###
### cannot change this logic --> first incorrect answer stops the quiz and puts the user in the article ###
### requires learning.css style sheet in /www folder ###

# post_treatment mod is the current working example

# TODO: move to separate R package

#' quiz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_quiz_ui <- function(id){
  ns <- NS(id) # parent module

  tagList(
    # shinyjs::useShinyjs(),
    div(
      id = ns('quiz-container'),
      class = 'quiz-container',
      uiOutput(outputId = ns('UI_quiz'))
    )
  )
}

#' quiz Server Functions
#'
#' @noRd
# mod_quiz_server <- function(id, id_parent = character(0), question_texts, question_prompts, correct_answers, graders = NULL, message_correct, message_wrong, message_skipped, embed_quiz = TRUE, sandbox_mode = FALSE){
mod_quiz_server <- function(id, id_parent = character(0), questions, message_correct, message_wrong, message_skipped, embed_quiz = TRUE, sandbox_mode = FALSE){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    ns <- NS(NS(id_parent)(id))

    # message(paste0('The quiz module has a namespace id of: ', id))

    # validate(need(length(questions) == length(answers),
    #               'length(questions) must equal length(answers)'))

    # add css class to the quiz container if embedding
    if (isTRUE(embed_quiz)) shinyjs::addClass(id = 'quiz-container', class = 'quiz-embedded')

    # resample the questions if in sandbox mode
    if (isTRUE(sandbox_mode)){
      # number of questions
      n <- 50L

      # sample indices for replicating the questions
      indices <- sample(seq_along(questions), size = n, replace = TRUE)
      questions <- questions[indices]
      # question_prompts <- question_prompts[indices]
      # correct_answers <- correct_answers[indices]
    }

    # add headers to question texts
    # question_texts <- quiz_format_question_texts(questions)
    for (i in seq_along(questions)){
      # print(questions[[i]]@question)
      questions[[i]]@question <- quiz_format_question_text(questions[[i]]@question, i)
    }

    # set the current state and potential values
    store <- reactiveValues(
      state = 'quiz-question-1',
      states = c(paste0('quiz-question-', seq_along(questions)), 'quiz-complete'),
      questions = questions,
      # question_prompts = question_prompts,
      # correct_answers = correct_answers,
      # graders = graders,
      # responses = rep(NA, length(question_texts) + 1),
      is_correct = rep(FALSE, length(questions)),
      ui_html = NULL,
      skipped = FALSE,
      sandbox_mode = isTRUE(sandbox_mode)
    )

    # reset quiz
    observeEvent(input$restart_button, {
      # reset the state to the first question
      store <- quiz_set_state(store, variable = 'current-state', value = 'quiz-question-1')

      # remove any responses
      # store$responses <- rep(NA, length(questions) + 1)
      store$questions <- questions
      store <- quiz_set_state(store, variable = 'quiz-skipped', value = FALSE)
      store$is_correct <- rep(FALSE, length(questions))
    })

    # skip quiz / finish quiz
    observeEvent(input$skip_button, {
      store <- quiz_set_state(store, variable = 'current-state', value = 'quiz-complete')
      store <- quiz_set_state(store, variable = 'quiz-skipped', value = TRUE)
    })

    # control state behavior
    observeEvent(store$state, {

      # scroll to top of quiz container
      scroll_to_div(ns = ns, id = 'quiz-container')

      # make non-quiz content visible (may be re-hidden depending on final state)
      shinyjs::show(selector = paste0('.', NS(id_parent)('learning-content')), asis = TRUE)

      # state behavior
      if (store$state == 'quiz-complete'){
        # determine the UI
        store$ui_html <- quiz_ui_quiz_complete(
          store,
          ns = ns,
          message_correct = message_correct,
          message_wrong = message_wrong,
          message_skipped = message_skipped
        )

        # unblur the text
        shinyjs::removeClass(selector = paste0('.', NS(id_parent)('learning-content-blur')),
                             asis = TRUE,
                             class = 'learning-content-blur')

      } else {
        # determine the UI
        store$ui_html <- quiz_ui_question(store, ns = ns)

        # hide non-quiz content
        if (!isTRUE(embed_quiz)){
          shinyjs::hide(selector = paste0('.', NS(id_parent)('learning-content')), asis = TRUE)
        }
      }
    })

    # on button submit, record answer and change the state
    observeEvent(input$submit_button, {

      # disable submit button to prevent double clicks
      shinyjs::disable(id = 'submit_button')

      # scroll to top of quiz container
      scroll_to_div(ns = ns, id = 'quiz-container')

      # record answers
      store <- quiz_set_state(store, variable = 'current-response', value = input$answers)

      # is the answer correct and record it
      is_correct <- quiz_is_current_correct(store)
      store <- quiz_set_state(store, 'current-correct', is_correct)

      # grade it
      delay_in_ms <- 2000
      if (is_correct){
        # add UI indicator
        add_checkmark(ns = ns, id = 'quiz-container', element = 'h3')

        # change the state
        shinyjs::delay(delay_in_ms, {
          new_state <- quiz_get_state(store, variable = 'next-state')
          store <- quiz_set_state(store, variable = 'current-state', value = new_state)
        })

      } else {
        # add UI indicator
        add_red_x(ns = ns, id = 'quiz-container', element = 'h3')

        # change the state
        # if in sandbox mode, go to next question otherwise end here
        shinyjs::delay(delay_in_ms, {
          if (quiz_in_sandbox_mode(store)){
            new_state <- quiz_get_state(store, variable = 'next-state')
            store <- quiz_set_state(store, variable = 'current-state', value = new_state)
          } else {
            store <- quiz_set_state(store, variable = 'current-state', value = 'quiz-complete')
          }
        })
      }
    })

    # render the UI
    output$UI_quiz <- renderUI(store$ui_html)
  })
}


# state machine -----------------------------------------------------------

#' @title Functions for managing the states of the quiz
#'
#' @description The quiz has states for each question and a final state for once the quiz ends. Only one state can be active at a time and the question text and answers shown depend on what state is active.
#'
#' These are `get` and `set` functions for retrieving state values and setting values. The states are originally created via a `reactiveValues` call within Shiny server (or `list` outside of Shiny; see example below).
#'
#' See the post-treatment learning module for a working example.
#'
#' @param store a list formatted like in the example
#' @param variable one of c('current-question', 'current-correct-answer', 'next-state', 'current-response')
#' @param state one of c('quiz-question-1', ..., 'quiz-question')
#'
#' @return depends on function
#' @noRd
#'
#' @author Joseph Marlo
#'
#' @examples
#' \dontrun{
#' question_1 <- "This is question 1"
#' question_2 <- "This is question 2"
#' question_texts <- list(question_1, question_2)
#' question_prompts <- list(radioButtons(...), radioButtons(...))
#' correct_answers <- list(c('1a'), c('2b'))
#' # use shiny::reactiveValues() in lieu of list()
#' store <- list(
#'   state = 'quiz-question-1',
#'   states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
#'   question_texts = question_texts,
#'   question_prompts = question_prompts,
#'   correct_answers = correct_answers,
#'   responses = c('yes', NA, NA),
#'   skipped = FALSE,
#'   sandbox_mode = FALSE
#' )
#' quiz_get_state(store, 'current-question')
#' }
#' @noRd
#' @describeIn quiz_get_state a getter function for the state machine
quiz_get_state <- function(store, variable = NULL, state = NULL){
  if (is.null(state)) state <- store$state
  if (is.null(variable)) return(state)
  if (!(state %in% store$states)) stop('state not in store$states')

  if (variable == 'current-question'){
    return(store$questions[store$states == state][[1]]@question)
  }
  # if (variable == 'current-answers'){
  #   return(store$question_prompts[store$states == state][[1]])
  # }
  if (variable == 'current-correct-answer'){
    # return(store$correct_answers[store$states == state][[1]])
    return(store$questions[store$states == state][[1]]@answerCorrectDisplay)
  }
  if (variable == 'current-grader'){
    # return(store$graders[store$states == state][[1]])
    return(store$questions[store$states == state][[1]]@grader)
  }
  if (variable == 'current-correct'){
    return(store$is_correct[store$states == state])
  }
  if (variable == 'next-state'){
    return(store$states[min(length(store$states), match(state, store$states) + 1)])
  }
  if (variable == 'current-response'){
    # return(store$responses[store$states == state][[1]])
    return(store$questions[store$states == state][[1]]@answerUser[[1]]) # there's some weird indexing that happens with the lists
  }
  if (variable == 'quiz-skipped'){
    return(store$skipped)
  }
  if (variable == 'sandbox-mode'){
    return(store$sandbox_mode)
  }
}

#' @noRd
#' @describeIn quiz_get_state a setter function for the state machine
quiz_set_state <- function(store, variable, value, state = NULL){

  if (is.null(state)) state <- quiz_get_state(store)
  if (is.null(value)) value <- character(0)

  if (variable == 'current-state'){
    store$state <- value
  }
  if (variable == 'current-response'){
    # store$responses[store$states == state] <- list(value)
    # store$questions[store$states == state][[1]]@answerUser[[1]] <- value
    store$questions[[which(store$states == state)]]@answerUser[[1]] <- value # there's some weird indexing that happens with the lists
  }
  if (variable == 'quiz-skipped'){
    if (!is.logical(value)) stop('value must logical for "quiz-skipped" variable')
    store$skipped <- value
  }
  if (variable == 'current-correct'){
    state_index <- store$states[store$states != 'quiz-complete']
    store$is_correct[state_index == state] <- value
  }

  return(store)
}

#' @noRd
#' @describeIn quiz_get_state Backup function to check that an answer matches a response, agnostic of ordering
quiz_is_answer_correct <- function(answer, key){
  if (length(answer) != length(key)) return(FALSE)
  if(!is.numeric(answer)) is_correct <- purrr::map2_lgl(answer, key, function(resp, key) base::setequal(resp, key))
  if(is.numeric(answer)) is_correct <-  purrr::map2_lgl(answer, key, function(resp, key) between(resp, key-.1, key+.1))
  is_correct <- isTRUE(all(is_correct))
  return(is_correct)
}

#' @noRd
#' @describeIn quiz_get_state check that current-response is correct
quiz_is_current_correct <- function(store){
  current_response <- unname(quiz_get_state(store, variable = 'current-response'))
  current_correct_answer <- unname(quiz_get_state(store, variable = 'current-correct-answer'))

  # if there is a grader function, use it. Otherwise use the generic one defined above
  current_grader <- quiz_get_state(store, 'current-grader')
  if (shiny::isTruthy(current_grader)){
    is_correct <- current_grader(current_response)
  } else {
    is_correct <- quiz_is_answer_correct(current_response, current_correct_answer)
  }
  return(is_correct)
}

#' @noRd
#' @describeIn quiz_get_state check that recorded answers are correct and return a boolean vector
quiz_check_is_each_correct <- function(store){
  return(store$is_correct)
}

#' @noRd
#' @describeIn quiz_get_state check that all recorded answers are correct
quiz_is_all_correct <- function(store) {
  return(isTRUE(all(quiz_check_is_each_correct(store))))
}

#' @noRd
#' @describeIn quiz_get_state Check if the quiz in sandbox mode
quiz_in_sandbox_mode <- function(store){
  isTRUE(quiz_get_state(store, 'sandbox-mode'))
}

#' @noRd
#' @describeIn quiz_get_state Add a header denoting the question number
# quiz_format_question_texts <- function(questions){
#   purrr::map2(questions, seq_along(questions), function(q_text, i) {
#     htmltools::tagList(
#       htmltools::h4("Practice what you've learned"),
#       htmltools::hr(),
#       htmltools::h3(glue::glue("Question {i}")), # h3 required for checkmark/red x placement
#       q_text@question
#     )
#   })
# }
quiz_format_question_text <- function(question, i){
  htmltools::div(
    htmltools::h4("Practice what you've learned"),
    htmltools::hr(),
    htmltools::h3(glue::glue("Question {i}")), # h3 required for checkmark/red x placement
    question
  )
}

#' @noRd
#' @describeIn quiz_get_state UI to show once the quiz is completed
quiz_ui_quiz_complete <- function(store, ns, message_correct, message_wrong, message_skipped){

  # render ending message based on if answers are correct
  all_correct <- quiz_is_all_correct(store)
  if (quiz_get_state(store, variable = 'quiz-skipped')){
    html_content <- tagList(br(), add_message_skipped(message_skipped))
  } else if (all_correct) {
    html_content <- tagList(br(),
                            add_message_correct(message_correct),
                            add_confetti())
  } else {
    html_content <- tagList(br(), add_message_wrong(message_wrong))
  }

  # render the report table
  grade_report <- quiz_ui_quiz_complete_report(store)

  # render the restart button
  restart_button <- actionButton(
    inputId = ns('restart_button'),
    label = 'Restart quiz',
    class = 'restart-button'
  )

  # put it all together
  html_content <- tagList(
    html_content,
    grade_report,
    restart_button,
    br(), br(), hr(), br()
  )

  return(html_content)
}

#' @noRd
#' @describeIn quiz_get_state Quiz score and table of correct answers to show at the end
quiz_ui_quiz_complete_report <- function(store){

  in_sandbox <- quiz_in_sandbox_mode(store)

  # grade answers and convert into icons
  icon_right <- shiny::icon('check') |> as.character()
  icon_wrong <- shiny::icon('times') |> as.character()
  answers <- quiz_check_is_each_correct(store)
  answers_icons <- c(icon_wrong, icon_right)[answers + 1]

  # format question labels
  question_label <- paste0('Question ', seq_along(store$questions))

  # calculate score and format user's answers
  # if in sandbox mode, score is only for non skipped items
  answers_user_print <- purrr::map(store$questions, ~.x@answerUserDisplay(.x@answerUser[[1]]))
  answers_user_na <- purrr::map(store$questions, ~.x@answerUser[[1]]) |> is.na() # assumes NAs are skipped questions

  score <- ifelse(
    in_sandbox,
    mean(answers[!answers_user_na]),
    mean(answers)
  )
  if(is.na(score)) score <- 0
  skip_label <- '[skipped]'
  answers_user_print[answers_user_na] <- skip_label


  # format correct answers
  answers_correct_print <- purrr::map_chr(store$questions, ~.x@answerCorrectDisplay)

  # put everything in a table
  grade_tbl <- tibble::tibble(
    icon = answers_icons,
    label = question_label,
    `Your Answer` = answers_user_print,
    `Correct Answer` = answers_correct_print
  ) |>
    dplyr::filter(`Your Answer` != skip_label) |>
    reactable::reactable(
      columns = list(
        icon = reactable::colDef(name = '', html = TRUE, width = 40),
        label = reactable::colDef(name = '', width = 115),
        `Your Answer` = reactable::colDef(align = 'right'),
        `Correct Answer` = reactable::colDef(align = 'right')
      )
    )

  # add score to top of table
  grade_report <- htmltools::tagList(
    br(),
    htmltools::h4(glue::glue('Score: {scales::percent_format()(score)}')),
    grade_tbl
  )

  return(grade_report)
}

#' @noRd
#' @describeIn quiz_get_state UI to show for each question
quiz_ui_question <- function(store, ns){

  # render the questions
  html_content <- tagList(

    # question text
    quiz_get_state(store, 'current-question'),

    # question answer UI (e.g. radiobuttons, sortable divs, etc.)
    # quiz_get_state(store, 'current-answers'),

    # button to submit answer
    actionButton(inputId = ns('submit_button'),
                 label = 'Submit',
                 class = 'submit-button'),

    # button to skip quiz
    actionButton(
      inputId = ns('skip_button'),
      label = ifelse(quiz_in_sandbox_mode(store), 'Finish quiz', 'Skip quiz'),
      class = 'skip-button'
    )
  )

  return(html_content)
}


# classes -----------------------------------------------------------------

# class for quizes
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

# TODO:
setClass('quizMessages', slots = list(
  correct = 'character',
  wrong = 'character',
  skipped = 'character'
))

#
# setClass('answerCorrect', slots = list(
#   answer = 'list',
#   display = 'character',
#   grader = 'function'
# ))
#
# setClass('answerUser', slots = list(
#   answer = 'list',
#   display = 'character'
# ))


# helpers -----------------------------------------------------------------

scroll_to_div <- function(ns = NULL, id = 'quiz-container'){
  if (!is.null(ns)) id <- ns(id)
  js <- paste0("$('#", id, "')[0].scrollIntoView()")
  shinyjs::runjs(js)
}

# adds a green checkmark to a div
add_checkmark <- function(ns = NULL, id = 'quiz-container', element = 'h3'){

  # construct selector
  if (!missing(ns)) id <- paste0("#", ns(id))
  if (!missing(element)) selector <- paste(id, element)
  div_selector <- paste0('$("', selector, '")')

  # construct javascript
  js <- paste0(
    'if (',
    div_selector,
    '.children().length===0){',
    div_selector,
    '.append("\t" + \'<span class="glyphicon glyphicon-ok" style="color:green; font-size: 0.9em;"></span>\')}'
  )

  # run js
  shinyjs::runjs(js)
}

# adds a red X to a div
add_red_x <- function(ns = NULL, id = 'quiz-container', element = 'h3'){

  # construct selector
  if (!missing(ns)) id <- paste0("#", ns(id))
  if (!missing(element)) selector <- paste(id, element)
  div_selector <- paste0('$("', selector, '")')

  # construct javascript
  js <- paste0(
    'if (',
    div_selector,
    '.children().length===0){',
    div_selector,
    '.append("\t" + \'<span class="glyphicon glyphicon-remove" style="color:red; font-size: 0.9em;"></span>\')}'
  )

  # run js
  shinyjs::runjs(js)
}

add_message_correct <- function(text){
  # this relies on bootstrap css
  shiny::div(
    class = 'alert alert-success',
    p(text)
  )
}

add_message_wrong <- function(text){
  # this relies on bootstrap css
  shiny::div(
    class = 'alert alert-danger',
    p(text)
  )
}

add_message_skipped <- function(text){
  # this relies on bootstrap css
  shiny::div(
    class = 'alert alert-warning',
    p(text)
  )
}

add_confetti <- function(){
  # https://codepen.io/zer0kool/pen/KjZWRW
  # requires confetti.css to be in the www/css folder
  confetti_pieces <- div(
    class = 'confetti',
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece'),
    div(class = 'confetti-piece')
  )

  # remove after 10 seconds to prevent re-animations when coming back to the page
  shinyjs::delay(
    10*1000,
    shinyjs::hide(selector = '.confetti')
  )

  return(confetti_pieces)
}
