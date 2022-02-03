### this is the module that wraps a learning module with a quiz ###
### see post-treatment learning module for an example ###
### this might cause namespace issues ###
### constraints for questions are: question must return a list of user inputs that can be checked against ##
### you can have as many questions as you want ###
### cannot change logic --> first incorrect answer stops the quiz and puts the user in the article ###
### known bugs: 
# - double clicking on submit answer button will skip the next question
# - sortable divs require answers to be in the same order as answer key

require(shiny)
require(shinyjs)

ui_quiz <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(outputId = ns('UI_quiz'))
  )
}

server_quiz <- function(id, id_parent, question_texts, question_prompts, correct_answers, message_correct, message_wrong) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      # message(paste0('The quiz module has a namespace id of: ', id))
      
      # validate(need(length(questions) == length(answers),
      #               'length(questions) must equal length(answers)'))
      
      # set the current state and potential values
      store <- reactiveValues(
        state = 'quiz-question-1',
        states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
        question_texts = question_texts,
        question_prompts = question_prompts,
        correct_answers = correct_answers,
        responses = rep(NA, length(question_texts) + 1)
      )
      
      # on button submit, manage the state
      observeEvent(input$submit_button, {
        
        # record answers
        store <- set_state(store, variable = 'current-response', value = input$answers)
        
        # is the answer correct
        is_correct <- quiz_is_current_correct(store)
        
        # grade it
        if (is_correct){
          # add UI indicator
          add_checkmark(ns = ns)
          
          # change the state
          shinyjs::delay(1000, {
            new_state <- get_state(store, variable = 'next-state')
            store <- set_state(store, variable = 'current-state', value = new_state)
          })
        } else {
          # add UI indicator
          add_red_x(ns = ns)
          
          # change the state
          shinyjs::delay(1000, {
            store <- set_state(store, variable = 'current-state', value = 'quiz-complete')
          })
        }
        
      })
      
      # reset quiz
      observeEvent(input$restart_button, {
        # reset the state to the first question
        store <- set_state(store, variable = 'current-state', value = 'quiz-question-1')
        
        # remove any responses
        store$responses <- rep(NA, length(question_texts) + 1)
        
        # hide content
        shinyjs::runjs('$(".learning-content").hide()')
      })
      
      # render the UI
      output$UI_quiz <- renderUI({
        
        if (get_state(store) == 'quiz-complete'){
          
          # scroll to top
          shinyjs::runjs("window.scrollTo(0, 0)")

          # render ending message and confetti
          all_correct <- quiz_is_all_correct(store)
          if (all_correct) {
            html_content <- tagList(br(),
                                    add_message_correct(message_correct),
                                    add_confetti())
          } else {
            html_content <- tagList(br(), add_message_wrong(message_wrong))
          }

          # make non-quiz content visible
          shinyjs::runjs('$(".learning-content").show()')

        } else {

          # render the questions
          html_content <- tagList(
            p(get_state(store, 'current-question')),
            get_state(store, 'current-answers'),
            actionButton(inputId = NS(NS(id_parent)(id))('submit_button'),
                         label = 'Submit',
                         class = 'submit-button'),
            br(), 
          )
        }

        # add the restart button
        html_content <- tagList(
          html_content,
          br(),
          actionButton(inputId = NS(NS(id_parent)(id))('restart_button'),
                       label = 'Restart quiz',
                       class = 'restart-button'),
          br(), hr(), br(), br()
        )
        
        # wrap html in a div
        html_content <- div(class = 'quiz-container',
                            html_content)

        return(html_content)
      })
    }
  )
}



# state machine -----------------------------------------------------------


#' Manage the states of the quiz
#'
#' The quiz has states for each question and a final state for once the quiz ends. Only one state can be active at a time and the question text and answers shown depend on what state is active. 
#' 
#' These are `get` and `set` functions for retrieving state values and setting values. The states are originally created via a `reactiveValues` call within Shiny server (or `list` outside of Shiny; see example below).
#'
#' @param store a list formatted like in the example
#' @param variable one of c('current-question', 'current-answers', 'current-correct-answer', 'next-state', 'current-response')
#' @param state one of c('quiz-question-1', ..., 'quiz-question')
#'
#' @return
#' @export
#'
#' @examples
#' question_1 <- "This is question 1"
#' question_2 <- "This is question 2"
#' question_texts <- list(question_1, question_2)
#' question_prompts <- list(c('1a', '1b'), c('2a', '2b'))
#' correct_answers <- list(c('1a'), c('2b'))
#' # use shiny::reactiveValues() in lieu of list()
#' store <- list( 
#'   state = 'quiz-question-1',
#'   states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
#'   question_texts = question_texts,
#'   question_prompts = question_prompts,
#'   correct_answers = correct_answers,
#'   responses = c('yes', NA, NA)
#' )
#' get_state(store, 'current-question')
#' @describeIn get_state a getter function for the state machine
get_state <- function(store, variable = NULL, state = NULL){
  if (is.null(state)) state <- store$state
  if (is.null(variable)) return(state)
  if (!(state %in% store$states)) stop('state not in store$states')
  
  if (variable == 'current-question'){
    return(store$question_texts[store$states == state][[1]])
  }
  if (variable == 'current-answers'){
    return(store$question_prompts[store$states == state][[1]])
  }
  if (variable == 'current-correct-answer'){
    return(store$correct_answers[store$states == state][[1]])
  }
  if (variable == 'next-state'){
    return(store$states[min(length(store$question_texts)+1, match(state, store$states) + 1)])
  }
  if (variable == 'current-response'){
    return(store$responses[store$states == state][[1]])
  }
}


#' @describeIn get_state a setter function for the state machine
set_state <- function(store, variable, value, state = NULL){
  if (is.null(state)) state <- get_state(store)
  if (is.null(value)) value <- character(0)
  
  if (variable == 'current-state'){
    store$state <- value
  }
  
  if (variable == 'current-response'){
    store$responses[store$states == state] <- list(value)
  }
  
  return(store)
}

#' @describeIn get_state check that current-response correct
quiz_is_current_correct <- function(store){
  current_response <- unname(get_state(store, variable = 'current-response'))
  current_correct_answer <- unname(get_state(store, variable = 'current-correct-answer'))
  identical(current_response, current_correct_answer)
}

#' @describeIn get_state check that all recorded answers are correct
quiz_is_all_correct <- function(store) {
  tryCatch({
    # extract responses and correct answers
    responses <- unname(store$responses[-(length(store$question_texts) + 1)])
    correct_answers <- unname(store$correct_answers)
    
    # remove names
    responses <- purrr::map(responses, unname)
    correct_answers <- purrr::map(correct_answers, unname)
    
    # check if they are the same
    is_identical <- identical(responses, correct_answers)
    
    return(is_identical)
  },
  error = function(e) FALSE)
}


# helpers -----------------------------------------------------------------

# adds a green checkmark to a div
add_checkmark <- function(ns = NULL, div_id = 'submit_button'){
  # ns <- shiny::NS(ns)
  div_id <- ns(div_id)
  shinyjs::runjs({
    div_selector <- paste0('$("#', div_id, '")')
    paste0(
      'if (',
      div_selector,
      '.children().length===0){',
      div_selector,
      '.append("\t" + \'<span class="glyphicon glyphicon-ok" style="color:green"></span>\')}'
    )
  })
}

# adds a red X to a div
add_red_x <- function(ns = NULL, div_id = 'submit_button'){
  # ns <- shiny::NS(ns)
  div_id <- ns(div_id)
  shinyjs::runjs({
    div_selector <- paste0('$("#', div_id, '")')
    paste0(
      'if (',
      div_selector,
      '.children().length===0){',
      div_selector,
      '.append("\t" + \'<span class="glyphicon glyphicon-remove" style="color:red"></span>\')}'
    )
  })
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

# https://codepen.io/zer0kool/pen/KjZWRW
add_confetti <- function(){
  
  # individual confetti divs
  html <- div(
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
  
  # css
  html <- tagList(html, div(tags$style(
    "
    .confetti {
    display: flex;
    justify-content: center;
    align-items: center;
    position: fixed;
    width: 100%;
    height: 100%;
    top: 0;
    left: 0;
    overflow: hidden;
    z-index: -999;
    }
    .confetti-piece {
        position: absolute;
        width: 10px;
        height: 30px;
        background: #ffd300;
        top: 0;
        opacity: 0;
    }
    .confetti-piece:nth-child(1) {
        left: 7%;
        -webkit-transform: rotate(-40deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 182ms;
        -webkit-animation-duration: 1116ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(2) {
        left: 14%;
        -webkit-transform: rotate(4deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 161ms;
        -webkit-animation-duration: 1076ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(3) {
        left: 21%;
        -webkit-transform: rotate(-51deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 481ms;
        -webkit-animation-duration: 1103ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(4) {
        left: 28%;
        -webkit-transform: rotate(61deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 334ms;
        -webkit-animation-duration: 708ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(5) {
        left: 35%;
        -webkit-transform: rotate(-52deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 302ms;
        -webkit-animation-duration: 776ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(6) {
        left: 42%;
        -webkit-transform: rotate(38deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 180ms;
        -webkit-animation-duration: 1168ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(7) {
        left: 49%;
        -webkit-transform: rotate(11deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 395ms;
        -webkit-animation-duration: 1200ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(8) {
        left: 56%;
        -webkit-transform: rotate(49deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 14ms;
        -webkit-animation-duration: 887ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(9) {
        left: 63%;
        -webkit-transform: rotate(-72deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 149ms;
        -webkit-animation-duration: 805ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(10) {
        left: 70%;
        -webkit-transform: rotate(10deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 351ms;
        -webkit-animation-duration: 1059ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(11) {
        left: 77%;
        -webkit-transform: rotate(4deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 307ms;
        -webkit-animation-duration: 1132ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(12) {
        left: 84%;
        -webkit-transform: rotate(42deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 464ms;
        -webkit-animation-duration: 776ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(13) {
        left: 91%;
        -webkit-transform: rotate(-72deg);
        -webkit-animation: makeItRain 1000ms infinite ease-out;
        -webkit-animation-delay: 429ms;
        -webkit-animation-duration: 818ms;
        -webkit-animation-iteration-count: 3;
    }
    .confetti-piece:nth-child(odd) {
        background: #7431e8;
    }
    .confetti-piece:nth-child(even) {
        z-index: 1;
    }
    .confetti-piece:nth-child(4n) {
        width: 5px;
        height: 12px;
        -webkit-animation-duration: 2000ms;
    }
    .confetti-piece:nth-child(3n) {
        width: 3px;
        height: 10px;
        -webkit-animation-duration: 2500ms;
        -webkit-animation-delay: 1000ms;
    }
    .confetti-piece:nth-child(4n-7) {
      background: red;
    }
    @-webkit-keyframes makeItRain {
        from {opacity: 0;}
        50% {opacity: 1;}
        to {-webkit-transform: translateY(350px);}
    }
    "
  )))
  
  return(html)
}

