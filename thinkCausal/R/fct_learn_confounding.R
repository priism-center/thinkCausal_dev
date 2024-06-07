#' Content for the confounding quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
quiz_content_confounding <- local({

  ns <- shiny::NS('learning_confounder')
  ns1 <- shiny::NS(ns('quiz'))

    correct_aws <- -11.35

    question_prompt <- tagList(
      h5(glue::glue("Try calculating the ATE by taking the weighted average.")),
      p(glue::glue("Remember n is the weight of each group!")),
      reactable::renderReactable({
        readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p45.rds'))
      }),
      br(),
      br(),
      numericInput(inputId = ns1('answers'),
                   label = 'Weighted Average:',
                   value = NULL
                   )
    )


    q <- shinyquiz::create_question_raw(prompt = question_prompt,
                                        grader = function(user_input){setequal(round(user_input, 2), correct_aws)},
                                        correct_answer_pretty = paste(correct_aws, collapse = ', '))





  content <- shinyquiz::create_quiz(q, options = shinyquiz::set_quiz_options(ns = ns1,
                                                                             messages = shinyquiz::create_messages('Great job! You calculated the weighted average correctly!',
                                                                                                                   'Your weighted average is incorrect. You can review the example from earlier and try again!',
                                                                                                                   'The weighted average is -11.12.'
                                                                                                                   )))

  return(content)
})
