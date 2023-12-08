#' Content for the fundamental quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
quiz_content_fundamental <- local({

  ns <- shiny::NS('learning_fundamental')
  ns1 <- shiny::NS(ns('quiz'))

  question_prompt <- tagList(
    p(glue::glue("What is the individual causal effect of runner 3?")),
    reactable::reactable(readr::read_csv('inst/extdata/fundamental_table1.csv')[3,])
  )

  q <- shinyQuiz::create_question(prompt = question_prompt,
                             shinyQuiz::add_choice(''),
                             shinyQuiz::add_choice(245),
                             shinyQuiz::add_choice(8),
                             shinyQuiz::add_choice(-8, correct = T),
                             shinyQuiz::add_choice(253),
                             shinyQuiz::add_choice(249),
                             type = 'single',
                             ns = ns1)


  content <- shinyQuiz::create_quiz(q, options = shinyQuiz::set_quiz_options(ns = ns1, sandbox = F))

  return(content)

})
