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

  q <- shinyquiz::create_question(prompt = question_prompt,
                             shinyquiz::add_choice(''),
                             shinyquiz::add_choice(245),
                             shinyquiz::add_choice(8),
                             shinyquiz::add_choice(-8, correct = T),
                             shinyquiz::add_choice(253),
                             shinyquiz::add_choice(249),
                             type = 'single',
                             ns = ns1)


  content <- shinyquiz::create_quiz(q, options = shinyquiz::set_quiz_options(ns = ns1, sandbox = F))

  return(content)

})
