#' Content for the estimands quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
quiz_content_estimands <- local({

  # content <- list()
  # content$ns_quiz <- NS(NS('learning_estimands')('quiz'))
  #ns <- shiny::NS(shiny::NS('learning_estimands')('quiz'))
  ns <- shiny::NS('learning_estimands')
  ns1 <- shiny::NS(ns('quiz'))
  generate_estimand <- function(){
    po <- create_table(ate = sample(-25:0, 1), y_min = 20, y_max = 50, po_question = FALSE, ite_question = FALSE, id_unit = 'Runner', button = FALSE)
    estimand <- sample(c('ATE','ATT', 'ATC'), 1)
    estimand_long <- switch (estimand,
      'ATE' = 'average treatment effect (ATE).',
      'ATT' = 'average treatment effect on the treated (ATT)',
      'ATC' = 'average treatment effect on the control (ATC)'
    )

    # possible answers

    ate <- round(mean(as.numeric(po[[2]]$Y1) - as.numeric(po[[2]]$Y0)), 2)
    att <- round(mean(as.numeric(po[[2]]$Y1[po[[2]]$Z == '1']) - as.numeric(po[[2]]$Y0[po[[2]]$Z == '1'])), 2)
    atc <- round(mean(as.numeric(po[[2]]$Y1[po[[2]]$Z == '0']) - as.numeric(po[[2]]$Y0[po[[2]]$Z == '0'])), 2)
    avg_y <- round(lm(as.numeric(po[[2]]$Y) ~ as.numeric(po[[2]]$Z))$coef[2], 2)

    question_prompt <- tagList(
      h5(glue::glue("Calculating the {estimand}")),
      p(glue::glue("Imagine you're asked to determine the average treatment effect of wearing HyperShoes (Z = 1) on 5k running times. The potential outcomes for a sample of 6 runners are shown below. Use this data to calculate the {estimand_long}.")),
      HTML(po[[1]]),
      br(),
      p(glue::glue("The {estimand_long} for this sample of runners is:"))
    )


    q <- shinyQuiz::create_question(prompt = question_prompt,
                               shinyQuiz::add_choice(ate, correct = estimand == 'ATE'),
                               shinyQuiz::add_choice(att, correct = estimand == 'ATT'),
                               shinyQuiz::add_choice(atc, correct = estimand == 'ATC'),
                               shinyQuiz::add_choice(avg_y),
                               ns = ns1)


    return(q)

    }

  sandbox_q <- shinyQuiz::create_question_sandbox(.f = generate_estimand, n = 25)
  content <- shinyQuiz::create_quiz(sandbox_q, options = shinyQuiz::set_quiz_options(ns = ns1))

  return(content)
})
