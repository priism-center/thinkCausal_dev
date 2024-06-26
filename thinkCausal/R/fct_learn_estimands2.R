#' Content for the estimands2 quiz
#'
#' @description A fct function
#'
#' @return a list
#'
#' @noRd
quiz_content_estimands2 <- local({

  # content <- list()
  # content$ns_quiz <- NS(NS('learning_estimands')('quiz'))
  #ns <- shiny::NS(shiny::NS('learning_estimands')('quiz'))
  ns <- shiny::NS('learning_estimands2')
  ns1 <- shiny::NS(ns('quiz'))
  generate_estimand2 <- function(){
    n <- 750
    x = switch (sample(c('norm', 'unif'), 1),
      'norm' = rnorm(n),
      'unif' = runif(n, -2, 2)
    )

    if(isTRUE(sample(c(T, F), 1))){
      beta <- rnorm(1, 2)
      y1 = x*beta + rnorm(n) + rnorm(1, -2, 5)
      if(isTRUE(sample(c(T, F), 1))) beta <- rnorm(1, 2)
      y0 = x*beta + rnorm(n)

    }else{
      y0 <-  rnorm(1, 2,1)*x + rnorm(n)
      if(isTRUE(sample(c(T, F), 1))) y0 <-  rnorm(1, .3, 1)*x + I((x+.3)^2)*rnorm(1, 1, .5) + rnorm(n)
      y1 <- 6 + rnorm(1, .3, 1)*x + I((x+.3)^2)*rnorm(1, 1, .5) + rnorm(n)
    }

    z <- rbinom(n, 1, .5)
    y <- ifelse(z == 1, y1, y0)
    dat <- data.frame(y, z, x)

    correct_aws <- sample(c('ATE', 'ATT', 'ATC'), 1)

    val <- quantile(x)[sample(c(2:4), 1)]
    direction <- sample(c('>', '<'), 1)
    ind <- switch (sample(c('greater', 'less'), 1),
      'greater' = eval(parse(text = "x > val")),
      'less' = eval(parse(text = "x < val"))
    )

    dat <- switch (correct_aws,
      'ATE' = dat,
      'ATT' = dat %>% dplyr::filter(ind | z == 0),
      'ATC' = dat %>% dplyr::filter(ind | z == 1)
    )



    if(correct_aws == 'ATE') correct_aws <- c('ATE', 'ATT', 'ATC')


    question_prompt <- tagList(
      h5(glue::glue("Select estimands")),
      p(glue::glue("Given the data, which estimands can we estimate with a statistical model?")),
      renderPlot({
        ggplot2::ggplot(dat, ggplot2::aes(x, y, col = as.factor(z))) +
          ggplot2::geom_point() +
          ggplot2::scale_color_manual(values = c(4, 2), labels = c('Control', 'Treatment')) +
          ggplot2::labs(col = NULL) +
          ggplot2::theme_bw()
      }),
      br(),
      br(),
      checkboxGroupInput(inputId = ns1('answers'),
                         label = 'Select all that apply:',
                         choices = c('ATE', 'ATT', 'ATC'),
                         inline = TRUE
      )
      )


    q <- shinyquiz::create_question_raw(prompt = question_prompt,
                                    grader = function(user_input){setequal(user_input, correct_aws)},
                                    correct_answer_pretty = paste(correct_aws, collapse = ', '))


    return(q)

  }

  sandbox_q <- shinyquiz::create_question_random(.f = generate_estimand2, n = 25)
  content <- shinyquiz::create_quiz(sandbox_q, options = shinyquiz::set_quiz_options(ns = ns1))

  return(content)
})
