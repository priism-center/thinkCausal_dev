
# this may need to be a module
create_quiz <- function(questions, answers){
  
}

# post-treatment specific functions
create_quiz_post_treatment <- function(ns) {
  
  intro <- shiny::tagList(
    shiny::h2("Post-treatment variables"),
    shiny::p("[insert one/two sentence describing what the user will learn]")
  )
  
  drag_drop <- 'asd'
  
  question_1 <- shiny::tagList(
    shiny::h3("Question 1"),
    shiny::p("You’re tasked with determining if omega-3 fish oil supplements cause a decrease in blood pressure over a 6 month period. You have data from an experiment where participants were randomly assigned to take omega-3 fish oil supplements or a placebo supplement for 6 months. Besides the treatment variable (fish_oil) and the outcome variable (bp_6month) you have the following covariates: "),
    shiny::tags$li('Blood pressure measured at the start of the study (bp_baseline)'),
    shiny::tags$li('Blood pressure measured  3 months into the study (bp_3month)'),
    shiny::tags$li('Sex measured at the start of the study (sex)'),
    shiny::tags$li('Height measured at the start of the study (height)'),
    br(),br(),
    drag_drop
  )

  question_2 <- question_1
  
  questions <- shiny::tagList(
    intro,
    question_1,
    question_2,
    br(),br()
  )
  
  return(questions)
}

wrap_with_quiz <- function(ns, quiz, ...){
  shiny::tagList(
    quiz,
    ...
  )
}
