model_page <- tabPanel(
  title = 'Model',
  fluidRow(
    column(6,
           h4('1. Specify model'),
           wellPanel(
             selectInput('analysis_model_estimand', 
                         label = 'Select causal estimand', 
                         choices = c("",
                                     'Unsure', 
                                     'ATE - Average treatment effect' = 'ATE', 
                                     'ATC - Average treatment effect on the control' = 'ATC', 
                                     'ATT - Average treatment effect on the treated' = 'ATT')), 
             selectInput('analysis_random_intercept',
                         label = 'Random intercept',
                         choices = NULL),
             selectInput('analysis_model_support', 
                         label = 'Remove observations with weak common support', 
                         choices = c("", 'Unsure', 'Yes', 'No')), 
             HTML('<details><summary>Advanced modeling options</summary>'),
             selectInput("analysis_over_ride_common_support",
                         label = 'Common support rule:', 
                         choices = c('Standard deviation' = 'sd', 'Chi squared' = 'chisq'))
           )
    ), 
    column(6, 
           h4('2. Specify secoundary analyses'),
           wellPanel(
             selectInput('analysis_model_moderator_yes_no', 
                         label = 'Would you like to pre-specify subgroup analyses?', 
                         choices = c("No", "Yes",'Unsure')),
             conditionalPanel(condition = "input.analysis_model_moderator_yes_no == 'Yes'", 
                              selectInput('analysis_model_moderator_vars', 
                                          label = 'Select moderator(s)', 
                                          choices = NULL, 
                                          multiple = TRUE)),
           )
    ),
  ),
  fluidRow(
    column(6, 
           h4('3. Fit model'),
           wellPanel(
             actionButton(inputId = "analysis_model_button_next",
                          label = "Fit model"),
             br(), br(),
             tags$button(
               type = 'button',
               class = 'btn btn-default help',
               onclick = "openHelpPage('Concept2')",
               'Help'
             ),
             br(), br(),
             actionButton(inputId = "analysis_model_button_back",
                          label = "Back to EDA"),
             br(), br(),
             create_progress_bar(5/7*100)
           )
    )
  )
)
