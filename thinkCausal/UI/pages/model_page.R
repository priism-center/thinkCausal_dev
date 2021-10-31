model_page <- tabPanel(
  title = 'Model',
  fluidRow(
    column(6,
           h4('1. Specify variables'),
           wellPanel(
             textInput('treatment_name', 
                       label = 'What is the name of the treatment or intervention?', 
                       placeholder = 'treatment'),
             textInput('treatment_name', 
                       label = 'What are the units of your outcome variable?',
                       placeholder = 'units'))
    ),
    column(6,
           h4('2. Specify design'), 
           wellPanel(
             selectInput('anaylsis_design', 
                         label = 'Indicate the study design', 
                         choices = c("", 
                                     "Unsure", 
                                     'Observational', 
                                     'Randomized treatment', 
                                     'Block randomized treatment', 
                                     'Natural experiment')), 
             conditionalPanel(condition = "input.anaylsis_design == 'Observational'", 
                              selectInput('analysis_include_sens', 
                                          label = 'Include sensitivity analysis', 
                                          choices = c('Yes', 'No'))
             ),
             conditionalPanel(condition = "input.anaylsis_design == 'Block Randomized Treatment'", 
                              selectInput('analysis_blocking_variable',
                                          label = 'Select blocking variable',
                                          choices = NULL)
             )
           )
    ),
  ),
  fluidRow(
    column(6,
           h4('3. Specify model'),
           wellPanel(
             selectInput('analysis_model_estimand', 
                         label = 'Select causal estimand', 
                         choices = c("",'Unsure', 'ATC', 'ATE', 'ATT')), 
             selectInput('analysis_random_intercept',
                         label = 'Random intercept',
                         choices = NULL),
             selectInput('analysis_model_support', 
                         label = 'Remove observations with weak common support', 
                         choices = c("", 'Unsure', 'Yes', 'No')), 
             HTML('<details><summary>Advanced modeling options</summary>'),
             selectInput("analysis_over_ride_common_support",
                         label = 'Common support rule:', 
                         choices = c('standard deviation', 'chi squared'))
           )
    ), 
    column(6, 
           h4('4. Specify subgroup analyses'),
           wellPanel(
             selectInput('analysis_model_moderator_yes_no', 
                         label = 'Would you like to pre-specify subgroup analyses?', 
                         choices = c("No", "Yes",'Unsure')),
             conditionalPanel(condition = "input.analysis_model_moderator_yes_no == 'Yes'", 
                              selectInput('analysis_model_moderator_vars', 
                                          label = 'Select moderator(s)', 
                                          choices = NULL, 
                                          multiple = T)),
           )
    ),
  ),
  fluidRow(
    column(6, 
           h4('5. Fit model'),
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
