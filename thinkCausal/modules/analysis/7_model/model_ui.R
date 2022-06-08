
ui_model <- function(store, id){
  ns <- NS(id)
  tabPanel(
    title = 'Fit Model',
    value = id,
    fluidRow(
      column(4,
             wellPanel(
               class = 'card',
               h4('1. Specify model'),
               br(),
               selectInput(ns('analysis_model_estimand'),
                           label = 'Confirm causal estimand',
                           choices = c('ATE - Average treatment effect' = 'ATE',
                                       'ATC - Average treatment effect on the control' = 'ATC',
                                       'ATT - Average treatment effect on the treated' = 'ATT')),
               selectInput(ns('analysis_model_support'),
                           label = 'Remove observations without overlap',
                           choices = c('', 'Unsure', 'Yes', 'No')),
               HTML('<details><summary>Advanced modeling options</summary>'),
               selectInput(ns("analysis_over_ride_common_support"),
                           label = 'Common support rule:',
                           choices = c('Standard deviation' = 'sd', 'Chi squared' = 'chisq'))
             )
      ),
      column(4,
             wellPanel(
               class = 'card',
               h4('2. Specify secondary analyses'),
               br(),
               selectInput(ns('analysis_model_moderator_yes_no'),
                           label = 'Would you like to pre-specify subgroup analyses?',
                           choices = c("No", "Yes",'Unsure')),
               conditionalPanel(condition = "input.analysis_model_moderator_yes_no == 'Yes'", ns = ns,
                                selectInput(ns('analysis_model_moderator_vars'),
                                            label = 'Select moderator(s)',
                                            choices = NULL,
                                            multiple = TRUE)),
             )
      ),
      column(4,
             wellPanel(
               class = 'card',
               h4('3. Fit model'),
               br(),
               actionButton(inputId = ns("analysis_model_button_next"),
                            class = "nav-btn-focus",
                            label = "Fit model"),
               br(), br(),
               create_link_to_help('Model'),
               br(), br(),
               actionButton(inputId = ns("analysis_model_button_back"),
                            label = "Back")
               # br(), br(),
               # create_progress_bar(5/7*100)
             )
      )
    )
  )
  
}

