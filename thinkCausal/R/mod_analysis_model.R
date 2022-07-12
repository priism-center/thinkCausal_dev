#' analysis_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_model_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::box(
        width = 4,
        collapsible = FALSE,
        title = '1. Specify model',
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
                    label = 'Overlap rule:',
                    choices = c('Standard deviation' = 'sd', 'Chi squared' = 'chisq'))
        ),
      bs4Dash::box(
        width = 4,
        collapsible = FALSE,
        title = '2. Specify secondary analyses',
        selectInput(ns('analysis_model_moderator_yes_no'),
                    label = 'Would you like to pre-specify subgroup analyses?',
                    choices = c("No", "Yes", 'Unsure')),
        conditionalPanel(
          condition = "input.analysis_model_moderator_yes_no == 'Yes'",
          ns = ns,
          selectInput(ns('analysis_model_moderator_vars'),
                      label = 'Create subgroups by:',
                      choices = NULL,
                      multiple = TRUE))
        ),
      bs4Dash::box(
        width = 4,
        collapsible = FALSE,
        title = '3. Fit model',
        actionButton(inputId = ns("analysis_model_button_next"),
                     class = "nav-path",
                     label = "Fit model"),
        actionButton(inputId = ns('analysis_model_help'),
                     label = 'Help me'),
        actionButton(inputId = ns("analysis_model_button_back"),
                     label = "Back")
        )
      )
    )
}

#' analysis_model Server Functions
#'
#' @noRd
mod_analysis_model_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_model_help, {
      open_help_sidebar(store, 'Model')
    })

    return(store)
  })
}

## To be copied in the UI
# mod_analysis_model_ui("analysis_model_1")

## To be copied in the server
# mod_analysis_model_server("analysis_model_1")
