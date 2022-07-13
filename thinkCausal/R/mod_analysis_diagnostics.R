#' analysis_diagnostics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_diagnostics_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::box(
        width = 6,
        collapsible = FALSE,
        title = 'Model diagnostics',
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
        br(),
        conditionalPanel("input.analysis_diagnostics_tabs == 'Overlap'", ns = ns,
                         selectInput(
                           inputId = ns("analysis_diagnostics_plot_overlap_covariate"),
                           label = "By covariate:",
                           choices = NULL,
                           selected = NULL
                         )),
        conditionalPanel("input.analysis_diagnostics_tabs == 'Residual vs fit'", ns = ns,
                         selectInput(
                           inputId = ns("analysis_diagnostics_plot_residual_covariate"),
                           label = "By covariate: ",
                           multiple = FALSE,
                           choices = NULL,
                           selected = NULL
                         )),
        actionButton(inputId = ns('analysis_diagnostics_help'),
                     label = 'What are these plots telling me?'),
        uiOutput(outputId = ns('analysis_diagnosis_buttons_ui'))
      ),
      bs4Dash::box(
        width = 6,
        collapsible = FALSE,
        title = 'Trace plot',
        plotOutput(ns('analysis_diagnostics_plot_trace'),
                   height = 500),
        downloadButton(ns('download_diagnostic_plot'), label = "Download plot")
      )
    ),
    fluidRow(
      bs4Dash::box(
        width = 6,
        collapsible = FALSE,
        title = 'Overlap',
        plotOutput(ns('analysis_diagnostics_plot_support'),
                   height = 500)
        # downloadButton(ns('download_diagnostic_plot'), label = "Download plot")
      ),
      bs4Dash::box(
        width = 6,
        collapsible = FALSE,
        title = 'Residual vs fit',
        plotOutput(ns('analysis_diagnostics_plot_residual'),
                   height = 500)
        # downloadButton(ns('download_diagnostic_plot'), label = "Download plot")
      )
    ),
    fluidRow(
      bs4Dash::box(
        width = 6,
        collapsible = FALSE,
        title = 'Residual normality',
        plotOutput(ns('analysis_diagnostics_plot_normal'),
                   height = 500)
        # downloadButton(ns('download_diagnostic_plot'), label = "Download plot")
      )
    )
  )
}

#' analysis_diagnostics Server Functions
#'
#' @noRd
mod_analysis_diagnostics_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_diagnostics_help, {
      open_help_sidebar(store, 'Diagnostics')
    })

    return(store)
  })
}

## To be copied in the UI
# mod_analysis_diagnostics_ui("analysis_diagnostics_1")

## To be copied in the server
# mod_analysis_diagnostics_server("analysis_diagnostics_1")
