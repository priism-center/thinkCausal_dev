#' analysis_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    '< RESULTS >'
  )
}

#' analysis_results Server Functions
#'
#' @noRd
mod_analysis_results_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_analysis_results_ui("analysis_results_1")

## To be copied in the server
# mod_analysis_results_server("analysis_results_1")
