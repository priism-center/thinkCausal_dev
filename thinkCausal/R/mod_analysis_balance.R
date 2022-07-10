#' analysis_balance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_balance_ui <- function(id){
  ns <- NS(id)
  tagList(
    '< BALANCE >'
  )
}

#' analysis_balance Server Functions
#'
#' @noRd
mod_analysis_balance_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_analysis_balance_ui("analysis_balance_1")

## To be copied in the server
# mod_analysis_balance_server("analysis_balance_1")
