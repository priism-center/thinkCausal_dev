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
    '< DIAGNOSTICS >'
  )
}

#' analysis_diagnostics Server Functions
#'
#' @noRd
mod_analysis_diagnostics_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(store)
  })
}

## To be copied in the UI
# mod_analysis_diagnostics_ui("analysis_diagnostics_1")

## To be copied in the server
# mod_analysis_diagnostics_server("analysis_diagnostics_1")
