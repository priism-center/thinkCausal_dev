#' analysis_verify UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_verify_ui <- function(id){
  ns <- NS(id)
  tagList(
    '< VERIFY DATA >'
  )
}

#' analysis_verify Server Functions
#'
#' @noRd
mod_analysis_verify_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_analysis_verify_ui("analysis_verify_1")

## To be copied in the server
# mod_analysis_verify_server("analysis_verify_1")
