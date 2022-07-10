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
    '< MODEL >'
  )
}

#' analysis_model Server Functions
#'
#' @noRd
mod_analysis_model_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_analysis_model_ui("analysis_model_1")

## To be copied in the server
# mod_analysis_model_server("analysis_model_1")
