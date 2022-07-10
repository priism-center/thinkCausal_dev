#' analysis_visualize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_visualize_ui <- function(id){
  ns <- NS(id)
  tagList(
    '< VISUALIZE >'
  )
}

#' analysis_visualize Server Functions
#'
#' @noRd
mod_analysis_visualize_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_analysis_visualize_ui("analysis_visualize_1")

## To be copied in the server
# mod_analysis_visualize_server("analysis_visualize_1")
