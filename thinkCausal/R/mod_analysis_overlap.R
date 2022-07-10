#' analysis_overlap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_overlap_ui <- function(id){
  ns <- NS(id)
  tagList(
    '< OVERLAP >'
  )
}

#' analysis_overlap Server Functions
#'
#' @noRd
mod_analysis_overlap_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_analysis_overlap_ui("analysis_overlap_1")

## To be copied in the server
# mod_analysis_overlap_server("analysis_overlap_1")
