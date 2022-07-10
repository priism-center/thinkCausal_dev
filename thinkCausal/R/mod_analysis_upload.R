#' analysis_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    '< UPLOAD >'
  )
}

#' analysis_upload Server Functions
#'
#' @noRd
mod_analysis_upload_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_analysis_upload_ui("analysis_upload_1")

## To be copied in the server
# mod_analysis_upload_server("analysis_upload_1")
