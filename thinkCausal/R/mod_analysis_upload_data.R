#' analysis_upload_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_upload_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' analysis_upload_data Server Functions
#'
#' @noRd 
mod_analysis_upload_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_analysis_upload_data_ui("analysis_upload_data_1")
    
## To be copied in the server
# mod_analysis_upload_data_server("analysis_upload_data_1")
