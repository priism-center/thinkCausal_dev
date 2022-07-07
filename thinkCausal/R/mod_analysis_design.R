#' analysis_design UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_design_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' analysis_design Server Functions
#'
#' @noRd 
mod_analysis_design_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_analysis_design_ui("analysis_design_1")
    
## To be copied in the server
# mod_analysis_design_server("analysis_design_1")
