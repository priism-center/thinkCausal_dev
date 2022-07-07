#' learning_estimands UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_learning_estimands_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' learning_estimands Server Functions
#'
#' @noRd 
mod_learning_estimands_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_learning_estimands_ui("learning_estimands_1")
    
## To be copied in the server
# mod_learning_estimands_server("learning_estimands_1")
