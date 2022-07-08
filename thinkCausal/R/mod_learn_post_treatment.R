#' learn_post_treatment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_learn_post_treatment_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' learn_post_treatment Server Functions
#'
#' @noRd 
mod_learn_post_treatment_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_learn_post_treatment_ui("learn_post_treatment_1")
    
## To be copied in the server
# mod_learn_post_treatment_server("learn_post_treatment_1")
