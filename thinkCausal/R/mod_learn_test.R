#' learn_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_test_ui <- function(id){
  ns <- NS(id)
  tagList(


  )
}

#' learn_test Server Functions
#'
#' @noRd
mod_learn_test_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


  })
}

## To be copied in the UI
# mod_learn_test_ui("learn_test_1")

## To be copied in the server
# mod_learn_test_server("learn_test_1")
