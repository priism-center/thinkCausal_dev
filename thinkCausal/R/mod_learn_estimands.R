#' learn_estimands UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_estimands_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' learn_estimands Server Functions
#'
#' @noRd
mod_learn_estimands_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_learn_estimands_ui("learn_estimands_1")

## To be copied in the server
# mod_learn_estimands_server("learn_estimands_1")
