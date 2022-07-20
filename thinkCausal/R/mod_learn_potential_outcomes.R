#' learn_potential_outcomes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_potential_outcomes_ui <- function(id){
  ns <- NS(id)
  tagList(
    '< POTENTIAL OUTCOMES >'
  )
}

#' learn_potential_outcomes Server Functions
#'
#' @noRd
mod_learn_potential_outcomes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_learn_potential_outcomes_ui("learn_potential_outcomes_1")

## To be copied in the server
# mod_learn_potential_outcomes_server("learn_potential_outcomes_1")
