#' versionC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_versionC_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' versionC Server Functions
#'
#' @noRd
mod_learn_versionC_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_versionC_ui("versionC_1")

## To be copied in the server
# mod_versionC_server("versionC_1")
