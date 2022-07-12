#' analysis_subgroup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_subgroup_ui <- function(id){
  ns <- NS(id)
  tagList(
    '< SUBGROUP ANALYSES >'
  )
}

#' analysis_subgroup Server Functions
#'
#' @noRd
mod_analysis_subgroup_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(store)
  })
}

## To be copied in the UI
# mod_analysis_subgroup_ui("analysis_subgroup_1")

## To be copied in the server
# mod_analysis_subgroup_server("analysis_subgroup_1")
