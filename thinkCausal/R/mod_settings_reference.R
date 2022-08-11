#' settings_reference UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_reference_ui <- function(id){
  ns <- NS(id)
  tagList(
    includeMarkdown(app_sys('app', 'www', 'md', 'reference.md'))
  )
}

#' settings_reference Server Functions
#'
#' @noRd
mod_settings_reference_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_settings_reference_ui("settings_reference_1")

## To be copied in the server
# mod_settings_reference_server("settings_reference_1")
