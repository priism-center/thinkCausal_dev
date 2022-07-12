#' settings_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    includeMarkdown(app_sys('app', 'www', 'md', 'about.md'))
  )
}

#' settings_about Server Functions
#'
#' @noRd
mod_settings_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_settings_about_ui("settings_about_1")

## To be copied in the server
# mod_settings_about_server("settings_about_1")
