#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # global store ------------------------------------------------------------

  # initialize list to store variables
  store <- reactiveValues(
    log = list(as.character(Sys.time())),
    module_ids = module_ids,
    page_history = NULL,
    js = NULL
  )


  # modules -----------------------------------------------------------------

  store <- mod_settings_options_server('settings_options', store)
  store <- mod_analysis_design_server('analysis_design', store)

}
