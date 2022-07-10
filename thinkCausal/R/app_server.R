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
    session_global = session,
    log = list(as.character(Sys.time())),
    module_ids = module_ids,
    page_history = NULL,
    js = NULL
  )


  # modules -----------------------------------------------------------------

  # other
  mod_home_server('home', store)
  store <- mod_settings_options_server('settings_options', store)
  mod_settings_reference_server('settings_reference')
  mod_settings_about_server('settings_about')

  # learn
  mod_learn_server(id = 'learn', store = store)
  mod_learn_estimands_server('learn_estimands')
  mod_learn_post_treatment_server('learn_post_treatment')

  # analysis
  store <- mod_analysis_design_server('analysis_design', store)
  store <- mod_analysis_upload_server("analysis_upload", store)
  store <- mod_analysis_verify_server("analysis_verify", store)
  store <- mod_analysis_visualize_server("analysis_visualize", store)
  store <- mod_analysis_balance_server("analysis_balance", store)
  store <- mod_analysis_overlap_server("analysis_overlap", store)
  store <- mod_analysis_model_server("analysis_model", store)
  store <- mod_analysis_diagnostics_server("analysis_diagnostics", store)
  store <- mod_analysis_results_server("analysis_results", store)
  store <- mod_analysis_subgroup_server("analysis_subgroup", store)

}
