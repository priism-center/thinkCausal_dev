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

  # other
  store <- mod_settings_options_server('settings_options', store)

  # learn
  mod_learn_server('learn')
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


  # links -------------------------------------------------------------------

  # example of how to change tabs from the server
  # bs4Dash::updateTabItems(session, inputId = 'sidebar', selected = 'learn_estimands')

  # links from home to learn home page and analysis
  observeEvent(input[[NS('home')('learn_img')]], {
    bs4Dash::updateTabItems(session, inputId = 'sidebar', selected = 'learn')
  })
  observeEvent(input[[NS('home')('analysis_img')]], {
    bs4Dash::updateTabItems(session, inputId = 'sidebar', selected = 'describe')
  })

  # links from learn home page to each learn article
  selectors <- c('learn_estimands', 'learn_post_treatment')
  purrr::map(selectors, function(sel){
    observeEvent(input[[NS('learn')(glue::glue('{sel}_img'))]], {
      bs4Dash::updateTabItems(session, inputId = 'sidebar', selected = sel)
    })
  })

}
