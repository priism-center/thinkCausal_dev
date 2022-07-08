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

  # analysis
  store <- mod_analysis_design_server('analysis_design', store)

  # learn
  mod_learn_server('learn')
  mod_learn_estimands_server('learn_estimands')
  mod_learn_post_treatment_server('learn_post_treatment')


  # example of how to change tabs from the server
  # bs4Dash::updateTabItems(session, inputId = 'sidebar', selected = 'learn_estimands')



  # learn links -------------------------------------------------------------

  selectors <- c('learn_estimands', 'learn_post_treatment')
  purrr::map(selectors, function(sel){
    observeEvent(input[[NS('learn')(glue::glue('{sel}_img'))]], {
      bs4Dash::updateTabItems(session, inputId = 'sidebar', selected = sel)
    })
  })

}
