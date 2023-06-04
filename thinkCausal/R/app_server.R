#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # close loading spinner
  Sys.sleep(1.5) # prevent flashing
  waiter::waiter_hide()


  # global store ------------------------------------------------------------

  # initialize list to store variables
  store <- reactiveValues(
    session_global = session,
    log = list(as.character(Sys.time())),
    # js = NULL,
    options = list(theme_custom = theme_minimal_no_transparency())
  )


  # JavaScript initiated functions ------------------------------------------

  # move page when JS says so
  # usually triggered by links in the help slideover
  observeEvent(input$js_open_page, {
    new_page <- input$js_open_page$page
    toggle_help <- input$js_open_page$toggleHelp
    if (isTRUE(toggle_help)) bs4Dash::updateControlbar(id = "help-slideover", session = session)
    bs4Dash::updateTabItems(session, inputId = 'sidebar', selected = new_page)
  })


  # modules -----------------------------------------------------------------

  # other
  mod_home_server(module_ids$home, store)
  mod_reproduce_server(module_ids$reproduce, store)
  store <- mod_settings_options_server(module_ids$settings$options, store)
  mod_settings_reference_server(module_ids$settings$reference)
  mod_settings_about_server(module_ids$settings$about)

  # learn
  mod_learn_server(module_ids$learn$home, store)
  mod_learn_estimands_server(module_ids$learn$estimands)
  mod_learn_estimands2_server(module_ids$learn$estimands2)
  mod_learn_fundamental_server(module_ids$learn$fundamental)
  mod_learn_rct_analysis_server(module_ids$learn$randomization)
  mod_learn_post_treatment_server(module_ids$learn$post_treatment, store)
  mod_learn_potential_outcomes_server(module_ids$learn$potential_outcomes)
  mod_learn_obs_analysis_server(module_ids$learn$observational)
  mod_learn_balance_server(module_ids$learn$balance)

  mod_learn_scrolly_example_server('learn_scrolly')

  # analysis
  store <- mod_analysis_upload_server(module_ids$analysis$upload, store)
  store <- mod_analysis_variable_selection_server(module_ids$analysis$select, store)
  store <- mod_analysis_verify_server(module_ids$analysis$verify, store)
  store <- mod_analysis_visualize_server(module_ids$analysis$visualize, store)
  store <- mod_analysis_balance_server(module_ids$analysis$balance, store)
  store <- mod_analysis_overlap_server(module_ids$analysis$overlap, store)
  store <- mod_analysis_model_server(module_ids$analysis$model, store)
  store <- mod_analysis_diagnostics_server(module_ids$analysis$diagnostics, store)
  store <- mod_analysis_results_server(module_ids$analysis$results, store)
  store <- mod_analysis_subgroup_server(module_ids$analysis$subgroup, store)


  # other -------------------------------------------------------------------

  # mobile popup warning
  # TODO: this can be removed for native installation
  observe(if (isTRUE(shinybrowser::is_device_mobile())) show_popup_mobile(session))

  # toggle side bar help menu
  # bs4Dash::updateControlbar(id = "help-slideover", session = session)
}
