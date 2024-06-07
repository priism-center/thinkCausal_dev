#' set module IDs
#'
#' @description A utils function
#'
#' @return a list of the module ids
#'
#' @noRd
module_ids <- list(
  home = 'home',
  learn = list(
    home = 'learning_home',
    randomization = 'learning_randomization',
    post_treatment = 'learning_post_treatment',
    selection = 'learning_variable_selection',
    colinearity = 'learning_colinearity',
    estimands = 'learning_estimands',
    estimands2 = 'learning_estimands2',
    obs_ignorability = 'learning_obs_ignorability',
    # versionA = 'learn_versionA',
    # versionB = 'learn_versionB',
    # fundamental = 'learning_fundamental',
    # rand_dist = 'learning_randomization_dist',
    bias_efficiency = 'learning_bias_efficiency',
    rct_covariates = 'learning_rct_covariates',
    confounders_measured = 'learning_confounders_measured',
    potential_outcomes = "learning_potential_outcomes",
    observational = "learning_observational",
    confounder = "learning_confounder"
  ),
  analysis = list(
    design = 'analysis_design',
    upload = 'analysis_upload_data',
    question = 'analysis_causal_question',
    selection = 'analysis_variable_selection',
    verify = 'analysis_verify_data',
    visualize = 'analysis_eda',
    balance = 'analysis_balance',
    overlap = 'analysis_overlap',
    model = 'analysis_model',
    diagnostic = 'analysis_diagnostic',
    results = 'analysis_results',
    subgroup = 'analysis_subgroup'
  ),
  reproduce = 'reproduce',
  settings = list(
    options = 'settings_options',
    reference = 'settings_reference',
    about = 'settings_about'
  )
)

set.seed(2)

# increase maximum file upload size limit to 10mb
options(shiny.maxRequestSize = 10*1024^2)
