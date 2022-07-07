#' set module IDs
#'
#' @description A utils function
#'
#' @return a list of the module ids
#'
#' @noRd
module_ids <- list(
  learning = list(
    randomization = 'learning_randomization',
    post_treatment = 'learning_post_treatment',
    estimands = 'learning_estimands',
    bias_efficiency = 'learning_bias_efficiency',
    rct_covariates = 'learning_rct_covariates',
    ignorability = 'learning_ignorability',
    confounders_measured = 'learning_confounders_measured',
    potential_outcomes = "learning_potential_outcomes"
  ),
  analysis = list(
    design = 'analysis_design',
    data = 'analysis_upload_data',
    verify = 'analysis_verify_data',
    eda = 'analysis_eda',
    balance = 'analysis_balance',
    overlap = 'analysis_overlap',
    model = 'analysis_model',
    diagnostic = 'analysis_diagnostic',
    results = 'analysis_results',
    subgroup = 'analysis_subgroup'
  )
)

set.seed(2)

# increase maximum file upload size limit to 10mb
options(shiny.maxRequestSize = 10*1024^2)
