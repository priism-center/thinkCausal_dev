analysis_header <- navbarMenu(
  title = 'Analyze',
  ui_design(id = module_ids$analysis$design),
  ui_data(id = module_ids$analysis$data),
  ui_eda(id = module_ids$analysis$eda),
  ui_model(id = module_ids$analysis$model),
  ui_diagnostic(id = module_ids$analysis$diagnostic),
  ui_results(id = module_ids$analysis$results),
  moderator_page
)
