analysis_header <- navbarMenu(
  title = 'Analyze',
  ui_design(id = module_ids$analysis$design),
  ui_data(id = module_ids$analysis$data),
  ui_verify(id = module_ids$analysis$verify),
  ui_eda(id = module_ids$analysis$eda),
  ui_balance(id = module_ids$analysis$balance),
  ui_overlap(id = module_ids$analysis$overlap),
  ui_model(id = module_ids$analysis$model),
  ui_diagnostic(id = module_ids$analysis$diagnostic),
  ui_results(id = module_ids$analysis$results),
  ui_subgroup(id = module_ids$analysis$subgroup)
)
