analysis_header <- navbarMenu(
  title = 'Analyze',
  ui_design(id = 'analysis_design'),
  ui_data(id = 'analysis_data'),
  eda_page,
  model_page,
  diagnostics_page,
  results_page, 
  moderator_page
)
