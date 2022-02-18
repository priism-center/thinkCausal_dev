analysis_header <- navbarMenu(
  title = 'Analyze',
  ui_design(id = 'analysis_design'),
  ui_data(id = 'analysis_data'),
  ui_eda(id = 'analysis_eda'),
  ui_model(id = 'analysis_model'),
  ui_diagnostic(id = 'analysis_diagnostic'),
  results_page, 
  moderator_page
)
