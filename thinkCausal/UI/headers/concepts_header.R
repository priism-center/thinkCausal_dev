concepts_header <- navbarMenu(
  title = 'Learn',
  concepts_page,
  "-----",
  tabPanel(title = "Test your understanding", 
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))), 
  tabPanel(title = 'PotentialOutcomes',
           PotentialOutcomesUI(id = "concepts_potentialoutcomes")),
  tabPanel(title = 'Randomization', 
           randomizationUI(id = "concepts_randomization")),
  # tabPanel(title = 'PO Test', 
  #          poUI(id = 'potential_outcomes_test')),
  tabPanel(title = "Fundamental problem",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))),
  tabPanel(title = "Assumptions",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))),
  tabPanel(title = "Regression methods",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md')))
  )