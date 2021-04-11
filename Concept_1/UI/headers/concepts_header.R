concepts_header <- navbarMenu(
  title = 'Concepts',
  concepts_page,
  "-----",
  tabPanel(title = "Test your Understanding", 
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))), 
  tabPanel(title = 'Randomization', 
           randomizationUI(id = "concepts_randomization")),
  tabPanel(title = "Fundamental problem",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))),
  tabPanel(title = "Assumptions",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))),
  tabPanel(title = "Regression methods",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md')))
  )