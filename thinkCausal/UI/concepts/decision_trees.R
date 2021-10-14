# this defines the decision trees page under Concepts

decision_trees_concept <- tagList(
  includeMarkdown(file.path("UI", "markdowns", 'concepts_decision_trees_text_1.md')),
  br(),br(),br(),
  tags$iframe(src = 'concepts/decision_trees/index.html', height = 1000, width = '100%'),
  br(),br(),br(),
  includeMarkdown(file.path("UI", "markdowns", 'concepts_decision_trees_text_2.md'))
)
