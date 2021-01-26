# concepts_dropdown <- navbarMenu(title = 'Concepts',
#                                 tabPanel(
#                                   title = "Concepts",
#                                   includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))
#                                 )
# )

concepts_dropdown <- tabPanel(title = "Concepts",
                              includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))
                              )