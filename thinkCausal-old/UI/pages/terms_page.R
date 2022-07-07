terms_page <- tabPanel(
  title = "Terms of use",
  style = "padding-left: 3rem;",
  includeMarkdown(file.path("UI", "markdowns", 'terms_of_use.md'))
)
