diagnostics_node <- tabPanel(
  title = "Model diagnostics", #htmlOutput("exploration_tab_name"),
  tabPanel(
    title = "Model diagnostics",
    sidebarLayout(
      sidebarPanel(
        h4('Model Diagnostics'),
        tags$button(type = 'button',
                    class = 'btn btn-default help',
                    onclick = "openConceptsPage('Concept2')",
                    'Help me'),
      ),
      mainPanel()
    )
  ))