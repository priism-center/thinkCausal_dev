analysis_model_diagostics <- tabPanel(
  title = "Model diagnostics",
  sidebarLayout(
    sidebarPanel(
      h4('Model Diagnostics'),
      tags$button(type = 'button',
                  class = 'btn btn-default',
                  onclick = "openNavPage('Concept2')",
                  'Help me'),
      ),
    mainPanel()
    )
  )
