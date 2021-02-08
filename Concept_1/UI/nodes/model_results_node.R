results_node <- tabPanel(
  title = "Results", 
  tabPanel("Model", 
           sidebarLayout(
             sidebarPanel(
               h4('Model results'),
               tags$button(type = 'button',
                           class = 'btn btn-default help',
                           onclick = "openConceptsPage('Concept2')",
                           'Help me')
             ),
             mainPanel()
           )))

