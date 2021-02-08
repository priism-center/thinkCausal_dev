model_node <- tabPanel(title = "Model",
                       tabPanel("Model",
                                sidebarLayout(
                                  sidebarPanel(
                                    h4('Specify Model'),
                                    awesomeRadio(
                                      inputId = "estimand",
                                      label = "Select Causal Estimand",
                                      choices = c('ATE', 'ATT', 'ATC'),
                                      selected = 1
                                    ),
                                    awesomeRadio(
                                      inputId = "comm_sup",
                                      label = "Common Support Rule",
                                      choices = c('None', 'Standard Deviation', 'Chi Squared Test')
                                    ),
                                    HTML('<details><summary>Advanced options</summary>'),
                                    br(),
                                    awesomeRadio(
                                      inputId = "example1",
                                      label = "Example 1",
                                      choices = 1:3
                                    ),
                                    awesomeRadio(
                                      inputId = "example2",
                                      label = "Example 2",
                                      choices = 1:3
                                    ),
                                    awesomeRadio(
                                      inputId = "example3",
                                      label = "Example 3",
                                      choices = 1:3
                                    ),
                                    br(),
                                    HTML('</details><br>'),
                                    br(),
                                    tags$button(
                                      type = 'button',
                                      class = 'btn btn-default help',
                                      onclick = "openConceptsPage('Concept3')",
                                      'Help me'
                                    ),
                                    br(),
                                    br(),
                                    actionButton(inputId = "analysis_model_button_run_model",
                                                 label = "Run model"),
                                  ),
                                  mainPanel(includeMarkdown('UI/markdowns/estimands.md'))
                                )))
