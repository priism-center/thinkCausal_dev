model_node <- tabPanel(title = "Model",
                       tabPanel("Model",
                                sidebarLayout(
                                  sidebarPanel(
                                    h4('Specify Model'),
                                    awesomeRadio(inputId = "analysis_model_radio_design", 
                                                 label = 'Select Assignment of Treatment (Z):', 
                                                 choices = c('Non-Random (Observational)', 
                                                             'Random (Experimental)', 
                                                             'Quasi-Random (Natural Experiment)')),
                                    awesomeRadio(
                                      inputId = "analysis_model_radio_estimand",
                                      label = "Select Causal Estimand",
                                      choices = c('ATE', 'ATT', 'ATC'),
                                      selected = 1
                                    ),
                                    awesomeRadio(
                                      inputId = "analysis_model_radio_support",
                                      label = "Common Support Rule",
                                      choices = c('None', 'Standard Deviation', 'Chi Squared Test')
                                    ),
                                    HTML('<details><summary>Advanced options</summary>'),
                                    br(),
                                    awesomeRadio(
                                      inputId = "",
                                      label = "Use weighted propensity scores",
                                      choices = 1:3
                                    ),
                                    awesomeRadio(
                                      inputId = "",
                                      label = "TMLE adjustment",
                                      choices = 1:3
                                    ),
                                    awesomeRadio(
                                      inputId = "",
                                      label = "Propensity score fit model",
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
                                    br(),br(),
                                    div(
                                      class = 'backNextContainer',
                                      actionButton(inputId = "analysis_model_button_back",
                                                   label = "Back to EDA"),
                                      actionButton(inputId = "analysis_model_button_next",
                                                   label = "Run model")
                                    )
                                  ),
                                  mainPanel(includeMarkdown('UI/markdowns/estimands.md'))
                                )))
