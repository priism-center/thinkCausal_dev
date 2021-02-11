model_node <- tabPanel(title = "Model",
                       tabPanel("Model",
                                sidebarLayout(
                                  sidebarPanel(
                                    h4('Specify Model'),
                                    awesomeRadio(inputId = "analysis_model_radio_design", 
                                                 label = 'Select Assignment of Treatment (Z):', 
                                                 choices = c('Non-Random (Observational)' = 'non_random', 
                                                             'Random (Experimental)' = 'random', 
                                                             'Quasi-Random (Natural Experiment)' = 'quasi')),
                                    awesomeRadio(
                                      inputId = "analysis_model_radio_estimand",
                                      label = "Select Causal Estimand",
                                      choices = c('ATE', 'ATT', 'ATC'),
                                      selected = 1
                                    ),
                                    awesomeRadio(
                                      inputId = "analysis_model_radio_support",
                                      label = "Common Support Rule",
                                      choices = c('None' = "none", 
                                                  'Standard Deviation' = 'sd', 
                                                  'Chi Squared Test' = 'chisq')
                                    ),
                                    HTML('<details><summary>Advanced options</summary>'),
                                    br(),
                                    awesomeRadio(
                                      inputId = "",
                                      label = "model outcome with",
                                      choices = c('BART', "Weighted Propensity Score", 'TMLE'), 
                                      selected = 'BART'
                                    ),
                                    awesomeRadio(
                                      inputId = "",
                                      label = "model propensity score fit with:",
                                      choices = c('BART', 'generalized linear model','do not fit propensity scores'), 
                                      selected = 'BART'
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
                                                   label = "Fit model")
                                    )
                                  ),
                                  mainPanel(htmlOutput('analysis_model_summary'))
                                )))
