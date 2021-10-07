# model_page <- tabPanel(title = 'Model',
#                        tabPanel('Model', 
#                                 h4('Specify variables'), 
#                                 wellPanel(
#                                   textInput('treatment_name', label = 'What is the name of the treatment or intervention?', placeholder = 'treatment'), 
#                                   textInput('treatment_name', label = 'What are the units of your outcome variable?', placeholder = 'units'), 
#                                   selectInput('analysis_model_moderator_yes_no', label = 'Would you like to pre-specify sub-group analyses', choices = c("", 'No', 'Yes', 'Learn more about sub-group analyses'))
#                                 ), 
#                                 h4('Specify model'), 
#                                 wellPanel(
#                                   selectInput('analysis_model_radio_estimand', label = 'Select causal estimand', choices = c("",'Unsure', 'ATE', 'ATT', 'ATC'))
#                                 )
#                                 )
#                        )


model_page <- tabPanel(title = "Model",
                       tabPanel("Model",
                                sidebarLayout(
                                  sidebarPanel(
                                    h4('Specify your model'),
                                    awesomeRadio(
                                      inputId = "analysis_model_radio_design",
                                      label = 'Select Assignment of Treatment (Z):',
                                      choices = c('Non-Random (Observational)' = 'non_random',
                                                  'Random (Experimental)' = 'random',
                                                  'Quasi-Random (Natural Experiment)' = 'quasi'),
                                      selected = 5
                                      ),
                                    htmlOutput(outputId = 'analysis_model_text_design'),
                                    htmlOutput(outputId = 'analysis_model_text_design_noinput'),
                                    awesomeRadio(
                                      inputId = "analysis_model_radio_estimand",
                                      label = "Select causal estimand",
                                      choices = c('ATE',
                                                  'ATT',
                                                  'ATC',
                                                  'Unsure of Estimand' = 'unsure'),
                                      selected = 5
                                    ),
                                    htmlOutput(outputId = 'analysis_model_text_estimand_noinput'),
                                    awesomeRadio(
                                      inputId = "analysis_model_radio_support",
                                      label = "Select common support rule",
                                      choices = c('None' = "none",
                                                  'Standard Deviation' = 'sd',
                                                  'Chi Squared Test' = 'chisq',
                                                  'Unsure of Common Support Rule' = 'unsure'),
                                      selected = 5
                                    ),
                                    br(),
                                    h4('Specify moderators'),
                                    awesomeRadio(
                                      inputId = "analysis_model_moderator_yes_no",
                                      inline = TRUE,
                                      label = "Pre-specify moderation tests",
                                      choices = c('Yes', 'No'),
                                      selected = 'No'
                                    ),
                                    conditionalPanel(
                                      condition = "input.analysis_model_moderator_yes_no == 'Yes'",
                                      selectInput(inputId = "analysis_model_moderator_vars",
                                                  label = "Select moderators from covariates",
                                                  choices = NULL,
                                                  multiple = TRUE)
                                    ),
                                    br(),
                                    htmlOutput(outputId = 'analysis_model_text_support_noinput'),
                                    HTML('<details><summary>Advanced options</summary>'),
                                    br(),
                                    awesomeRadio(
                                      inputId = "analysis_model_outcome",
                                      label = "Model outcome with",
                                      choices = c('BART', "Weighted Propensity Score", 'TMLE'),
                                      selected = 'BART'
                                    ),
                                    awesomeRadio(
                                      inputId = "analysis_model_pscore",
                                      label = "Model propensity score fit with:",
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
                                    ),
                                    br(),
                                    create_progress_bar(5/7*100)
                                  ),
                                  mainPanel(htmlOutput('analysis_model_summary'))
                                )))
