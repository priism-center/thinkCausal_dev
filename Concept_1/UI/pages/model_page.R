model_page <- tabPanel(title = "Model",
                       tabPanel("Model",
                                sidebarLayout(
                                  sidebarPanel(
                                    h4('Specify Model'),
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
                                      label = "Select Causal Estimand",
                                      choices = c('ATE', 
                                                  'ATT', 
                                                  'ATC', 
                                                  'Unsure of Estimand' = 'unsure'),
                                      selected = 5
                                    ),
                                    htmlOutput(outputId = 'analysis_model_text_estimand_noinput'),
                                    awesomeRadio(
                                      inputId = "analysis_model_radio_support",
                                      label = "Common Support Rule",
                                      choices = c('None' = "none", 
                                                  'Standard Deviation' = 'sd', 
                                                  'Chi Squared Test' = 'chisq', 
                                                  'Unsure of Common Support Rule' = 'unsure'),
                                      selected = 5
                                    ),
                                    h4('Specify Moderators'),
                                    awesomeRadio(
                                      inputId = "analysis_model_moderator_yes_no", 
                                      inline = T,
                                      label = "Pre-Specify Moderation Tests", 
                                      choices = c('Yes', 'No'), 
                                      selected = 'No'
                                    ),
                                    conditionalPanel(
                                      condition = "input.analysis_model_moderator_yes_no == 'Yes'", 
                                      selectInput(inputId = "analysis_model_moderator_vars",
                                                  label = "Select Moderators From Covariates",
                                                  choices = NULL,
                                                  multiple = TRUE)
                                    ), 
                                    br(),br(),

                                    htmlOutput(outputId = 'analysis_model_text_support_noinput'),
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
                                    ),
                                    br(),
                                    create_progress_bar(5/7*100)
                                  ),
                                  mainPanel(htmlOutput('analysis_model_summary'))
                                )))
