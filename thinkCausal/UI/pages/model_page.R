model_page <- tabPanel(title = 'Model',
                       tabPanel('Model',
                                  fluidRow(
                                    column(6,
                                h4('Specify variables'),
                                wellPanel(
                                  textInput('treatment_name', 
                                            label = 'What is the name of the treatment or intervention?', 
                                            placeholder = 'treatment'),
                                  textInput('treatment_name', 
                                            label = 'What are the units of your outcome variable?', 
                                            placeholder = 'units'))
                                ),
                                column(6,  
                                h4('Specify design'), 
                                wellPanel(
                                  selectInput('anaylsis_design', 
                                              label = 'Indicate the study design', 
                                              choices = c("", 
                                                          "Unsure", 
                                                          'Observational', 
                                                          'Randomized Treatment', 
                                                          'Block Randomized Treatment', 
                                                          'Natural Experement')), 
                                  conditionalPanel(condition = "input.anaylsis_design == 'Observational'", 
                                                   selectInput('analysis_include_sens', 
                                                               label = 'Include Sensitivity Analysis', 
                                                               choices = c('Yes', 'No'))
                                                   ),
                                  conditionalPanel(condition = "input.anaylsis_design == 'Block Randomized Treatment'", 
                                                   selectInput('analysis_blocking_variable',
                                                               label = 'Select blocking variable',
                                                               choices = NULL)

                                  )
                              
                                )))),
                       
                                fluidRow(
                                  column(6, 
                                 
                                h4('Specify model'),
                                wellPanel(
                                  selectInput('analysis_model_estimand', 
                                              label = 'Select causal estimand', 
                                              choices = c("",'Unsure', 'ATC', 'ATE', 'ATT')), 
                                  selectInput('analysis_random_intercept',
                                              label = 'Random Intercept',
                                              choices = NULL),
                          
                                  selectInput('analysis_model_support', 
                                              label = 'Remove observations with weak common support', 
                                              choices = c("", 'Unsure', 'Yes', 'No')), 
                                  HTML('<details><summary>Advanced Modeling Options</summary>'),
                                  selectInput("analysis_over_ride_common_support", 
                                              label = 'Common Support rule:', 
                                              choices = c('standard deviation', 
                                                          'chi squared'))
                                  )), 
                                column(6, 
                                h4('Specify sub-group analyses'),
                                wellPanel(
                                selectInput('analysis_model_moderator_yes_no', 
                                            label = 'Would you like to pre-specify sub-group analyses?', 
                                            choices = c("No", "Yes",'Unsure'), 
                                            ),
                                conditionalPanel(condition = "input.analysis_model_moderator_yes_no == 'Yes'", 
                                                 selectInput('analysis_model_moderator_vars', 
                                                             label = 'Select moderator(s)', 
                                                             choices = NULL, 
                                                             multiple = T)),
                                
                                )
                       )), 
                       div(class = 'backNextContainer', 
                           style = "width:50%;",
                         actionButton(inputId = "analysis_model_button_next",
                                      label = "Fit model")
                       ),
                       br(), 
                       div(
                           class = 'backNextContainer',
                           style = "width:50%;", 
                           actionButton(inputId = "analysis_model_button_back",
                                        label = "Back to EDA")
                       ))

# 
# model_page <- tabPanel(title = "Model",
#                        tabPanel("Model",
#                                 sidebarLayout(
#                                   sidebarPanel(
#                                     h4('Specify your model'),
#                                     awesomeRadio(
#                                       inputId = "analysis_model_radio_design",
#                                       label = 'Select Assignment of Treatment (Z):',
#                                       choices = c('Non-Random (Observational)' = 'non_random',
#                                                   'Random (Experimental)' = 'random',
#                                                   'Quasi-Random (Natural Experiment)' = 'quasi'),
#                                       selected = 5
#                                       ),
#                                     htmlOutput(outputId = 'analysis_model_text_design'),
#                                     htmlOutput(outputId = 'analysis_model_text_design_noinput'),
#                                     awesomeRadio(
#                                       inputId = "analysis_model_radio_estimand",
#                                       label = "Select causal estimand",
#                                       choices = c('ATE',
#                                                   'ATT',
#                                                   'ATC',
#                                                   'Unsure of Estimand' = 'unsure'),
#                                       selected = 5
#                                     ),
#                                     htmlOutput(outputId = 'analysis_model_text_estimand_noinput'),
#                                     awesomeRadio(
#                                       inputId = "analysis_model_radio_support",
#                                       label = "Select common support rule",
#                                       choices = c('None' = "none",
#                                                   'Standard Deviation' = 'sd',
#                                                   'Chi Squared Test' = 'chisq',
#                                                   'Unsure of Common Support Rule' = 'unsure'),
#                                       selected = 5
#                                     ),
#                                     br(),
#                                     h4('Specify moderators'),
#                                     awesomeRadio(
#                                       inputId = "analysis_model_moderator_yes_no",
#                                       inline = TRUE,
#                                       label = "Pre-specify moderation tests",
#                                       choices = c('Yes', 'No'),
#                                       selected = 'No'
#                                     ),
#                                     conditionalPanel(
#                                       condition = "input.analysis_model_moderator_yes_no == 'Yes'",
#                                       selectInput(inputId = "analysis_model_moderator_vars",
#                                                   label = "Select moderators from covariates",
#                                                   choices = NULL,
#                                                   multiple = TRUE)
#                                     ),
#                                     br(),
#                                     htmlOutput(outputId = 'analysis_model_text_support_noinput'),
#                                     HTML('<details><summary>Advanced options</summary>'),
#                                     br(),
#                                     awesomeRadio(
#                                       inputId = "analysis_model_outcome",
#                                       label = "Model outcome with",
#                                       choices = c('BART', "Weighted Propensity Score", 'TMLE'),
#                                       selected = 'BART'
#                                     ),
#                                     awesomeRadio(
#                                       inputId = "analysis_model_pscore",
#                                       label = "Model propensity score fit with:",
#                                       choices = c('BART', 'generalized linear model','do not fit propensity scores'),
#                                       selected = 'BART'
#                                     ),
#                                     br(),
#                                     HTML('</details><br>'),
#                                     br(),
#                                     tags$button(
#                                       type = 'button',
#                                       class = 'btn btn-default help',
#                                       onclick = "openHelpPage('Concept3')",
#                                       'Help me'
#                                     ),
#                                     br(),br(),
#                                     div(
#                                       class = 'backNextContainer',
#                                       actionButton(inputId = "analysis_model_button_back",
#                                                    label = "Back to EDA"),
#                                       actionButton(inputId = "analysis_model_button_next",
#                                                    label = "Fit model")
#                                     ),
#                                     br(),
#                                     create_progress_bar(5/7*100)
#                                   ),
#                                   mainPanel(htmlOutput('analysis_model_summary'))
#                                 )))
