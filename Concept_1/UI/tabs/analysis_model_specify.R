analysis_model_specify <- tabPanel("Model", 
                       sidebarLayout(
                         sidebarPanel(
                           h4('Specify Model'), 
                           radioButtons(inputId = "estimand", 
                                        label = "Select Causal Estimand", 
                                        choices = c('ATE', 'ATT', 'ATC'), 
                                        selected = 1), 
                           radioButtons(inputId = "comm_sup", 
                                        label = "Common Support Rule", 
                                        choices = c('None', 'Standard Deviation', 'Chi Squared Test'))
                         ),
                         mainPanel(
                           includeMarkdown('UI/markdowns/estimands.md')
                         )))