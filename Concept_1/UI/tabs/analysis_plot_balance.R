analysis_plot_balance <- tabPanel(title = "Balance Plots",
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 4,
                                      h4("Visualize Balance Between Treatment and Control"),
                                      selectInput(
                                        inputId = "analysis_plot_balance_select_var",
                                        label = "Select Variables for Balance Check:",
                                        multiple = TRUE,
                                        choices = X_names,
                                        selected = X_names
                                      ),
                                      br(),
                                      tags$button(type = 'button',
                                                  class = 'btn btn-default',
                                                  onclick = "openConceptsPage('Concept3')",
                                                  'What is this plot telling me?'),
                                      br(),br(),
                                      actionButton(inputId = "analysis_plots_balance_button_next",
                                                   label = "Next")
                                      
                                      # add advanced option to remove scale
                                    ),
                                    mainPanel(
                                      width = 8,
                                      br(),
                                      plotOutput("analysis_plot_balance_plot", height = 500)
                                    )
                                  ))