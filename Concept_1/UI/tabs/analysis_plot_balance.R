analysis_plot_balance <- tabPanel(title = "Balance Plots",
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 4,
                                      h4("Visualize Balance Between Treatment and Control"),
                                      checkboxInput(inputId = "all_balance", 
                                                    label = "Include All Variables", 
                                                    value = TRUE),
                                      conditionalPanel(
                                        "input.all_balance == false",
                                        selectInput(
                                          "balance_var",
                                          label = "Select Variables for Blance Check:",
                                          multiple = T,
                                          choices = X_names,
                                          selected = X_names
                                        )
                                      ),
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