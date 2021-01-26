analysis_plot_balance <- tabPanel("Balance Plots",
                                   sidebarLayout(sidebarPanel(
                                     h4("Visualize Balance Between Treatment and Control"), 
                                     checkboxInput(inputId = "all_balance", label = "Include All Variables", TRUE), 
                                     conditionalPanel("input.all_balance == false", 
                                                      selectInput("balance_var", 
                                                                  label = "Select Variables for Blance Check:", 
                                                                  multiple = T, 
                                                                  choices = NULL))
                                     
                                     # add advanced option to remove scale
                                   ), mainPanel()))