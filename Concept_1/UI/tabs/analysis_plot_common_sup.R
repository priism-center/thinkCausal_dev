analysis_plot_common_sup <- tabPanel("Common Support Plots",
                                     sidebarLayout(sidebarPanel(
                                       width = 4,
                                       h4(
                                         "Check Common Support"
                                       ),
                                       h6("With Selected Variables(s)"), 
                                      selectInput(inputId = "sup_var", 
                                                  label = "Choose Confounder", 
                                                  choices = NULL), 
                                      h6("With Dimention Reduction"), 
                                      checkboxInput(inputId = "dim.red", 
                                                    label = "Dimention Reduction", FALSE)), 
                                      mainPanel()
                                     ))