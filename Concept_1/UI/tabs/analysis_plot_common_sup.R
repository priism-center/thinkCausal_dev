analysis_plot_common_sup <- tabPanel(
  title = "Common Support Plots",
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Check Common Support"),
      h6("With Selected Variables(s)"),
      selectInput(
        inputId = "sup_var",
        label = "Choose Confounders to Plot",
        choices = X_names,
        multiple = TRUE,
      ),
      h6("With Dimension Reduction"),
      checkboxInput(inputId = "dim.red",
                    label = "Dimension Reduction", FALSE),
      br(),br(),
      actionButton(inputId = "analysis_plots_support_button_next",
                   label = "Next")
  ),
  mainPanel()))