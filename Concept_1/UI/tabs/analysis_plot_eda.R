analysis_plot_eda <- tabPanel(
  title = "Descriptive Plots",
  absolutePanel(id = "analysis_plots_descriptive_loading_message",
                br(),
                HTML("Data must be first uploaded and columns selected."),
                style = "z-index: -2;"),
  uiOutput(outputId = 'analysis_plots_descriptive_eda_module')
  )
