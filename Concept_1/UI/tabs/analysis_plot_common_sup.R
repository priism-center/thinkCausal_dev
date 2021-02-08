analysis_plot_common_sup <- tabPanel(
  title = "Common Support Plots",
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Check Common Support"),
      selectInput(
        inputId = "analysis_plot_overlap_select_var",
        label = "Select Variables for Overlap Check:",
        multiple = TRUE,
        choices = X_names,
        selected = X_names
      ),
      h6("With Dimension Reduction"),
      awesomeCheckbox(inputId = "dim.red",
                    label = "Dimension Reduction", FALSE),
      br(),
      tags$button(type = 'button',
                  class = 'btn btn-default help',
                  onclick = "openConceptsPage('Concept3')",
                  'What is this plot telling me?'),
      br(),br(),
      div(
        class = 'backNextContainer',
        actionButton(inputId = "analysis_plots_support_button_back",
                     label = "Back"),
        actionButton(inputId = "analysis_plots_support_button_next",
                     label = "Next")
      )
  ),
  mainPanel(
    width = 8,
    br(),
    plotOutput(outputId = "analysis_plot_overlap_plot", 
               height = 800)
  )
  )
  )