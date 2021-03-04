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
      awesomeRadio(inputId = "dim.red",
                    label = "View:", inline = T, 
                   choices = c("By Variables" = 1, 
                               "One Number Summary" = 2), 
                   selected = 1),
      br(),
      awesomeRadio(inputId = "overlap.type",
             label = "Plot Type:", inline = T, 
             choices = c('Histogram', 'Density'), 
             selected = 'Histogram'),
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
      ),
      br(),
      create_progress_bar(3/7*100)
  ),
  mainPanel(
    width = 8,
    br(),
    plotOutput(outputId = "analysis_plot_overlap_plot", height = 800)

  )
  )
  )
