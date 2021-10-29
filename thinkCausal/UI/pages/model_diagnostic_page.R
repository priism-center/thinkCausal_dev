diagnostics_page <- tabPanel(
  title = "Model diagnostics", #htmlOutput("exploration_tab_name"),
  tabPanel(
    title = "Model diagnostics",
    sidebarLayout(
      sidebarPanel(
        h4('Model diagnostics'),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
        br(),
        tags$button(type = 'button',
                    class = 'btn btn-default help',
                    onclick = "openHelpPage('Concept2')",
                    'What are these plots telling me?'),
        br(),br(),
        uiOutput(outputId = 'analysis_diagnosis_buttons_ui'),
        br(),
        create_progress_bar(6/7*100)
      ),
      mainPanel(
        tabsetPanel(
          id = "analysis_diagnostics_tabs",
          tabPanel(
            title = "Trace plot",
            br(),
            plotOutput('analysis_diagnostics_plot_trace',
                       height = 500)
          ),
          tabPanel(
            title = 'Common support',
            br(),
            plotOutput('analysis_diagnostics_plot_support',
                       height = 600)
          )
        )
      )
    )
  ))