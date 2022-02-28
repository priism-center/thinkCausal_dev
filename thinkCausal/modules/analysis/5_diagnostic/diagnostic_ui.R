
ui_diagnostic <- function(store, id){
  ns <- NS(id)
  tabPanel(
    title = "Model diagnostics",
    value = module_ids$analysis$diagnostic,
    tabPanel(
      title = "Model diagnostics",
      sidebarLayout(
        sidebarPanel(
          h4('Model diagnostics'),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
          br(),
          create_link_to_help('Diagnostics', button_label = 'What are these plots telling me?'),
          br(),br(),
          downloadButton(ns('download_diagnostic_plot'), label = "Download plot"),
          br(), br(),
          uiOutput(outputId = ns('analysis_diagnosis_buttons_ui')),
          br(),
          create_progress_bar(6/7*100)
        ),
        mainPanel(
          tabsetPanel(
            id = ns("analysis_diagnostics_tabs"),
            tabPanel(
              title = "Trace plot",
              br(),
              plotOutput(ns('analysis_diagnostics_plot_trace'),
                         height = 500)
            ),
            tabPanel(
              title = 'Common support',
              br(),
              plotOutput(ns('analysis_diagnostics_plot_support'),
                         height = 600)
            )
          )
        )
      )
    ))
  
}

