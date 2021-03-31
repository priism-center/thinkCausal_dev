results_page <- tabPanel(
  title = "Results", 
  tabPanel("Model", 
           sidebarLayout(
             sidebarPanel(
               h4('Model results'),
               htmlOutput('analysis_results_table_summary'),
               h5("Results interpretation"),
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
               br(),
               downloadButton(outputId = "analysis_results_button_download",
                              label = "Download log"),
               br(), br(),
               tags$button(type = 'button',
                           class = 'btn btn-default help',
                           onclick = "openConceptsPage('Concept2')",
                           'What are these plots telling me?'),
               br(), br(),
               actionButton(inputId = "analysis_results_button_back",
                            label = "Diagnostics"),
               br(),br(),
               create_progress_bar(7/7*100)
             ),
             mainPanel(
               br(),
               tabsetPanel(
                 id = "analysis_results_tabs",
                 tabPanel(
                   title = "Individual treatment effects",
                   br(),
                   plotOutput(outputId = 'analysis_results_plot_ITE',
                              height = 500)
                 ),
                 tabPanel(
                   title = 'Conditional individual treatment effects',
                   br(),
                   plotOutput(outputId = 'analysis_results_plot_CITE',
                              height = 500)
                 )
               )
             )
           )))

