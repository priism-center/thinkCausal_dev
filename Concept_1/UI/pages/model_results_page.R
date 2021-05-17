results_page <- tabPanel(
  title = "Results", 
  tabPanel("Model", 
           sidebarLayout(
             sidebarPanel(
               h4('Model results'),
               htmlOutput('analysis_results_table_summary'),
               h5("Results interpretation:"),
              awesomeRadio(inputId = 'interpretation', 
                           label = NULL,
                           choices = c('Causal', 'Non-Causal'), 
                           selected = 'Causal', 
                           inline = T),
              conditionalPanel(condition = "input.interpretation == 'Causal'", 
                               p("The IHDP[auto fill] led to an increase of 4.2 points[auto fill] for students[auto fill] in this study"),
              ),
              
              conditionalPanel(condition = "input.interpretation == 'Non-Causal'", 
                               p("Students who participated in the IHDP scored 4.2 points higher, on average, than a simmilar group of students in the study who did not participate in the program. Simmilarity is conceptualized with respect to all covirates included in the analysis."),
              ),
              awesomeRadio(inputId = "plot_result_type", 
                           label = "Plot:", 
                           choices = c('Sample', 'Individual'), 
                           selected = 'Sample',
                           inline = T),
              
              awesomeRadio(inputId = "plot_result_style", 
                           label = NULL, 
                           choices = c('Histigram', 'Density'), 
                           selected = 'Density',
                           inline = T),
              
              conditionalPanel(condition = "input.plot_result_type == 'Sample'",
              awesomeCheckboxGroup(inputId = 'central_tendency', 
                              label = "Show:", 
                              inline = T, 
                              choices = c('Mean', 'Median'), 
                              selected = 0),
              awesomeCheckboxGroup(inputId = 'show_interval', 
                                   label = NULL, 
                                   inline = T, 
                                   choices = list('80% ci' = .8, '95% ci' = .95), 
                                   selected = 'none')),
              awesomeRadio(inputId = 'show_reference', 
                           label = 'Include reference line:', 
                           choices = c('Yes', 'No'),
                           inline = T, 
                           selected = 'No'), 
              conditionalPanel(condition = "input.show_reference == 'Yes'", 
                               numericInput(inputId = "reference_bar", 
                                            label = "Reference Number", 
                                            value = 0, 
                                            step = 1),),
              
               br(),
               downloadButton(outputId = "analysis_results_button_download",
                              label = "Download script"),
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
               # br(),
               # tabsetPanel(
               #   id = "analysis_results_tabs",
               #   tabPanel(
               #     title = "Estimated Treatment Effect",
               #     br(),
                  conditionalPanel(condition = "input.plot_result_type == 'Sample'", 
                                   plotOutput(outputId = 'analysis_results_plot_PATE',
                                              height = 500)), 
                  conditionalPanel(condition = "input.plot_result_type == 'Individual'", 
                                   plotOutput(outputId = 'analysis_results_plot_ITE',
                                              height = 500))
                   
             #   )
             # )
           )
          )
        )
  )

