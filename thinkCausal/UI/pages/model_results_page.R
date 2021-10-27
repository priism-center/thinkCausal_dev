results_page <- tabPanel(
  title = "Results", 
  tabPanel("Model Results", 
           sidebarLayout(
             sidebarPanel(
               
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
              awesomeRadio(inputId = "plot_result_style", 
                           label = "Plot:", 
                           choices = c('Histogram', 'Density'), 
                           selected = 'Density',
                           inline = T),
              awesomeCheckboxGroup(inputId = 'central_tendency', 
                              label = NULL, 
                              inline = T, 
                              choices = c('Mean', 'Median'), 
                              selected = 0),
              awesomeCheckboxGroup(inputId = 'show_interval', 
                                   label = NULL, 
                                   inline = T, 
                                   choices = list('80% ci' = .8, '95% ci' = .95), 
                                   selected = 'none'),
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
              tags$button(type = 'button',
                          class = 'btn btn-default help',
                          onclick = "openConceptsPage('Concept2')",
                          'What are these plots telling me?'),
              br(), br(),
              actionButton(inputId = "analysis_results_button_back",
                           label = "See diagnostics"),
              br(),br(),
              actionButton(inputId = "analysis_results_button_subgroup",
                           label = "See results by subgroups"),
              br(),br(),
               create_progress_bar(7/7*100)
             ),
             mainPanel(

                 plotOutput(outputId = 'analysis_results_plot_PATE',
                            height = 400),

                  h4('Model results'),
                  # htmlOutput('analysis_results_table_summary')
                 DT::dataTableOutput('analysis_results_table_summary')
                   
             #   )
             # )
           )
          )
        )
  )

