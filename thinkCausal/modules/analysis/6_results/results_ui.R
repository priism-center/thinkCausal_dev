
ui_results <- function(store, id){
  ns <- NS(id)
  tabPanel(
    title = "Results",
    value = id,
    tabPanel("Model Results",
             sidebarLayout(
               sidebarPanel(
                 h4("Results"),
                 h5("Results interpretation:"),
                 radioButtons(inputId = ns('interpretation'),
                              label = NULL,
                              choices = c('Causal', 'Non-Causal'),
                              selected = 'Causal',
                              inline = T),
                 # conditionalPanel(condition = "input.interpretation == 'Causal'",
                 #                  p("The IHDP[auto fill] led to an increase of 4.2 points[auto fill] for students[auto fill] in this study"),
                 # ),
                 #
                 # conditionalPanel(condition = "input.interpretation == 'Non-Causal'",
                 #                  p("Students who participated in the IHDP scored 4.2 points higher, on average, than a simmilar group of students in the study who did not participate in the program. Simmilarity is conceptualized with respect to all covirates included in the analysis."),
                 # ),
                 radioButtons(inputId = ns("plot_result_style"),
                              label = "Plot:",
                              choices = c('Histogram', 'Density'),
                              selected = 'Density',
                              inline = T),
                 checkboxGroupInput(inputId = ns('central_tendency'),
                                    label = NULL,
                                    inline = T,
                                    choices = c('Mean', 'Median'),
                                    selected = 0),
                 checkboxGroupInput(inputId = ns('show_interval'),
                                    label = NULL,
                                    inline = T,
                                    choices = list('80% ci' = .8, '95% ci' = .95),
                                    selected = 'none'),
                 radioButtons(inputId = ns('show_reference'),
                              label = 'Include reference line:',
                              choices = c('Yes', 'No'),
                              inline = T,
                              selected = 'No'),
                 conditionalPanel(condition = "input.show_reference == 'Yes'", ns = ns,
                                  numericInput(inputId = ns("reference_bar"),
                                               label = "Reference number",
                                               value = 0,
                                               step = 1),),
                 br(),
                 create_link_to_help('Results', button_label = 'What is this plot telling me?'),
                 br(), br(),
                 downloadButton(ns('download_PATE_plot'), label = "Download plot"),
                 br(), br(),
                 actionButton(inputId = ns("analysis_results_button_back"),
                              label = "See diagnostics"),
                 br(),br(),
                 actionButton(inputId = ns("analysis_results_button_subgroup"),
                              class = "nav-btn-focus",
                              label = "See results by subgroups"),
                 br(),br(),
                 create_progress_bar(7/7*100)
               ),
               mainPanel(
                 plotOutput(outputId = ns('analysis_results_plot_PATE'),
                            height = 400),
                 h4('Model results'),
                 DT::dataTableOutput(ns('analysis_results_table_summary')),
                 br(),
                 h4('Interpretation'),
                 textOutput(outputId = ns('results_text'))
               )
             )
    )
  )
  
}

