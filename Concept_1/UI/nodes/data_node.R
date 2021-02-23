data_node <- tabPanel(
  title = 'Data', 
         tabsetPanel(
           id = "analysis_data_tabs",
           tabPanel(title = "Load", 
                    fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        # find and select input file
                        h4("Upload Your Data:"),
                        div(id = "upload_file_div",
                          fileInput(inputId = "analysis_data_upload", 
                                    label = "Choose File",
                                    buttonLabel = 'Browse',
                                    multiple = FALSE,
                                    accept = c('.csv', '.txt', '.xlsx', '.dta', '.spss'),
                                    placeholder = 'csv, txt, xlsx, dta, or spss'),
                        ),
                        uiOutput(outputId = 'analysis_data_delim'),
                        awesomeCheckbox(inputId = "analysis_data_header", 
                                        label = "Data contains a header row", 
                                        value = TRUE),
                        awesomeCheckbox(inputId = "analysis_data_check_auto_convert", 
                                        label = "Auto convert logical columns", 
                                        value = FALSE),
                        awesomeCheckbox(inputId = 'analysis_data_modify',
                                      label = "Modify your data",
                                      value = FALSE
                                    ),
                        conditionalPanel(
                          condition = "input.analysis_data_modify == true",
                          uiOutput(outputId = 'analysis_data_modify_UI'),
                          actionButton(inputId = 'analysis_data_button_modify_save',
                                       label = 'Save modifications')
                        ),
                        # awesomeCheckbox(inputId = 'analysis_data_rename',
                        #               label = "Rename columns",
                        #               value = FALSE
                        #             ),
                        # awesomeCheckbox(inputId = 'analysis_data_changeDataTypes',
                        #                 label = "Change data types",
                        #                 value = FALSE
                        # ),
                        # conditionalPanel(
                        #   condition = "input.analysis_data_rename == true",
                        #   uiOutput(outputId = 'analysis_data_rename_UI'),
                        #   actionButton(inputId = 'analysis_data_button_rename_save',
                        #                label = 'Save column names')
                        # ),
                        # conditionalPanel(
                        #   condition = "input.analysis_data_changeDataTypes == true",
                        #   uiOutput(outputId = 'analysis_data_changeDataTypes_UI'),
                        #   actionButton(inputId = 'analysis_data_button_changeDataTypes_save',
                        #                label = 'Save data types')
                        # ),
                        br(),br(),
                        actionButton(inputId = 'create_practice',
                                     label = 'Create a Practice Data Set'),
                        
                        conditionalPanel('input.create_practice %2 != 0', 
                                         br(), 
                                         sliderInput(inputId = 'sim.diff', 
                                                     label = div(style='width:400px;', 
                                                                 div(style='float:left;', 'Least Difficult'), 
                                                                 div(style='float:right;', 'Most Difficult')), 
                                                      min = 1, max = 6, value = 1, width = '400px')), 
                        
                        br(), br(),
                        actionButton(inputId = "analysis_data_load_button_next",
                                     label = "Next")
                      ),
                      mainPanel(
                        br(),
                        tabsetPanel(
                          id = "analysis_data_tabs",
                          tabPanel(
                            title = "Your data",
                            DT::dataTableOutput('analysis_data_table')
                          ),
                          tabPanel(
                            title = 'Upload logs'
                          )
                        )
                      )
                    )
           ),
           # 
           # tabPanel("Transform Data", fluid = TRUE,
           #          h4("Pivoting Data Wide/Long or Transposing from row to column form"),
           #          sidebarLayout(
           #            sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
           #            mainPanel(
           #              # insert output
           #            )
           #          )
           # ),
           
           
           tabPanel(title = "Select Data", 
                    fluid = TRUE,
                    # hr('Indicate Treatment Variable, Outcome Variable and Confounders'),
                    sidebarLayout(
                      sidebarPanel(h4("Select Variables:"),  
                                   # Column Selection for Z, and identify treatment
                                   selectInput(inputId = "analysis_data_select_select_zcol", 
                                               label = "Select Treatment (Z) Column", 
                                               choices = NULL),
                                   # TODO: automate this with a smart default
                                   # selectInput(inputId = "analysis_data_select_select_treatment", 
                                   #             label = "Select the Value Representing Receiving Treatment",
                                   #             choices = list('TRUE FALSE', '0 1', 'Yes No')),      
                                   selectInput(inputId = "analysis_data_select_select_ycol", 
                                               label = "Select Response (Y) Column", 
                                               choices = NULL), 
                                   selectInput(inputId = "analysis_data_select_select_xcol", 
                                               label = "Select Covariates (X) Columns", 
                                               choices = NULL, 
                                               multiple = TRUE),
                                   actionButton(inputId = 'analysis_data_select_column_save',
                                                label = 'Save column assignments'),
                                   br(),br(),
                                   tags$button(type = 'button',
                                               class = 'btn btn-default help',
                                               onclick = "openConceptsPage('Concept3')",
                                               'Help me'),
                                   br(),br(),
                                   div(
                                     class = 'backNextContainer',
                                     actionButton(inputId = "analysis_data_select_button_back",
                                                  label = "Back"),
                                     actionButton(inputId = "analysis_data_select_button_next",
                                                  label = "Next"),
                                     ),
                                   br(),
                                   progress_bar(1/7*100)
                                   ), 
                      mainPanel(
                        br(),
                        tabsetPanel(
                          id = "analysis_data_select_tabs",
                          tabPanel(
                            title = "Your selected data",
                            DT::dataTableOutput('analysis_data_select_table')
                          ),
                          tabPanel(
                            title = 'TBD'
                          )
                        )
                      )
                    )
           ) 
           
           # tabPanel(title = "Study Design", 
           #          fluid = TRUE,
           #          sidebarLayout(
           #            sidebarPanel(width = 6, 
           #            h4("Indicate Study Design"),
           #            awesomeRadio(inputId = "analysis_data_design_radio_design", 
           #                         label = 'Select Assignment of Treatment (Z):', 
           #                         choices = c('Non-Random (Observational)', 
           #                                     'Random (Experimental)', 
           #                                     'Quasi-Random (Natural Experiment)')),
           #            br(),
           #            tags$button(type = 'button',
           #                        class = 'btn btn-default help',
           #                        onclick = "openConceptsPage('Concept3')",
           #                        'Help me'),
           #            br(),br(),
           #            div(
           #              class = 'backNextContainer',
           #              actionButton(inputId = "analysis_data_design_button_back",
           #                           label = "Back"),
           #              actionButton(inputId = "analysis_data_design_button_next",
           #                           label = "Next"),
           #            )
           #            ),
           #            mainPanel(
           #              # insert output
           #            )
           #          )
           # )
         ))
