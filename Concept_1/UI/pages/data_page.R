data_page <- tabPanel(
  title = 'Data', 
         tabsetPanel(
           id = "analysis_data_tabs",
           tabPanel(title = "Load",
                    fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel(
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
                            selectInput(inputId = "analysis_data_select_select_zcol",
                                        label = "Select Treatment (Z) Column",
                                        choices = NULL),
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
                        br(), br(),
                        actionButton(inputId = "analysis_data_load_button_next",
                                     label = "Next")
                      ),
                      mainPanel(
                        br(),
                        plotOutput(outputId = 'analysis_data_plot_DAG',
                                   height = '600px')
                      )
                    )
           ),
           tabPanel(title = "Select Data",
                    fluid = TRUE,
                    # hr('Indicate Treatment Variable, Outcome Variable and Confounders'),
                    sidebarLayout(
                      sidebarPanel(
                        div(
                          class = 'backNextContainer',
                          actionButton(inputId = 'analysis_data_text_reset',
                                       label = 'Reset variable changes'),
                          actionButton(inputId = 'analysis_data_save',
                                       label = 'Save changes')
                        ),
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
                                       label = "Next")
                          ),
                        br(),
                        create_progress_bar(1/7*100)
                      ),
                      mainPanel(
                        br(),
                        tabsetPanel(
                          id = "analysis_data_tabs",
                          tabPanel(
                            title = 'Variable view',
                            br(),
                            uiOutput(outputId = 'analysis_data_modify_UI')
                          ),
                          tabPanel(
                            title = "Data view",
                            DT::dataTableOutput('analysis_data_table')
                          ),
                          tabPanel(
                            title = "saved data [to be deleted]",
                            DT::dataTableOutput('analysis_data_select_table')
                          )
                        )
                      )
              )
           )
         )
)
