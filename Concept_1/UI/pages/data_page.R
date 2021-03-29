data_page <- tabPanel(
  title = 'Data', 
         tabsetPanel(
           id = "analysis_data_tabs",
           tabPanel(title = "Load Data",
                    fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        h4("Upload your data"),
                        HTML("<p>Data should be rectangular and in <a href='https://en.wikipedia.org/wiki/Wide_and_narrow_data' target='_blank' rel='noopener noreferrer'>wide format</a> where each column represents one variable.</p>"),
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
                        # selectInput(inputId = "analysis_data_select_select_ycol",
                        #             label = "Select Response (Y) Column",
                        #             choices = NULL),
                        # selectInput(inputId = "analysis_data_select_select_zcol",
                        #             label = "Select Treatment (Z) Column",
                        #             choices = NULL),
                        # selectInput(inputId = "analysis_data_select_select_xcol",
                        #             label = "Select Covariates (X) Columns",
                        #             choices = NULL,
                        #             multiple = TRUE),
                        actionButton(inputId = 'analysis_data_button_columnAssignSave',
                                     label = 'Save role assignments'),
                        br(),br(),
                        tags$button(type = 'button',
                                    class = 'btn btn-default help',
                                    onclick = "openConceptsPage('Concept3')",
                                    'Help me'),
                        br(), br(),
                        actionButton(inputId = "analysis_data_load_button_next",
                                     label = "Next"),
                        br(), br(),
                        HTML('<details><summary>Advanced options</summary>'),
                        p("Have a large number of variables? Upload your data in [this format] to bypass the variable selection and data formating process."),
                        'TODO: add downloaded template and upload process',
                        br(),
                        HTML('</details><br>'),
                      ),
                      mainPanel(
                        br(),
                        # plotOutput(outputId = 'analysis_data_plot_DAG',
                        #            height = '600px')
                        uiOutput(outputId = 'analysis_data_UI_dragdrop')
                      )
                    )
           ),
           tabPanel(title = "Select Data",
                    fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        h4("Rename and verify your data"),
                        p("Ensure your data is properly named and the data types are correct."),
                        br(),
                        tags$button(type = 'button',
                                    class = 'btn btn-default help',
                                    onclick = "openConceptsPage('Concept3')",
                                    'Help me'),
                        br(),br(),
                        div(
                          class = 'backNextContainer',
                          actionButton(inputId = 'analysis_data_button_reset',
                                       label = 'Reset variable changes'),
                          actionButton(inputId = 'analysis_data_save',
                                       label = 'Save changes')
                        ),
                        br(),
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
                        wellPanel(
                          style = "overflow-y:scroll; max-height: 400px; background-color: transparent; overflow-y: scroll",
                          uiOutput(outputId = 'analysis_data_modify_UI')
                        ),
                        hr(style = "height: 1px; background-color: #bfbfbf"),
                        h4("Your data", style = "padding: 0; margin: 0"),
                        wellPanel(
                          style = "overflow-y:scroll; max-height: 400px; background-color: transparent; overflow-y: scroll",
                          DT::dataTableOutput('analysis_data_table')
                        )
                      )
              )
           )
         )
)
