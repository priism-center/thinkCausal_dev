data_page <- tabPanel(
  title = 'Data', 
         tabsetPanel(
           id = "analysis_data_tabs",
           tabPanel(title = "Upload",
                    fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        h4("Upload your data"),
                        HTML("<p>Data should be rectangular and in <a href='https://en.wikipedia.org/wiki/Wide_and_narrow_data' target='_blank' rel='noopener noreferrer'>wide format</a> where each column represents one variable.</p>"),
                        div(
                          id = "upload_file_div",
                          fileInput(inputId = "analysis_data_upload", 
                                    label = "Choose file:",
                                    buttonLabel = 'Browse',
                                    multiple = FALSE,
                                    accept = c('.csv', '.txt', '.xlsx', '.dta', '.spss'),
                                    placeholder = 'csv, txt, xlsx, dta, or spss'),
                        ),
                        conditionalPanel(
                          condition = "output.show_delim == true",
                          radioButtons(
                            inputId = 'analysis_data_delim_value',
                            label = "Column delimiter:",
                            selected = ' ',
                            choices = list('[space]' = ' ', '[tab]' = '\t', ',', '|', '-', ':'),
                            inline = FALSE
                          )
                        ),
                        checkboxInput(inputId = "analysis_data_header", 
                                      label = "Data contains a header row", 
                                      value = TRUE),
                        br(),
                        tags$button(type = 'button',
                                    class = 'btn btn-default help',
                                    onclick = "openHelpPage('Dataloading')",
                                    'Help me'),
                        br(), br(),
                        div(
                          class = 'backNextContainer',
                          actionButton(inputId = "analysis_data_button_back",
                                       label = "Back"),
                          actionButton(inputId = 'analysis_data_button_columnAssignSave',
                                       label = 'Save role assignments')
                        ), 
                        br(), 
                      ),
                      mainPanel(
                        br(),
                        uiOutput(outputId = 'analysis_data_UI_dragdrop')
                      )
                    )
           ),
           tabPanel(title = "Group",
                    fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        h4("Are any of these variables part of a natural group?"),
                        p("Variables that have been dummy coded should be grouped together. Please verify any pre-filled groups. Empty groups will be ignored."),
                        br(),
                        actionButton(inputId = 'analysis_data_add_group',
                                     label = 'Add group'),
                        br(), br(),
                        actionButton(inputId = 'analysis_data_remove_group',
                                     label = 'Remove group'),
                        br(), br(),
                        tags$button(type = 'button',
                                    class = 'btn btn-default help',
                                    onclick = "openHelpPage('Concept3')", # not sure where this is linked to
                                    'Help me'),
                        br(), br(),
                        div(
                          class = 'backNextContainer',
                          actionButton(inputId = "analysis_data_pivot_button_back",
                                       label = "Back"),
                          actionButton(inputId = 'analysis_data_save_groupings',
                                       label = 'Save groupings')
                        ),
                        br(),
                        create_progress_bar(1/9*100)
                      ),
                      mainPanel(
                        br(),
                        uiOutput(outputId = 'analysis_data_UI_dragdrop_grouping')
                      )
                    )
           ),
           tabPanel(title = "Verify",
                    fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        h4("Rename and verify your data"),
                        p("Ensure your data is named and the data types are correct."),
                        htmlOutput(outputId = 'analysis_data_text_na'),
                        br(),
                        actionButton(inputId = 'analysis_data_button_reset',
                                     label = 'Reset variable changes'),
                        br(), br(),
                        tags$button(type = 'button',
                                    class = 'btn btn-default help',
                                    onclick = "openHelpPage('Concept3')",
                                    'Help me'),
                        br(), br(),
                        div(
                          class = 'backNextContainer',
                          actionButton(inputId = "analysis_data_select_button_back",
                                       label = "Back"),
                          actionButton(inputId = 'analysis_data_save',
                                       label = 'Save changes')
                        ),
                        br(),
                        create_progress_bar(1/7*100)
                      ),
                      mainPanel(
                        wellPanel(
                          style = "overflow-y:scroll; max-height: 400px; background-color: transparent; padding: 15px 15px 0 15px;",
                          uiOutput(outputId = 'analysis_data_modify_UI'),
                          div(class = 'bottom-shadow')
                        ),
                        hr(style = "height: 1px; background-color: #bfbfbf"),
                        h4("Your data", style = "padding: 0; margin: 0"),
                        wellPanel(
                          style = "background-color: transparent;",
                          DT::dataTableOutput('analysis_data_table')
                        )
                      )
              )
           )
         )
)
