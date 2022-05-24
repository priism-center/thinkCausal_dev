ui_data <- function(store, id) {
  ns <- NS(id)
  tabPanel(title = 'Upload Data',
           value = id, 
           #fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               h4("Upload your data"),
               HTML(
                 "<p>Data should be rectangular and in wide format where each column represents one variable.</p>
                        <p> Files can be uploaded from .csv, .txt, Excel (.xlsx), SPSS (.sav) or STATA (.dta) formats.</p>"
               ),
               div(
                 id = "upload_file_div",
                 fileInput(
                   inputId = ns("analysis_upload_data_upload"),
                   label = "Choose file:",
                   buttonLabel = 'Browse',
                   multiple = FALSE,
                   accept = c('.csv', '.txt', '.xlsx', '.sav', '.dta'),
                   placeholder = 'csv, txt, xlsx, sav, or dta'
                 ),
               ),
               conditionalPanel(
                 condition = "output.show_delim == true",
                 ns = ns,
                 radioButtons(
                   inputId = ns('analysis_upload_data_delim_value'),
                   label = "Column delimiter:",
                   selected = ' ',
                   choices = list('[space]' = ' ', '[tab]' = '\t', ',', '|', '-', ':'),
                   inline = FALSE
                 )
               ),
               checkboxInput(
                 inputId = ns("analysis_upload_data_header"),
                 label = "Data contains a header row",
                 value = TRUE
               ),
               br(),
               create_link_to_help('Data'),
               br(),
               br(),
               div(
                 class = 'backNextContainer',
                 actionButton(
                   inputId = ns("analysis_upload_data_button_back"),
                   label = "Back"
                 ),
                 actionButton(
                   inputId = ns('analysis_upload_data_button_columnAssignSave'),
                   class = 'nav-btn-focus',
                   label = 'Save role assignments'
                 )
               )
               # br(),
             ),
             mainPanel(br(),
                       uiOutput(
                         outputId = ns('analysis_upload_data_UI_dragdrop')
                       ))
           ))
  
}
