ui_verify <- function(store, id) {
  ns <- NS(id)
  tabPanel(title = 'Verify Data Types',
           value = id,
           sidebarLayout(
             sidebarPanel(
               h4("Rename and verify your data"),
               p("Ensure your data is named and the data types are correct."),
               htmlOutput(outputId = ns('analysis_verify_data_text_na')),
               br(),
               actionButton(
                 inputId = ns('analysis_verify_data_button_reset'),
                 label = 'Reset variable changes'
               ),
               br(),
               br(),
               create_link_to_help('Data'),
               br(),
               br(),
               div(
                 class = 'backNextContainer',
                 actionButton(
                   inputId = ns("analysis_verify_data_select_button_back"),
                   label = "Back"
                 ),
                 actionButton(
                   inputId = ns('analysis_verify_data_save'),
                   class = "nav-btn-focus",
                   label = 'Save changes'
                 )
               )
               # br(),
               # create_progress_bar(1/7*100)
             ),
             mainPanel(
               wellPanel(style = "overflow-y:scroll; max-height: 400px; background-color: transparent; padding: 15px 15px 0 15px;",
                         uiOutput(outputId = ns(
                           'analysis_verify_data_modify_UI'
                         )),
                         div(class = 'bottom-shadow')),
               hr(style = "height: 1px; background-color: #bfbfbf"),
               h4("Your data", style = "padding: 0; margin: 0"),
               wellPanel(style = "background-color: transparent;",
                         DT::dataTableOutput(ns(
                           'analysis_verify_data_table'
                         )))
             )
           ))
  
}
