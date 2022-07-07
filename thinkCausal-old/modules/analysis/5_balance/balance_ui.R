ui_balance <- function(store, id) {
  ns <- NS(id)
  tabPanel(title = "Check Balance",
           value = id,
           # Balance EDA UI ----------------------------------------------------------
           sidebarLayout(
             sidebarPanel(
               width = 4,
               h4("Visualize balance between treatment and control"),
               selectInput(
                 inputId = ns("analysis_balance_select_var"),
                 label = "Select variables for balance check:",
                 multiple = TRUE,
                 choices = NULL,
                 selected = NULL
               ),
               br(),
               create_link_to_help('Balance', button_label = 'What is this plot telling me?'),
               br(),
               br(),
               downloadButton(ns('download_balance_plot'), label = "Download plot"),
               br(),
               br(),
               div(
                 class = 'backNextContainer',
                 actionButton(
                   inputId = ns("analysis_balance_button_back"),
                   label = "Back"
                 ),
                 actionButton(
                   inputId = ns("analysis_balance_button_next"),
                   class = "nav-btn-focus",
                   label = "Next"
                 )
               )
               # br(),
               # create_progress_bar(4/7*100),
               
             ),
             mainPanel(width = 8,
                       br(),
                       plotOutput(
                         outputId = ns("analysis_balance_plot"),
                         height = 500
                       ))
           ))
  
}
