ui_overlap <- function(store, id) {
  ns <- NS(id)
  tabPanel(title = "Check Overlap",
           value = id,
           sidebarLayout(
             sidebarPanel(
               width = 4,
               h4("Check Overlap"),
               selectInput(
                 inputId = ns("analysis_overlap_type"),
                 label = "View:",
                 choices = c("By Propensity Score" = 1, 
                             "By Variables" = 2
                 ),
                 selected = 1
               ),
               selectInput(
                 inputId = ns("analysis_overlap_method"),
                 label = "Plot type:",
                 choices = c('Histogram', 'Density'),
                 selected = 'Histogram'
               ),
               conditionalPanel(condition = "input.analysis_overlap_type == '2'", 
                                ns = ns, 
                                selectInput(
                                  inputId = ns("analysis_overlap_select_var"),
                                  label = "Select a variable for overlap check:",
                                  multiple = FALSE,
                                  choices = NULL,
                                  selected = NULL
                                )), 
               br(),
               create_link_to_help('EDA', button_label = 'What is this plot telling me?'),
               br(),
               br(),
               downloadButton(ns('download_overlap_plot'), label = "Download plot"),
               br(),
               br(),
               div(
                 class = 'backNextContainer',
                 actionButton(
                   inputId = ns("analysis_plots_support_button_back"),
                   label = "Back"
                 ),
                 actionButton(
                   inputId = ns("analysis_plots_support_button_next"),
                   class = "nav-btn-focus",
                   label = "Next"
                 )
               )
               # br(),
               # create_progress_bar(3/7*100)
             ),
             mainPanel(width = 8,
                       br(),
                       plotOutput(
                         outputId = ns("analysis_overlap_plot"),
                         height = 800
                       ))
           ))
  
}
