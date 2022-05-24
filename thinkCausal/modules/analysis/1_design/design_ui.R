
ui_design <- function(store, id){
  ns <- NS(id)
  tabPanel(
    title = 'Describe Design',
    value = id,
    fluidRow(
      column(4,
             wellPanel(
               class = 'card',
               h4(create_info_icon('1. Describe your study', 'This information will be used to help interpret results later on.')),
               br(),
               textInput(ns('treatment_name'),
                         label = 'What is the name of the treatment or intervention?',
                         placeholder = 'treatment condition'),
               textInput(ns('treatment_units'),
                         label = 'What are the units of your outcome variable?',
                         placeholder = 'units'),
               textInput(ns('treatment_participants'),
                         label = 'Describe the participants in this study',
                         placeholder = 'participants'),
               h5('Example:'),
               htmlOutput(ns("analysis_design_text"))
             )
      ),
      column(4,
             wellPanel(
               class = 'card',
               h4('2. Describe the study design'),
               br(),
               selectInput(inputId = ns('analysis_design'),
                           label = 'Indicate the study design',
                           choices = c("",
                                       "Unsure",
                                       'Observational',
                                       'Randomized treatment',
                                       'Block randomized treatment')), 
               selectInput(inputId = ns('analysis_weights'), 
                           label = 'Do you have survey weights?', 
                           choices = c("", "Unsure", "No", "Yes")), 
               selectInput(ns('analysis_random_intercept'),
                           # label = 'Random intercept',
                           label = create_info_icon('Does your study have clustered or nested data?', 'Classes within schools or patients within medical practices are examples of clustered/nested data'),
                           choices = c("", "Unsure", "No", "Yes")),
             )
      ),
      column(4,
             wellPanel(
               class = 'card',
               h4('3. Upload data'),
               br(),
               actionButton(inputId = ns("analysis_design_button_next"),
                            class = "nav-btn-focus",
                            label = "Go to data"),
               br(), br(),
               create_link_to_help('Study design')
             )
      )
    )
  )
}

