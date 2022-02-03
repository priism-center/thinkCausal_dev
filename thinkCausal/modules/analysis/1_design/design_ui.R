
ui_design <- function(store, id){
  ns <- NS(id)
  tabPanel(
    title = 'Design',
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
                                       'Block randomized treatment'))
             )
      ),
      column(4,
             wellPanel(
               class = 'card',
               h4('3. Upload data'),
               br(),
               actionButton(inputId = ns("analysis_design_button_next"),
                            label = "Go to data"),
               br(), br(),
               tags$button(
                 type = 'button',
                 class = 'btn btn-default help',
                 onclick = "openHelpPage('Concept2')",
                 'Help'
               )
             )
      )
    )
  )
}

