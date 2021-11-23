design_page <- tabPanel(
  title = 'Design',
  fluidRow(
  column(4,
         wellPanel(
           class = 'card',
           h4('1. Specify variables'),
           br(),
           textInput('treatment_name', 
                     label = 'What is the name of the treatment or intervention?', 
                     placeholder = 'treatment'),
           textInput('treatment_units', 
                     label = 'What are the units of your outcome variable?',
                     placeholder = 'units'), 
           textInput('treatment_participants', 
                     label = 'Describe the participants in this study',
                     placeholder = 'participants'), 
           h5('Example:'), 
           htmlOutput("analysis_design_text")
           )
  ),
  column(4,
         wellPanel(
           class = 'card',
           h4('2. Specify design'),
           br(),
           selectInput(inputId = 'anaylsis_design', 
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
           actionButton(inputId = "analysis_design_button_next",
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