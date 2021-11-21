design_page <- tabPanel(
  title = 'Design',
  fluidRow(
  column(6,
         h4('1. Specify variables'),
         wellPanel(
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
           textOutput("analysis_design_text.1"), 
            br(), 
            textOutput("analysis_design_text.2") 
           
           )
  ),
  column(6,
         h4('2. Specify design'), 
         wellPanel(
           selectInput(inputId = 'anaylsis_design', 
                       label = 'Indicate the study design', 
                       choices = c("", 
                                   "Unsure", 
                                   'Observational', 
                                   'Randomized treatment', 
                                   'Block randomized treatment'))
         )
      )
  ), 
  fluidRow(
    column(6, 
           wellPanel(
           actionButton(inputId = "analysis_design_button_next",
                        label = "Next"), 
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