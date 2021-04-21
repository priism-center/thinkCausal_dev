moderator_page <- tabPanel(
  title = "Treatment moderators",
  tabsetPanel(
    id = "moderator_tabs",
    tabPanel(
      title = 'Pre-Specifed Moderation Tests', 
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "analysis_moderator_vars",
                      label = "Select moderator:",
                      choices = NULL)
        ),
        mainPanel(
          br()
        )
    )), 
    tabPanel(title = 'Estimated Variable Importance',
             sidebarLayout(
               sidebarPanel(
                 h5("Variable importance interpretation"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                 br(),
                 awesomeRadio(inputId = 'analysis_moderator_varImportance_radio', 
                              label = 'Show plot or table:', 
                              choices = c('Plot (top 10 confounders)', 'Table (all confounders)'))
               ),
               mainPanel(
                 br(),
                 conditionalPanel(condition = "input.analysis_moderator_varImportance_radio == 'Plot (top 10 confounders)'", 
                 plotOutput(outputId = "analysis_moderator_varImportance_plot", 
                            height = 500)), 
                 conditionalPanel(condition = "input.analysis_moderator_varImportance_radio == 'Table (all confounders)'",
                                  dataTableOutput(outputId ="analysis_moderator_varImportance_table", 
                                                  height = 500))
               )
             )),
    tabPanel(
      title = 'Exploratory Moderation Tests', 
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "analysis_moderators_explore_select",
                      label = "Select moderator:",
                      multiple = FALSE,
                      choices = NULL, 
                      selected = NULL)
        ),
        mainPanel(
          br(),
          plotOutput(outputId = "analysis_moderators_explore_plot",
                     height = 500)
        )
      )
    )
  )
)
