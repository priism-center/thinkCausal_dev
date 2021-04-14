# moderator_page <- tabPanel(
#   title = "Treatment Moderators",
# 
#   tabsetPanel(
#   conditionalPanel(
#      condition = "input.analysis_model_moderator_yes_no == 'Yes'",
#   tabPanel(
#     title = 'Pre-Specifed Moderation Tests', 
#     mainPanel()
#  )
# ), 
# tabPanel(
#   title = 'Exploratory Moderation Tests', 
#   mainPanel()
# )
# )
# )
# 
# 

moderator_page <- tabPanel(
  title = "Treatment Moderators",
  tabsetPanel(
    id = "moderator_tabs",
    tabPanel(
      title = 'Pre-Specifed Moderation Tests', 
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "analysis_model_moderator_vars",
                      label = "Select Moderator:",
                      choices = NULL)
          #uiOutput('explor_moderators')
        ),
        mainPanel()
    )), 
    tabPanel(title = 'Estimated Variable Importance',
             sidebarLayout(
               sidebarPanel(
                 h5("Variable Importance Interpretation"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                 br(),
                 awesomeRadio(inputId = 'variable_importance_view', 
                              label = 'Veiw:', 
                              choices = c('Plot (top 10 confounders)', 'Table (all confounders)'))
               ),
               mainPanel(
                 conditionalPanel(condition = "input.variable_importance_view == 'Plot (top 10 confounders)'", 
                 plotOutput(outputId = "variable_importance_plot", 
                            height = 500)), 
                 conditionalPanel(condition = "input.variable_importance_view == 'Table (all confounders)'",
                                    dataTableOutput(outputId ="variable_importance_table", 
                                                    height = 500))
                 
                 
               )
             )),
    tabPanel(
      title = 'Exploratory Moderation Tests', 
      sidebarLayout(
        sidebarPanel(
          # selectInput(inputId = "analysis_model_moderator_vars",
          #             label = "Select Moderator:",
          #             choices = NULL)
          # awesomeRadio(
          #   inputId = 'expore_type', 
          #   label = 'Exploratory Plot View:', 
          #   choices = c('Moderators', 'Variable Importance'),
          #   selected = 'Moderators'),
          #uiOutput('explor_moderators')
          selectInput(
                      inputId = "eda_moderation",
                      label = "Select Moderator:",
                      multiple = FALSE,
                      choices = NULL, 
                      selected = NULL)
        ),
        mainPanel(
          plotOutput(outputId = "cate_plot",
                     height = 500)
        )
      )
    )
  )
)
