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
      title = 'Exploratory Moderation Tests', 
      sidebarLayout(
        sidebarPanel(
          # selectInput(inputId = "analysis_model_moderator_vars",
          #             label = "Select Moderator:",
          #             choices = NULL)
          uiOutput('explor_moderators')
        ),
        mainPanel()
      )
    )
  )
)