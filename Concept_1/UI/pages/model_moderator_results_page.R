moderator_page <- tabPanel(
  title = "Treatment moderators",
  tabsetPanel(
    id = "moderator_tabs",
    tabPanel(
      title = 'Individual Treatment Effects', 
      sidebarLayout(
        sidebarPanel(
          awesomeRadio(inputId = 'icate_type', 
                       label = "Plot Type:", 
                       choices = c('Ordered Effects', 'Histigram'), 
                       selected = 'Ordered Effects')
        ),
        mainPanel(
          br(),
          conditionalPanel(condition = "input.icate_type == 'Ordered Effects'",
                           plotOutput(outputId = "ordered_icate",
                                      height = 500)),
          
          conditionalPanel(condition = "input.icate_type == 'Histigram'",
                           plotOutput(outputId = "histigram_icate",
                                      height = 500))
        )
    )), 
    tabPanel(title = 'Predictors of Individual Treatment Effects',
             sidebarLayout(
               sidebarPanel(
                 h5("Variable importance interpretation"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                 br(),

                 awesomeRadio(inputId = 'set_tree_depth', 
                              label = 'Tree Depth', 
                              choices = list('1' = 1, '2' = 2, '3' = 3), 
                              inline = T, 
                              selected = '2')
               ),
               mainPanel(
                 br(),
                 plotOutput(outputId = "analysis_moderator_single_tree", 
                            height = 500)
                 
               )
             )),
    tabPanel(
      title = 'Moderation Tests', 
      sidebarLayout(
        sidebarPanel(
          awesomeRadio(inputId = 'moderation_type_class', 
                       label = 'Select Moderation Test:', 
                       choices = list('Pre-Specified' = 'pre',
                                      'Exploratory' = 'exp',
                                      'Learn more'), 
                       selected = 'pre'
                       ),
          
          conditionalPanel(condition = "input.moderation_type_class == 'exp'", 
                           selectInput(inputId = "analysis_moderators_explore_select",
                                       label = "Select moderator:",
                                       multiple = FALSE,
                                       choices = NULL, 
                                       selected = NULL))

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
