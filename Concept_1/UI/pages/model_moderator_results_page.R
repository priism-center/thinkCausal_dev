moderator_page <- tabPanel(
  title = "Treatment moderators",
  tabsetPanel(
    id = "moderator_tabs",
    tabPanel(
      title = 'ICATE', 
      sidebarLayout(
        sidebarPanel(
          div(class = 'backNextContainer',
              actionButton(inputId = 'icate_tree',
                           label = 'ICATE Regression Tree')),
          conditionalPanel(
            condition = "input.analysis_model_moderator_yes_no == 'Yes'",
            br(),
            div(class = 'backNextContainer',
                actionButton(inputId = 'pre_sub',
                             label = 'Prespecified subgroup analysis'))),
          br(),
          div(class = 'backNextContainer',
              actionButton(inputId = 'exp_sub',
                           label = 'Exploratory subgroup analysis'))
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
    tabPanel(title = 'ICATE Regression Tree',
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
      title = 'Subgroup Analyses', 
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(condition = "input.analysis_model_moderator_yes_no == 'Yes'", 
                           awesomeRadio(inputId = 'moderation_type_class', 
                                        label = 'Type of Subgroup Analysis:', 
                                        choices = c('Prespecified',
                                                    'Exploratory'), 
                                        selected = 'Prespecified')
                           
                           ),
          
          conditionalPanel(condition = "input.moderation_type_class == 'Prespecified' & input.analysis_model_moderator_yes_no == 'Yes'", 
                           selectInput(inputId = "analysis_moderator_vars",
                                       label = "Group by:",
                                       multiple = FALSE,
                                       choices = NULL, 
                                       selected = NULL)),
          
          conditionalPanel(condition = "input.moderation_type_class != 'Prespecified' & input.analysis_model_moderator_yes_no == 'Yes'", 
                           selectInput(inputId = "analysis_moderators_explore_select",
                                       label = "Group by:",
                                       multiple = FALSE,
                                       choices = NULL, 
                                       selected = NULL)),
          
          conditionalPanel(condition = "input.analysis_model_moderator_yes_no == 'No'", 
                           selectInput(inputId = "analysis_moderators_explore_select",
                                       label = "Group by:",
                                       multiple = FALSE,
                                       choices = NULL, 
                                       selected = NULL))
          
        ),
        mainPanel(
          br(),
          plotOutput(outputId = "analysis_moderators_explore_plot",
                     height = 500)
        )))
      )
    )
  
