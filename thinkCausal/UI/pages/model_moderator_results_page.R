moderator_page <- tabPanel(
  title = "Subgroup results",
  tabsetPanel(
    id = "analysis_moderator_tabs",
    tabPanel(
      title = 'ICATE',
      sidebarLayout(
        sidebarPanel(
          h4("Individual conditional average treatment effects"),
          p("Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
          br(),
          radioButtons(inputId = "icate_type",
                       label = 'Plot type:',
                       choices = list(
                         "Ordered ICATEs" = 'ordered', 
                         "Histogram of ICATEs" = 'histogram', 
                         "Regression Tree on ICATEs" = "tree")),
          conditionalPanel(condition = "input.icate_type == ordered", 
                           selectInput(inputId = 'plotBart_waterfall_desc', 
                                       label = "Show in descending order:", 
                                       choices = list('Yes' = T, 'No' = F)), 
                           selectInput(inputId = 'plotBart_waterfall_order', 
                                       label = "Order by:", 
                                       choices = NULL), 
                           selectInput(inputId = 'plotBart_waterfall_color', 
                                       label = "Color by:", 
                                       choices = NULL)), 
          br(), br(), 
          tags$button(type = 'button',
                      class = 'btn btn-default help',
                      onclick = "openHelpPage('Concept3')",
                      'What is this plot telling me?'),
          br(),br(),
          div(class = 'backNextContainer',
              actionButton(inputId = 'analysis_moderator_icate_button_back', 
                           label = 'Back'),
              actionButton(inputId = 'analysis_moderator_icate_button_next',
                           label = 'Next'))
          ),
        mainPanel(
          br(),
          plotOutput(outputId = "icate",
                     height = 500)
        )
    )),
    tabPanel(
      title = 'Subgroup Analyses',
      sidebarLayout(
        sidebarPanel(
          h4("Subgroup analyses"),
          p("Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
          br(),
          conditionalPanel(condition = "input.analysis_model_moderator_yes_no == 'Yes'",
                           radioButtons(inputId = 'moderation_type_class',
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
                           selectInput(inputId = "analysis_moderators_explore_only",
                                       label = "Group by:",
                                       multiple = FALSE,
                                       choices = NULL,
                                       selected = NULL)), 
          br(), br(),
          tags$button(type = 'button',
                      class = 'btn btn-default help',
                      onclick = "openHelpPage('Concept3')",
                      'What is this plot telling me?'),
          br(),br(),
          div(class = 'backNextContainer',
              actionButton(inputId = 'analysis_moderator_analyses_button_back', 
                           label = 'Back to tree'),
              actionButton(inputId = 'analysis_moderator_analyses_button_results',
                           label = 'Back to model results')
          ),
          br(),
          actionButton(inputId = 'analysis_moderator_analyses_button_reproduce',
                       label = 'Log analyses'),
        ),
        mainPanel(
          br(),
          plotOutput(outputId = "analysis_moderators_explore_plot",
                     height = 500)
        )))
      )
    )

