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
                         "Regression Tree on ICATEs" = 'tree',
                         "Ordered ICATEs" = 'ordered',
                         "Histogram of ICATEs" = 'histogram'
                       )),
          conditionalPanel(condition = "input.icate_type == 'tree'",
                           sliderInput(inputId = 'plotBart_tree_depth',
                                       label = "Tree depth:",
                                       value = 2,
                                       min = 1, max = 3, step = 1)),
          conditionalPanel(condition = "input.icate_type == 'ordered'",
                           # selectInput(inputId = 'plotBart_waterfall_desc',
                           #             label = "Show in descending order:",
                           #             choices = c('Yes', 'No')),
                           selectInput(inputId = 'plotBart_waterfall_order',
                                       label = "Order by:",
                                       choices = NULL),
                           selectInput(inputId = 'plotBart_waterfall_color',
                                       label = "Color by:",
                                       choices = NULL)),

          conditionalPanel(condition = "input.icate_type == 'histogram'",
                           selectInput(inputId = 'plotBart_ICATE_color',
                                       label = 'Group by:',
                                       choices = NULL),
                           sliderInput(
                             inputId = "plotBart_ICATE_n_bins",
                             label = "Number of bins: ",
                             min = 5,
                             max = 50,
                             value = 30,
                             step = 1
                           ),
                           sliderInput(
                             inputId = "plotBart_ICATE_alpha",
                             label = "Transparency:",
                             min = 0,
                             max = 1,
                             value = .6,
                             step = .1
                           )
          ),

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
      title = 'Exploratory Subgroup Analyses',
      sidebarLayout(
        sidebarPanel(
          h4("Subgroup analyses"),
          p("Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
          br(),
          selectInput(inputId = 'plotBart_moderator_vars',
                      label = 'Subgroup Anlaysis by:',
                      multiple = F,
                      choices = NULL,
                      selected = NULL),
          br(),

          uiOutput(outputId = "sub_group_ui"),

          br(),
          actionButton(inputId = 'anaysis_moderator_fit',
                       label = 'Analyze Subgroup'),
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

