moderator_page <- tabPanel(
  title = "Subgroup results",
  tabsetPanel(
    id = "analysis_moderator_tabs",
    tabPanel(
      title = 'ICATE',
      sidebarLayout(
        sidebarPanel(
          h4("Individual conditional average treatment effects"),
          p("It is recomended to use all 3 plot types to inform exploratory subgroup analyses in the following section."),
          br(),
          conditionalPanel(condition = "input.icate_type == 'tree'",
                             p('Variables included in the regression tree are a good starting point for exploratory subgroup analyses.')),
          conditionalPanel(condition = "input.icate_type == 'ordered'",
                           p('Plot the orderd ICATEs to help identify if there is high or low treatment effect variability across covariates. You may color the plot by categorical covariates or re-order the plot by continuous covariates.')),
          conditionalPanel(condition = "input.icate_type == 'histogram'",
                           p('Plot the distribution of ICATEs a wider distribution suggests greater variation in the treatment effect across variables. Color by categorical variables to see differences in distributions.')),
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
                      onclick = "openHelpSection('Concept3')",
                      'What is this plot telling me?'),
          br(),br(),
          downloadButton('download_ICATE_plot', label = "Download plot"),
          br(), br(),
          div(class = 'backNextContainer',
              actionButton(inputId = 'analysis_moderator_icate_button_back',
                           label = 'Back'),
              actionButton(inputId = 'analysis_moderator_icate_button_next',
                           label = 'Next'))
        ),
        mainPanel(
          br(),
          plotOutput(outputId = "analysis_moderators_icate_plot",
                     height = 500)
        )
      )),
    tabPanel(
      title = 'Exploratory Subgroup Analyses',
      sidebarLayout(
        sidebarPanel(
          h4("Subgroup analyses"),
          p("Text to come soon..."),
          br(),
          selectInput(inputId = 'plotBart_moderator_vars',
                      label = 'Subgroup analysis by:',
                      multiple = FALSE,
                      choices = NULL,
                      selected = NULL),
          br(),
          uiOutput(outputId = "sub_group_ui"),
         # uiOutput(outputId = "sub_group_pannel"), 
          br(),
          actionButton(inputId = 'analysis_moderator_fit',
                       label = 'Analyze subgroup'),
          br(), br(),
          tags$button(type = 'button',
                      class = 'btn btn-default help',
                      onclick = "openHelpSection('Concept3')",
                      'What is this plot telling me?'),

          br(),br(),
          downloadButton('download_ESA_plot', label = "Download plot"),
          br(), br(),
          div(class = 'backNextContainer',
              actionButton(inputId = 'analysis_moderator_analyses_button_back',
                           label = 'Back'),
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

