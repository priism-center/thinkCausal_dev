moderator_page <- tabPanel(
  title = "View Subgroup Results",
  tabsetPanel(
    id = "analysis_moderator_tabs",
    tabPanel(
      title = 'Check',
      sidebarLayout(
        sidebarPanel(
          h4("Check for treatment effect heterogeneity"),
          p("It is recomended to use all 3 plot types to inform exploratory subgroup analyses in the following section."),
          br(),
          conditionalPanel(condition = "input.icate_type == 'tree'",
                           p('Variables included in the regression tree are a good starting point for exploratory subgroup analyses.')),
          conditionalPanel(condition = "input.icate_type == 'ordered'",
                           p('Plot the orderd ICATEs to help identify if there is high or low treatment effect variability across covariates. You may color the plot by categorical covariates or re-order the plot by continuous covariates.')),
          conditionalPanel(condition = "input.icate_type == 'histogram'",
                           p('Plot the distribution of ICATEs a wider distribution suggests greater variation in the treatment effect across variables. Color by categorical variables to see differences in distributions.')),
          selectInput(inputId = "check_type",
                      label = 'Choose a plot type:',
                      choices = list(
                        "Ordered ICATEs" = 'ordered',
                        "Histogram of ICATEs" = 'histogram'
                      )),
          conditionalPanel(condition = "input.check_type == 'histogram'",
                           sliderInput(
                             inputId = "plotBart_ICATE_n_bins",
                             label = "Number of bins: ",
                             min = 5,
                             max = 100,
                             value = 30,
                             step = 5
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
              actionButton(inputId = 'analysis_moderator_check_button_back',
                           label = 'Back'),
              actionButton(inputId = 'analysis_moderator_check_button_next',
                           class = "nav-btn-focus",
                           label = 'Next'))
        ),
        mainPanel(
          br(),
          plotOutput(outputId = "analysis_moderators_check_plot",
                     height = 500)
        )
      )), 
    # start explore tab 
    tabPanel(
      title = 'Explore', 
      sidebarLayout(
        sidebarPanel(
          h4("Explore predictors of possible treatment effect heterogeneity"),
          sliderInput(inputId = 'plotBart_tree_depth',
                      label = "Choose a Tree depth:",
                      value = 2,
                      min = 1, max = 3, step = 1), 
          br(), br(),
          tags$button(type = 'button',
                      class = 'btn btn-default help',
                      onclick = "openHelpSection('Concept3')",
                      'What is this plot telling me?'),
          br(),br(),
          downloadButton('download_moderator_explore_plot', label = "Download plot"),
          br(), br(),
          div(class = 'backNextContainer',
              actionButton(inputId = 'analysis_moderator_explore_button_back',
                           label = 'Back'),
              actionButton(inputId = 'analysis_moderator_explore_button_next',
                           class = "nav-btn-focus",
                           label = 'Next'))
          ), 
        mainPanel(
          br(), 
          outputId = "analysis_moderators_explore_plot",
          height = 500)
      )), 
    # start test tab 
    tabPanel(
      title = 'Test', 
      sidebarLayout(
        sidebarPanel(
          h4("Subgroup analyses"),
          p("Text to come soon..."),
          br(),
          selectInput(inputId = 'plotBart_moderator_vars',
                      label = 'Choose a varaible for subgroup analysis:',
                      multiple = FALSE,
                      choices = NULL,
                      selected = NULL),
          br(),
          uiOutput(outputId = "sub_group_ui"),
          # uiOutput(outputId = "sub_group_pannel"),
          br(),
          actionButton(inputId = 'analysis_moderator_fit',
                       class = "nav-btn-focus",
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
              actionButton(inputId = 'analysis_moderator_test_button_back',
                           label = 'Back'),
              actionButton(inputId = 'analysis_moderator_analyses_button_results',
                           label = 'Back to model results')
          ),
          br(),
          actionButton(inputId = 'analysis_moderator_analyses_button_reproduce',
                       label = 'Log analyses')
        ), 
        mainPanel(
          br(),
          plotOutput(outputId = "analysis_moderators_test_plot",
                     height = 500)
        )
      )
    )
  )
)



# 
# moderator_page <- tabPanel(
#   title = "View Subgroup Results",
#   tabsetPanel(
#     id = "analysis_moderator_tabs",
#     tabPanel(
#       title = 'Check',
#       sidebarLayout(
#         sidebarPanel(
#           h4("Individual conditional average treatment effects"),
#           p("It is recomended to use all 3 plot types to inform exploratory subgroup analyses in the following section."),
#           br(),
#           conditionalPanel(condition = "input.icate_type == 'tree'",
#                              p('Variables included in the regression tree are a good starting point for exploratory subgroup analyses.')),
#           conditionalPanel(condition = "input.icate_type == 'ordered'",
#                            p('Plot the orderd ICATEs to help identify if there is high or low treatment effect variability across covariates. You may color the plot by categorical covariates or re-order the plot by continuous covariates.')),
#           conditionalPanel(condition = "input.icate_type == 'histogram'",
#                            p('Plot the distribution of ICATEs a wider distribution suggests greater variation in the treatment effect across variables. Color by categorical variables to see differences in distributions.')),
#           selectInput(inputId = "check_type",
#                        label = 'Choose a plot type:',
#                        choices = list(
#                          "Ordered ICATEs" = 'ordered',
#                          "Histogram of ICATEs" = 'histogram'
#                        )),
#           conditionalPanel(condition = "input.check_type == 'histogram'",
#                            sliderInput(
#                              inputId = "plotBart_ICATE_n_bins",
#                              label = "Number of bins: ",
#                              min = 5,
#                              max = 100,
#                              value = 30,
#                              step = 5
#                            )
#           ),
# 
#           br(), br(),
#           tags$button(type = 'button',
#                       class = 'btn btn-default help',
#                       onclick = "openHelpSection('Concept3')",
#                       'What is this plot telling me?'),
#           br(),br(),
#           downloadButton('download_ICATE_plot', label = "Download plot"),
#           br(), br(),
#           div(class = 'backNextContainer',
#               actionButton(inputId = 'analysis_moderator_icate_button_back',
#                            label = 'Back'),
#               actionButton(inputId = 'analysis_moderator_icate_button_next',
#                            class = "nav-btn-focus",
#                            label = 'Next'))
#         ),
#         mainPanel(
#           br(),
#           plotOutput(outputId = "analysis_moderators_check_plot",
#                      height = 500)
#         )
#       )),
#     tabPanel(
#       title = 'Explore',
#       sidebarLayout(
#         sidebarPanel(
#           sliderInput(inputId = 'plotBart_tree_depth',
#                      label = "Choose a Tree depth:",
#                      value = 2,
#                      min = 1, max = 3, step = 1)),
#         mainPanel(
#           br(),
#           outputId = "analysis_moderators_explore_plot",
#           height = 500)
#       ))
#     ),
#     tabPanel(
#       title = 'Test',
#       sidebarLayout(
#         sidebarPanel(
#           h4("Subgroup analyses"),
#           p("Text to come soon..."),
#           br(),
#           selectInput(inputId = 'plotBart_moderator_vars',
#                       label = 'Choose a varaible for subgroup analysis:',
#                       multiple = FALSE,
#                       choices = NULL,
#                       selected = NULL),
#           br(),
#           uiOutput(outputId = "sub_group_ui"),
#          # uiOutput(outputId = "sub_group_pannel"),
#           br(),
#           actionButton(inputId = 'analysis_moderator_fit',
#                        class = "nav-btn-focus",
#                        label = 'Analyze subgroup'),
#           br(), br(),
#           tags$button(type = 'button',
#                       class = 'btn btn-default help',
#                       onclick = "openHelpSection('Concept3')",
#                       'What is this plot telling me?'),
# 
#           br(),br(),
#           downloadButton('download_ESA_plot', label = "Download plot"),
#           br(), br(),
#           div(class = 'backNextContainer',
#               actionButton(inputId = 'analysis_moderator_analyses_button_back',
#                            label = 'Back'),
#               actionButton(inputId = 'analysis_moderator_analyses_button_results',
#                            label = 'Back to model results')
#           ),
#           br(),
#           actionButton(inputId = 'analysis_moderator_analyses_button_reproduce',
#                        label = 'Log analyses'),
#         ),
#         mainPanel(
#           br(),
#           plotOutput(outputId = "analysis_moderators_test_plot",
#                      height = 500)
#         )))
#   )
# 
# 
