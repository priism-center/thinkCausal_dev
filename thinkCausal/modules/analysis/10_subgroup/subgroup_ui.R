ui_subgroup <- function(store, id) {
  ns <- NS(id)
  tabPanel(title = "View Subgroup Results",
           value = id,
           sidebarLayout(
             sidebarPanel(
               conditionalPanel(condition = "input.analysis_subgroup_tabs == 'Pre-specified Subgroup Analysis'", ns = ns,
                                h4("Pre-specified Subgroup Analysis"), 
                                p("Examine the treatment effect conditional on the selected pre-specifed subgroup."), 
                                br(),
                                selectInput(inputId = ns('analysis_subgroup_prespecifed'), 
                                            label = 'Subgroup results by:',
                                            choices = NULL, 
                                            selected = NULL
                                )  
                                ), 
               conditionalPanel(condition = "input.analysis_subgroup_tabs == 'Search'", ns = ns,
                                h4("Search covariates for predictors of treatment effect heterogeneity"),
                                p('This regression tree uses covariates as predictors of ICATEs, predictors shown in the tree are helpful in informing exploratory subgroup analyses'), 
                                sliderInput(inputId = ns('plotBart_tree_depth'),
                                            label = "Choose a Tree depth:",
                                            value = 2,
                                            min = 1, max = 3, step = 1)
                                ), 
               conditionalPanel(condition = "input.analysis_subgroup_tabs == 'Exploratory Subgroup Analysis'", ns = ns,
                                h4("Exploratory Subgroup Analyses"), 
                                p("These results are exploratory in nature. Examine the treatment effect conditional on the selected subgroup."), 
                                br(),
                                selectInput(inputId = ns('analysis_subgroup_explore'), 
                                            label = 'Subgroup results by:',
                                            choices = NULL, 
                                            selected = NULL)  
                                ), 
               conditionalPanel(condition = "input.analysis_subgroup_tabs == 'ICATE'", ns = ns, 
                                h4("ICATE plots"), 
                                p('Check for heterogenety in the treatment effect by exploring the variation of ICATEs.'), 
                                selectInput(inputId = ns('icate_plot_type'), 
                                            label = 'Plot type:', 
                                            choices = c('Histogram', 'Waterfall'), 
                                            selected = 'Histogram'), 
                                conditionalPanel(condition = "input.icate_plot_type == 'Histogram'", ns = ns, 
                                                  sliderInput(
                                                    inputId = ns("plotBart_ICATE_n_bins"),
                                                    label = "Number of bins: ",
                                                    min = 5,
                                                    max = 100,
                                                    value = 30,
                                                    step = 5
                                                  ))
                                ),
               br(), br(),
               create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
               br(),br(),
               downloadButton('download_subgroup_plot', label = "Download plot"),
               br(), br(),
               div(class = 'backNextContainer',
               actionButton(inputId = ns('analysis_moderator_analyses_button_results'),
                                label = 'Back to model results')
               ),
               br(),
               actionButton(inputId = ns('analysis_moderator_analyses_button_reproduce'),
                            label = 'Log analyses')
               
             ),
             mainPanel(
               tabsetPanel(id = ns("analysis_subgroup_tabs"),
                  tabPanel(title = 'Pre-specified Subgroup Analysis', 
                           br(), 
                           plotOutput(outputId = ns('analysis_subgroup_prespecified_plot'), 
                                      height = 500)
                           ), 
                  tabPanel(title = 'Search', 
                           br(), 
                           plotOutput(outputId = ns('analysis_subgroup_search_plot'), 
                                      height = 500)
                           ), 
                  tabPanel(title = 'Exploratory Subgroup Analysis', 
                           br(), 
                           plotOutput(outputId = ns('analysis_subgroup_explore_plot'), 
                                      height = 500)
                           ), 
                  tabPanel(title = 'ICATE', 
                           br(), 
                           plotOutput(outputId = ns('analysis_moderators_icate_plot'), 
                                      height = 500))
                         
                  
                  )
               
               )
             )
           )
}

# 
# ui_subgroup <- function(store, id){
#   ns <- NS(id)
#   tabPanel(
#     title = "View Subgroup Results",
#     value = id,
#     tabsetPanel(
#       id = "analysis_moderator_tabs",
#       tabPanel(
#         title = 'Check',
#         sidebarLayout(
#           sidebarPanel(
#             h4("Check for treatment effect heterogeneity"),
#             p("It is recomended to use all 3 plot types to inform exploratory subgroup analyses in the following section."),
#             br(),
#             conditionalPanel(condition = "input.icate_type == 'tree'",
#                              p('Variables included in the regression tree are a good starting point for exploratory subgroup analyses.')),
#             conditionalPanel(condition = "input.icate_type == 'ordered'",
#                              p('Plot the orderd ICATEs to help identify if there is high or low treatment effect variability across covariates. You may color the plot by categorical covariates or re-order the plot by continuous covariates.')),
#             conditionalPanel(condition = "input.icate_type == 'histogram'",
#                              p('Plot the distribution of ICATEs a wider distribution suggests greater variation in the treatment effect across variables. Color by categorical variables to see differences in distributions.')),
#             selectInput(inputId = ns("check_type"),
#                         label = 'Choose a plot type:',
#                         choices = list(
#                           "Ordered ICATEs" = 'ordered',
#                           "Histogram of ICATEs" = 'histogram'
#                         )),
#             conditionalPanel(condition = "input.check_type == 'histogram'",
#                              sliderInput(
#                                inputId = ns("plotBart_ICATE_n_bins"),
#                                label = "Number of bins: ",
#                                min = 5,
#                                max = 100,
#                                value = 30,
#                                step = 5
#                              )
#             ),
# 
#             br(), br(),
#             create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
#             br(),br(),
#             downloadButton('download_ICATE_plot', label = "Download plot"),
#             br(), br(),
#             div(class = 'backNextContainer',
#                 actionButton(inputId = ns('analysis_moderator_check_button_back'),
#                              label = 'Back'),
#                 actionButton(inputId = ns('analysis_moderator_check_button_next'),
#                              class = "nav-btn-focus",
#                              label = 'Next'))
#           ),
#           mainPanel(
#             br(),
#             plotOutput(outputId = ns("analysis_moderators_check_plot"),
#                        height = 500)
#           )
#         )),
#       # start explore tab
#       tabPanel(
#         title = 'Explore',
#         sidebarLayout(
#           sidebarPanel(
#             h4("Explore predictors of possible treatment effect heterogeneity"),
#             br(),
#             sliderInput(inputId = ns('plotBart_tree_depth'),
#                         label = "Choose a Tree depth:",
#                         value = 2,
#                         min = 1, max = 3, step = 1),
#             br(), br(),
#             create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
#             br(),br(),
#             downloadButton(ns('download_moderator_explore_plot'), label = "Download plot"),
#             br(), br(),
#             div(class = 'backNextContainer',
#                 actionButton(inputId = ns('analysis_moderator_explore_button_back'),
#                              label = 'Back'),
#                 actionButton(inputId = ns('analysis_moderator_explore_button_next'),
#                              class = "nav-btn-focus",
#                              label = 'Next'))
#           ),
#           mainPanel(
#             br(),
#             outputId = ns("analysis_moderators_explore_plot"),
#             height = 500)
#         )),
#       # start test tab
#       tabPanel(
#         title = 'Test',
#         sidebarLayout(
#           sidebarPanel(
#             h4("Subgroup analyses"),
#             p("Id eu nisl nunc mi ipsum. Ut aliquam purus sit amet luctus venenatis lectus. Sed augue lacus viverra vitae. Mattis vulputate enim nulla aliquet porttitor. Risus quis varius quam quisque. Arcu odio ut sem nulla. Nunc sed id semper risus in hendrerit gravid"),
#             br(),
#             selectInput(inputId = ns('plotBart_moderator_vars'),
#                         label = 'Choose a varaible for subgroup analysis:',
#                         multiple = FALSE,
#                         choices = NULL,
#                         selected = NULL),
#             br(),
#             uiOutput(outputId = ns("sub_group_ui")),
#             # uiOutput(outputId = "sub_group_pannel"),
#             br(),
#             actionButton(inputId = ns('analysis_moderator_fit'),
#                          class = "nav-btn-focus",
#                          label = 'Analyze subgroup'),
#             br(), br(),
#             create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
#             br(),br(),
#             downloadButton('download_ESA_plot', label = "Download plot"),
#             br(), br(),
#             div(class = 'backNextContainer',
#                 actionButton(inputId = ns('analysis_moderator_test_button_back'),
#                              label = 'Back'),
#                 actionButton(inputId = ns('analysis_moderator_analyses_button_results'),
#                              label = 'Back to model results')
#             ),
#             br(),
#             actionButton(inputId = ns('analysis_moderator_analyses_button_reproduce'),
#                          label = 'Log analyses')
#           ),
#           mainPanel(
#             br(),
#             plotOutput(outputId = ns("analysis_moderators_test_plot"),
#                        height = 500)
#           )
#         )
#       )
#     )
#   )
# 
# }
# 
