
ui_subgroup <- function(store, id){
  ns <- NS(id)
  tabPanel(
    title = "View Subgroup Results",
    value = id,
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
            selectInput(inputId = ns("check_type"),
                        label = 'Choose a plot type:',
                        choices = list(
                          "Ordered ICATEs" = 'ordered',
                          "Histogram of ICATEs" = 'histogram'
                        )),
            conditionalPanel(condition = "input.check_type == 'histogram'",
                             sliderInput(
                               inputId = ns("plotBart_ICATE_n_bins"),
                               label = "Number of bins: ",
                               min = 5,
                               max = 100,
                               value = 30,
                               step = 5
                             )
            ),
            
            br(), br(),
            create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
            br(),br(),
            downloadButton('download_ICATE_plot', label = "Download plot"),
            br(), br(),
            div(class = 'backNextContainer',
                actionButton(inputId = ns('analysis_moderator_check_button_back'),
                             label = 'Back'),
                actionButton(inputId = ns('analysis_moderator_check_button_next'),
                             class = "nav-btn-focus",
                             label = 'Next'))
          ),
          mainPanel(
            br(),
            plotOutput(outputId = ns("analysis_moderators_check_plot"),
                       height = 500)
          )
        )), 
      # start explore tab 
      tabPanel(
        title = 'Explore', 
        sidebarLayout(
          sidebarPanel(
            h4("Explore predictors of possible treatment effect heterogeneity"),
            br(), 
            sliderInput(inputId = ns('plotBart_tree_depth'),
                        label = "Choose a Tree depth:",
                        value = 2,
                        min = 1, max = 3, step = 1), 
            br(), br(),
            create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
            br(),br(),
            downloadButton(ns('download_moderator_explore_plot'), label = "Download plot"),
            br(), br(),
            div(class = 'backNextContainer',
                actionButton(inputId = ns('analysis_moderator_explore_button_back'),
                             label = 'Back'),
                actionButton(inputId = ns('analysis_moderator_explore_button_next'),
                             class = "nav-btn-focus",
                             label = 'Next'))
          ), 
          mainPanel(
            br(), 
            outputId = ns("analysis_moderators_explore_plot"),
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
            selectInput(inputId = ns('plotBart_moderator_vars'),
                        label = 'Choose a varaible for subgroup analysis:',
                        multiple = FALSE,
                        choices = NULL,
                        selected = NULL),
            br(),
            uiOutput(outputId = ns("sub_group_ui")),
            # uiOutput(outputId = "sub_group_pannel"),
            br(),
            actionButton(inputId = ns('analysis_moderator_fit'),
                         class = "nav-btn-focus",
                         label = 'Analyze subgroup'),
            br(), br(),
            create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
            br(),br(),
            downloadButton('download_ESA_plot', label = "Download plot"),
            br(), br(),
            div(class = 'backNextContainer',
                actionButton(inputId = ns('analysis_moderator_test_button_back'),
                             label = 'Back'),
                actionButton(inputId = ns('analysis_moderator_analyses_button_results'),
                             label = 'Back to model results')
            ),
            br(),
            actionButton(inputId = ns('analysis_moderator_analyses_button_reproduce'),
                         label = 'Log analyses')
          ), 
          mainPanel(
            br(),
            plotOutput(outputId = ns("analysis_moderators_test_plot"),
                       height = 500)
          )
        )
      )
    )
  )
  
}

