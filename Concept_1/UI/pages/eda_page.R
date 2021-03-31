eda_page <- tabPanel(
  title = "Exploratory Plots", #htmlOutput("exploration_tab_name"),
  tabsetPanel(
    id = "analysis_plot_tabs", 
    tabPanel(
      title = "Descriptive Plots",
      absolutePanel(id = "analysis_plots_descriptive_loading_message",
                    br(),
                    HTML("Data must be first uploaded and columns selected."),
                    style = "z-index: -2;"),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h4("Explore the data using scatter plots, histograms, density plots, and boxplots"),
          br(),
          selectInput(
            inputId = "analysis_eda_select_plot_type",
            label = "Plot type:",
            multiple = FALSE,
            choices = c("Scatter", "Histogram", "Density", "Boxplot"), #"Pairs"
            selected = "Scatter"
          ),
          conditionalPanel(
            condition = "input.analysis_eda_select_plot_type == 'Pairs'",
            selectInput(
              inputId = "analysis_eda_variable_pairs_vars",
              label = "Columns to plot",
              multiple = TRUE,
              choices = col_names,
              selected = col_names
            )
          ),
          conditionalPanel(
            condition = "input.analysis_eda_select_plot_type != 'Pairs'",
            selectInput(
              inputId = "analysis_eda_variable_x",
              label = "X: ",
              multiple = FALSE,
              choices = col_names,
              selected = col_names[1]
            ),
            conditionalPanel(
              condition = "input.analysis_eda_select_plot_type == 'Scatter'",
              selectInput(
                inputId = "analysis_eda_variable_y",
                label = "Y: ",
                multiple = FALSE,
                choices = col_names,
                selected = col_names[2]
              ),
              selectInput(
                inputId = "analysis_eda_variable_fill",
                label = "Fill color: ",
                multiple = FALSE,
                choices = col_names,
                selected = col_names[12]
              ),
              conditionalPanel(
                condition = "input.analysis_eda_variable_fill == 'Cluster'",
                selectInput(
                  inputId = "analysis_eda_variable_cluster",
                  label = "Clustering algorithm: ",
                  multiple = FALSE,
                  choices = c('k-means', 'Hierarchical'),
                  selected = 'k-means'
                ),
                sliderInput(
                  inputId = "analysis_eda_variable_n_clusters",
                  label = "Number of clusters: ",
                  min = 2,
                  max = 10,
                  value = 4,
                  step = 1
                ),
                HTML(
                  'Clustering using only selected X and Y variables. Not recommended when faceting.<br><br>'
                )
              ),
              selectInput(
                inputId = "analysis_eda_variable_size",
                label = "Size: ",
                multiple = FALSE,
                choices = col_names,
                selected = col_names[4]
              ),
              selectInput(
                inputId = "analysis_eda_variable_regression",
                label = "Linear regression: ",
                multiple = FALSE,
                choices = c('None', 'Include'),
                selected = 'None'
              ),
              bsPopover(id = 'analysis_eda_variable_regression',
                        title = "Linear regression",
                        content = 'Apply a linear regression (y ~ x) to each subgroup of your plot. If you facet on a variable, then the regressions will be calculated per each facet group.',
                        placement = 'top'),
            ), 
            conditionalPanel(
              condition = "input.analysis_eda_select_plot_type == 'Histogram'",
              sliderInput(
                inputId = "analysis_eda_variable_n_bins",
                label = "Number of bins: ",
                min = 5,
                max = 50,
                value = 20,
                step = 1
              )
            ),
            conditionalPanel(
              condition = "input.analysis_eda_select_plot_type == 'Boxplot'",
              selectInput(
                inputId = "analysis_eda_variable_group",
                label = "Grouping: ",
                multiple = FALSE,
                choices = categorical_names
              )
            ),
            selectInput(
              inputId = "analysis_eda_variable_facet",
              label = "Facet variable: ",
              multiple = FALSE,
              choices = c("None", categorical_names),
              selected = "None"
            ),
            bsPopover(id = 'analysis_eda_variable_facet',
                      title = "Facet variable",
                      content = 'Faceting splits the data by one or more variables and then plots these subsets.',
                      placement = 'top'),
            conditionalPanel(
              condition = "input.analysis_eda_variable_facet != 'None'",
              selectInput(
                inputId = "analysis_eda_variable_facet_second",
                label = "Second facet variable: ",
                multiple = FALSE,
                choices = c("None"),
                selected = "None"
              )
            ),
            conditionalPanel(
              condition = "input.analysis_eda_select_plot_type == 'Scatter'",
              sliderInput(
                inputId = "analysis_eda_variable_alpha",
                label = "Opacity: ",
                min = 0.1,
                max = 1,
                value = 0.5,
                step = 0.1
              )
            )
          ),
          HTML('<details><summary>Advanced options</summary>'),
          checkboxInput(inputId = "analysis_eda_check_jitter",
                        label = "Jitter the points?",
                        value = FALSE),
          actionButton(inputId = "analysis_eda_button_download",
                       label = "Download the plot"),
          br(),
          HTML('</details><br>'),
          br(),br(),
          div(
            class = 'backNextContainer',
            actionButton(inputId = "analysis_plots_descriptive_button_back",
                         label = "Back"),
            actionButton(inputId = "analysis_plots_descriptive_button_next",
                         label = "Next")
          )
        ),
        
        mainPanel(
          width = 6,
          br(),
          absolutePanel(id = "analysis_eda_loading_message",
                        HTML("Please wait. Building plot..."),
                        style = "z-index: -2;"),
          plotOutput(outputId = 'analysis_eda_plot',
                     height = 600,
                     brush = brushOpts(id = "analysis_eda_plot_brush")),
          br(),
          htmlOutput(outputId = "analysis_eda_brush_text"),
          DT::dataTableOutput(outputId = "analysis_eda_brush_info")
        )
      )
    ), 
   
# Common Support EDA UI -------------------------------------------------------
      tabPanel(
      title = "Common Support Plots",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h4("Check Common Support"),
          selectInput(
            inputId = "analysis_plot_overlap_select_var",
            label = "Select Variables for Overlap Check:",
            multiple = TRUE,
            choices = X_names,
            selected = X_names
          ),
          awesomeRadio(inputId = "dim.red",
                       label = "View:", inline = T, 
                       choices = c("By Variables" = 1, 
                                   "One Number Summary" = 2), 
                       selected = 1),
          br(),
          awesomeRadio(inputId = "overlap.type",
                       label = "Plot Type:", inline = T, 
                       choices = c('Histogram', 'Density'), 
                       selected = 'Histogram'),
          br(),
          tags$button(type = 'button',
                      class = 'btn btn-default help',
                      onclick = "openConceptsPage('Concept3')",
                      'What is this plot telling me?'),
          br(),br(),
          div(
            class = 'backNextContainer',
            actionButton(inputId = "analysis_plots_support_button_back",
                         label = "Back"),
            actionButton(inputId = "analysis_plots_support_button_next",
                         label = "Next")
          ),
          br(),
          create_progress_bar(3/7*100)
        ),
        mainPanel(
          width = 8,
          br(),
          plotOutput(outputId = "analysis_plot_overlap_plot", height = 800)
          
        )
      )
    ), 

# Balance EDA UI ----------------------------------------------------------
tabPanel(title = "Balance Plots",
         sidebarLayout(
           sidebarPanel(
             width = 4,
             h4("Visualize Balance Between Treatment and Control"),
             selectInput(
               inputId = "analysis_plot_balance_select_var",
               label = "Select Variables for Balance Check:",
               multiple = TRUE,
               choices = X_names,
               selected = X_names
             ),
             br(),
             tags$button(type = 'button',
                         class = 'btn btn-default help',
                         onclick = "openConceptsPage('Concept3')",
                         'What is this plot telling me?'),
             br(),br(),
             div(
               class = 'backNextContainer',
               actionButton(inputId = "analysis_plots_balance_button_back",
                            label = "Back"),
               actionButton(inputId = "analysis_plots_balance_button_next",
                            label = "Next")
             ),
             br(),
             create_progress_bar(4/7*100)
             # add advanced option to remove scale
           ),
           mainPanel(
             width = 8,
             br(),
             plotOutput(outputId = "analysis_plot_balance_plot", 
                        height = 500)
           )
         ))
    )
  )


