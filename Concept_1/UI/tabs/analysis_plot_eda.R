analysis_plot_eda <- tabPanel("Descriptive Plots", 
         sidebarLayout(
           sidebarPanel(
             width = 4,
             h4("Explore the data using scatter plots, histograms, density plots, and boxplots"),
             br(),
             uiOutput("exploration_selection_data_spawn"),
             selectInput(
               inputId ="exploration_select_plot_type",
               label = "Plot type:",
               multiple = FALSE,
               choices = c("Scatter", "Histogram", "Density", "Boxplot"),
               selected = "Scatter"
             ),
             selectInput(
               inputId ="exploration_variable_x",
               label = "X: ",
               multiple = FALSE,
               choices = col_names,
               selected = col_names[1]
             ),
             conditionalPanel(
               condition = "input.exploration_select_plot_type == 'Scatter'",
               selectInput(
                 inputId = "exploration_variable_y",
                 label = "Y: ",
                 multiple = FALSE,
                 choices = col_names,
                 selected = col_names[9]
               ),
               selectInput(
                 inputId = "exploration_variable_fill",
                 label = "Fill color: ",
                 multiple = FALSE,
                 choices = col_names,
                 selected = col_names[12]
               ),
               conditionalPanel(
                 condition = "input.exploration_variable_fill == 'Cluster'",
                 selectInput(
                   inputId = "exploration_variable_cluster",
                   label = "Clustering algorithm: ",
                   multiple = FALSE,
                   choices = c('k-means', 'Hierarchical'),
                   selected = 'k-means'
                 ),
                 sliderInput(
                   inputId = "exploration_variable_n_clusters",
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
                 inputId = "exploration_variable_size",
                 label = "Size: ",
                 multiple = FALSE,
                 choices = col_names,
                 selected = col_names[4]
               ),
               selectInput(
                 inputId = "exploration_variable_regression",
                 label = "Linear regression: ",
                 multiple = FALSE,
                 choices = c('None', 'Include'),
                 selected = 'None'
               ),
               bsPopover(id = 'exploration_variable_regression',
                         title = "Linear regression",
                         content = 'Apply a linear regression (y ~ x) to each subgroup of your plot. If you facet on a variable, then the regressions will be calculated per each facet group.',
                         placement = 'top'),
             ), 
             conditionalPanel(
               condition = "input.exploration_select_plot_type == 'Histogram'",
               sliderInput(
                 inputId = "exploration_variable_n_bins",
                 label = "Number of bins: ",
                 min = 5,
                 max = 50,
                 value = 20,
                 step = 1
               )
             ),
             conditionalPanel(
               condition = "input.exploration_select_plot_type == 'Boxplot'",
               selectInput(
                 inputId = "exploration_variable_group",
                 label = "Grouping: ",
                 multiple = FALSE,
                 choices = categorical_names
               )
             ),
             selectInput(
               inputId = "exploration_variable_facet",
               label = "Facet variable: ",
               multiple = FALSE,
               choices = c("None", categorical_names),
               selected = "None"
             ),
             bsPopover(id = 'exploration_variable_facet',
                       title = "Facet variable",
                       content = 'Faceting splits the data by one or more variables and then plots these subsets.',
                       placement = 'top'),
             conditionalPanel(
               condition = "input.exploration_variable_facet != 'None'",
               selectInput(
                 inputId = "exploration_variable_facet_second",
                 label = "Second facet variable: ",
                 multiple = FALSE,
                 choices = c("None"),
                 selected = "None"
               )
             ),
             conditionalPanel(
               condition = "input.exploration_select_plot_type == 'Scatter'",
               sliderInput(
                 inputId = "exploration_variable_alpha",
                 label = "Opacity: ",
                 min = 0.1,
                 max = 1,
                 value = 0.5,
                 step = 0.1
               )
             ),
             HTML('<details><summary>Advanced options</summary>'),
             actionButton(inputId = "exploration_button_download", 
                          label = "Download the plot"),
             br(),
             HTML('</details><br>')
           ),
           
           mainPanel(
             width = 6,
             plotOutput('exploration_plot',  height = 600,
                        brush = brushOpts(id = "plot1_brush")),
             br(),
             htmlOutput("brush_text"),
             DT::dataTableOutput("brush_info")
           )
         ))
