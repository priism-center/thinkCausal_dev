
ui_eda <- function(store, id){
  ns <- NS(id)
  tabPanel(
    title = "Exploratory plots",
    value = module_ids$analysis$eda,
    tabsetPanel(
      id = "analysis_plot_tabs", 
      tabPanel(
        title = "Descriptive Plots",
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h4("Explore your data visually"),
            br(),
            selectInput(
              inputId = ns("analysis_eda_select_plot_type"),
              label = "Plot type:",
              multiple = FALSE,
              choices = c("Scatter", "Histogram", "Barplot", "Density", "Boxplot"), #"Pairs"
              selected = "Scatter"
            ),
            conditionalPanel(
              condition = "input.analysis_eda_select_plot_type == 'Pairs'", ns = ns, 
              selectInput(
                inputId = ns("analysis_eda_variable_pairs_vars"),
                label = "Columns to plot",
                multiple = TRUE,
                choices = NULL,
                selected = NULL
              )
            ),
            conditionalPanel(
              condition = "input.analysis_eda_select_plot_type != 'Pairs'", ns = ns, 
              selectInput(
                inputId = ns("analysis_eda_variable_x"),
                label = "X: ",
                multiple = FALSE,
                choices = NULL,
                selected = NULL
              ),
              conditionalPanel(
                condition = "input.analysis_eda_select_plot_type == 'Scatter'", ns = ns, 
                selectInput(
                  inputId = ns("analysis_eda_variable_y"),
                  label = "Y: ",
                  multiple = FALSE,
                  choices = NULL,
                  selected = NULL
                ),
                selectInput(
                  inputId = ns("analysis_eda_variable_fill"),
                  label = "Fill color: ",
                  multiple = FALSE,
                  choices = NULL,
                  selected = NULL
                ),
                selectInput(
                  inputId = ns("analysis_eda_variable_shape"),
                  label = "Shape: ",
                  multiple = FALSE,
                  choices = NULL,
                  selected = NULL
                ),
                conditionalPanel(
                  condition = "input.analysis_eda_variable_fill == 'Cluster'", ns = ns, 
                  selectInput(
                    inputId = ns("analysis_eda_variable_cluster"),
                    label = "Clustering algorithm: ",
                    multiple = FALSE,
                    choices = c('k-means', 'Hierarchical'),
                    selected = 'k-means'
                  ),
                  sliderInput(
                    inputId = ns("analysis_eda_variable_n_clusters"),
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
                  inputId = ns("analysis_eda_variable_size"),
                  label = "Size: ",
                  multiple = FALSE,
                  choices = NULL,
                  selected = NULL
                ),
                selectInput(
                  inputId = ns("analysis_eda_variable_regression"),
                  label = "Linear regression: ",
                  multiple = FALSE,
                  choices = c('None', 'Include'),
                  selected = 'None'
                ),
              ), 
              conditionalPanel(
                condition = "input.analysis_eda_select_plot_type == 'Histogram'", ns = ns, 
                sliderInput(
                  inputId = ns("analysis_eda_variable_n_bins"),
                  label = "Number of bins: ",
                  min = 5,
                  max = 50,
                  value = 20,
                  step = 1
                )
              ),
              conditionalPanel(
                condition = "input.analysis_eda_select_plot_type == 'Boxplot'", ns = ns, 
                selectInput(
                  inputId = ns("analysis_eda_variable_group"),
                  label = "Grouping: ",
                  multiple = FALSE,
                  choices = NULL
                )
              ),
              selectInput(
                inputId = ns("analysis_eda_variable_facet"),
                label = "Panel variable: ",
                multiple = FALSE,
                choices = c("None", NULL),
                selected = "None"
              ),
              conditionalPanel(
                condition = "input.analysis_eda_variable_facet != 'None'", ns = ns, 
                selectInput(
                  inputId = ns("analysis_eda_variable_facet_second"),
                  label = "Second panel variable: ",
                  multiple = FALSE,
                  choices = c("None"),
                  selected = "None"
                )
              ),
              conditionalPanel(
                condition = "input.analysis_eda_select_plot_type == 'Scatter'", ns = ns, 
                sliderInput(
                  inputId = ns("analysis_eda_variable_alpha"),
                  label = "Opacity: ",
                  min = 0.1,
                  max = 1,
                  value = 0.5,
                  step = 0.1
                )
              )
            ),
            HTML('<details><summary>Advanced options</summary>'),
            checkboxInput(inputId = ns("analysis_eda_check_jitter"),
                          label = "Jitter the points?",
                          value = FALSE),
            HTML('</details><br>'),
            br(),br(),
            downloadButton(ns('download_descriptive_plot'), label = "Download plot"),
            br(), br(),
            div(
              class = 'backNextContainer',
              actionButton(inputId = ns("analysis_plots_descriptive_button_back"),
                           label = "Back"),
              actionButton(inputId = ns("analysis_plots_descriptive_button_next"),
                           label = "Next")
            ), 
            br(),
            create_progress_bar(3/8*100)
          ),
          
          mainPanel(
            width = 8,
            br(),
            plotOutput(outputId = ns('analysis_eda_plot'),
                       height = 600,
                       brush = brushOpts(id = ns("analysis_eda_plot_brush"))),
            br(),
            htmlOutput(outputId = ns("analysis_eda_brush_text")),
            DT::dataTableOutput(outputId = ns("analysis_eda_brush_info"))
          )
        )
      ), 
      
      # Common Support EDA UI -------------------------------------------------------
      tabPanel(
        title = "Common Support Plots",
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h4("Check common support"),
            selectInput(
              inputId = ns("analysis_plot_overlap_select_var"),
              label = "Select variables for overlap check:",
              multiple = TRUE,
              choices = NULL,
              selected = NULL
            ),
            radioButtons(
              inputId = ns("analysis_plot_overlap_type"),
              label = "View:", 
              inline = TRUE, 
              choices = c("By Variables" = 1, 
                          "One Number Summary" = 2),
              selected = 1
            ),
            br(),
            radioButtons(
              inputId = ns("analysis_plot_overlap_method"),
              label = "Plot type:", 
              inline = TRUE, 
              choices = c('Histogram', 'Density'), 
              selected = 'Histogram'
            ),
            br(),
            create_link_to_help('EDA', button_label = 'What is this plot telling me?'),
            br(),br(),
            downloadButton(ns('download_overlap_plot'), label = "Download plot"),
            br(), br(),
            div(
              class = 'backNextContainer',
              actionButton(inputId = ns("analysis_plots_support_button_back"),
                           label = "Back"),
              actionButton(inputId = ns("analysis_plots_support_button_next"),
                           label = "Next")
            ),
            br(),
            create_progress_bar(3/7*100)
          ),
          mainPanel(
            width = 8,
            br(),
            plotOutput(outputId = ns("analysis_plot_overlap_plot"), height = 800)
            
          )
        )
      ), 
      
      # Balance EDA UI ----------------------------------------------------------
      tabPanel(title = "Balance Plots",
               sidebarLayout(
                 sidebarPanel(
                   width = 4,
                   h4("Visualize balance between treatment and control"),
                   selectInput(
                     inputId = ns("analysis_plot_balance_select_var"),
                     label = "Select variables for balance check:",
                     multiple = TRUE,
                     choices = NULL,
                     selected = NULL
                   ),
                   br(),
                   create_link_to_help('EDA', button_label = 'What is this plot telling me?'),
                   br(),br(),
                   downloadButton(ns('download_balance_plot'), label = "Download plot"),
                   br(), br(),
                   div(
                     class = 'backNextContainer',
                     actionButton(inputId = ns("analysis_plots_balance_button_back"),
                                  label = "Back"),
                     actionButton(inputId = ns("analysis_plots_balance_button_next"),
                                  label = "Next")
                   ),
                   br(),
                   create_progress_bar(4/7*100),
                   
                 ),
                 mainPanel(
                   width = 8,
                   br(),
                   plotOutput(outputId = ns("analysis_plot_balance_plot"), 
                              height = 500)
                 )
               ))
    )
  )
  
}

