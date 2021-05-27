# this defines the eda plotting on the analysis tab
# TODO: think about smart defaults (both default columns and default plot type)

edaUI <- function(id, col_names, categorical_names) {
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Explore the data using scatter plots, histograms, density plots, and boxplots"),
        br(),
        selectInput(
          inputId = ns("analysis_eda_select_plot_type"),
          label = "Plot type:",
          multiple = FALSE,
          choices = c("Pairs", "Scatter", "Histogram", "Density", "Boxplot"),
          selected = "Pairs"
        ),
        conditionalPanel(
          condition = "input.analysis_eda_select_plot_type == 'Pairs'",
          ns = ns,
          selectInput(
            inputId = ns("analysis_eda_variable_pairs_vars"),
            label = "Columns to plot",
            multiple = TRUE,
            choices = col_names,
            selected = col_names
          )
        ),
        conditionalPanel(
          condition = "input.analysis_eda_select_plot_type != 'Pairs'",
          ns = ns,
          selectInput(
            inputId = ns("analysis_eda_variable_x"),
            label = "X: ",
            multiple = FALSE,
            choices = col_names,
            selected = col_names[1]
          ),
          conditionalPanel(
            condition = "input.analysis_eda_select_plot_type == 'Scatter'",
            ns = ns,
            selectInput(
              inputId = ns("analysis_eda_variable_y"),
              label = "Y: ",
              multiple = FALSE,
              choices = col_names,
              selected = col_names[2]
            ),
            selectInput(
              inputId = ns("analysis_eda_variable_fill"),
              label = "Fill color: ",
              multiple = FALSE,
              choices = col_names,
              selected = col_names[12]
            ),
            conditionalPanel(
              condition = "input.analysis_eda_variable_fill == 'Cluster'",
              ns = ns,
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
              choices = col_names,
              selected = col_names[4]
            ),
            selectInput(
              inputId = ns("analysis_eda_variable_regression"),
              label = "Linear regression: ",
              multiple = FALSE,
              choices = c('None', 'Include'),
              selected = 'None'
            ),
            bsPopover(id = ns('analysis_eda_variable_regression'),
                      title = "Linear regression",
                      content = 'Apply a linear regression (y ~ x) to each subgroup of your plot. If you facet on a variable, then the regressions will be calculated per each facet group.',
                      placement = 'top'),
          ), 
          conditionalPanel(
            condition = "input.analysis_eda_select_plot_type == 'Histogram'",
            ns = ns,
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
            condition = "input.analysis_eda_select_plot_type == 'Boxplot'",
            ns = ns,
            selectInput(
              inputId = ns("analysis_eda_variable_group"),
              label = "Grouping: ",
              multiple = FALSE,
              choices = categorical_names
            )
          ),
          selectInput(
            inputId = ns("analysis_eda_variable_facet"),
            label = "Facet variable: ",
            multiple = FALSE,
            choices = c("None", categorical_names),
            selected = "None"
          ),
          bsPopover(id = ns('analysis_eda_variable_facet'),
                    title = "Facet variable",
                    content = 'Faceting splits the data by one or more variables and then plots these subsets.',
                    placement = 'top'),
          conditionalPanel(
            condition = "input.analysis_eda_variable_facet != 'None'",
            ns = ns,
            selectInput(
              inputId = ns("analysis_eda_variable_facet_second"),
              label = "Second facet variable: ",
              multiple = FALSE,
              choices = c("None"),
              selected = "None"
            )
          ),
          conditionalPanel(
            condition = "input.analysis_eda_select_plot_type == 'Scatter'",
            ns = ns,
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
        actionButton(inputId = ns("analysis_eda_button_download"),
                     label = "Download the plot"),
        br(),
        HTML('</details><br>'),
        br(),br(),
        div(
          class = 'backNextContainer',
          actionButton(inputId = ns("analysis_plots_descriptive_button_back"),
                       label = "Back"),
          actionButton(inputId = ns("analysis_plots_descriptive_button_next"),
                       label = "Next")
        )
      ),
      
      mainPanel(
        width = 6,
        br(),
        absolutePanel(id = ns("analysis_eda_loading_message"),
                      HTML("Please wait. Building plot..."),
                      style = "z-index: -2;"),
        plotOutput(outputId = ns('analysis_eda_plot'),
                   height = 600,
                   brush = brushOpts(id = ns("analysis_eda_plot_brush"))),
        br(),
        htmlOutput(outputId = ns("analysis_eda_brush_text")),
        DT::dataTableOutput(outputId = ns("analysis_eda_brush_info"))
      )
    )
  )
}

edaServer <- function(id, input_data) {
  ns <- NS(id)
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # build the exploration plots
      output$analysis_eda_plot <- renderPlot({
        p <- plot_exploration(
          .data = input_data,
          .plot_type = input$analysis_eda_select_plot_type,
          .x = input$analysis_eda_variable_x,
          .y = input$analysis_eda_variable_y,
          .fill = input$analysis_eda_variable_fill,
          .fill_static = "#5c5980",
          .size = input$analysis_eda_variable_size,
          .alpha = input$analysis_eda_variable_alpha,
          .vars_pairs = input$analysis_eda_variable_pairs_vars,
          .n_bins = input$analysis_eda_variable_n_bins,
          .jitter = input$analysis_eda_check_jitter,
          .groups = input$analysis_eda_variable_group,
          .facet = input$analysis_eda_variable_facet,
          .facet_second = input$analysis_eda_variable_facet_second,
          .include_regression = input$analysis_eda_variable_regression
        )
        
        return(p)
      })
      
      # text above the brush table
      output$analysis_eda_brush_text <- renderText({
        
        if (input$analysis_eda_variable_facet == "None" & input$analysis_eda_select_plot_type == 'Scatter') {
          txt <- "<h4>Highlight data points on the above plot to view their information below</h4>"
        } else {
          txt <- NULL
        }
        
        return(txt)
      })
      
      # table of brushed data points from plot
      output$analysis_eda_brush_info <- DT::renderDataTable(
        
        # show only if there isn't faceting
        if (input$analysis_eda_variable_facet == "None" & input$analysis_eda_select_plot_type == 'Scatter') {
          
          create_datatable(
            brushedPoints(input_data, input$analysis_eda_plot_brush),
            selection = "none"
          )
        })
      
      # update second facet options so user cannot double facet on the same variable
      # b/c that causes an error
      observeEvent(input$analysis_eda_variable_facet, {
        if (input$analysis_eda_variable_facet != "None") {
          updateSelectInput(
            session = session,
            inputId = "analysis_eda_variable_facet_second",
            choices = setdiff(c("None", categorical_names), input$analysis_eda_variable_facet)
          )
        }
      })
    }
  )
}

