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
          inputId = ns("exploration_select_plot_type"),
          label = "Plot type:",
          multiple = FALSE,
          choices = c("Pairs", "Scatter", "Histogram", "Density", "Boxplot"),
          selected = "Pairs"
        ),
        conditionalPanel(
          condition = "input.exploration_select_plot_type == 'Pairs'",
          ns = ns,
          selectInput(
            inputId = ns("exploration_variable_pairs_vars"),
            label = "Columns to plot",
            multiple = TRUE,
            choices = col_names,
            selected = col_names
          )
        ),
        conditionalPanel(
          condition = "input.exploration_select_plot_type != 'Pairs'",
          ns = ns,
          selectInput(
            inputId = ns("exploration_variable_x"),
            label = "X: ",
            multiple = FALSE,
            choices = col_names,
            selected = col_names[1]
          ),
          conditionalPanel(
            condition = "input.exploration_select_plot_type == 'Scatter'",
            ns = ns,
            selectInput(
              inputId = ns("exploration_variable_y"),
              label = "Y: ",
              multiple = FALSE,
              choices = col_names,
              selected = col_names[2]
            ),
            selectInput(
              inputId = ns("exploration_variable_fill"),
              label = "Fill color: ",
              multiple = FALSE,
              choices = col_names,
              selected = col_names[12]
            ),
            conditionalPanel(
              condition = "input.exploration_variable_fill == 'Cluster'",
              ns = ns,
              selectInput(
                inputId = ns("exploration_variable_cluster"),
                label = "Clustering algorithm: ",
                multiple = FALSE,
                choices = c('k-means', 'Hierarchical'),
                selected = 'k-means'
              ),
              sliderInput(
                inputId = ns("exploration_variable_n_clusters"),
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
              inputId = ns("exploration_variable_size"),
              label = "Size: ",
              multiple = FALSE,
              choices = col_names,
              selected = col_names[4]
            ),
            selectInput(
              inputId = ns("exploration_variable_regression"),
              label = "Linear regression: ",
              multiple = FALSE,
              choices = c('None', 'Include'),
              selected = 'None'
            ),
            bsPopover(id = ns('exploration_variable_regression'),
                      title = "Linear regression",
                      content = 'Apply a linear regression (y ~ x) to each subgroup of your plot. If you facet on a variable, then the regressions will be calculated per each facet group.',
                      placement = 'top'),
          ), 
          conditionalPanel(
            condition = "input.exploration_select_plot_type == 'Histogram'",
            ns = ns,
            sliderInput(
              inputId = ns("exploration_variable_n_bins"),
              label = "Number of bins: ",
              min = 5,
              max = 50,
              value = 20,
              step = 1
            )
          ),
          conditionalPanel(
            condition = "input.exploration_select_plot_type == 'Boxplot'",
            ns = ns,
            selectInput(
              inputId = ns("exploration_variable_group"),
              label = "Grouping: ",
              multiple = FALSE,
              choices = categorical_names
            )
          ),
          selectInput(
            inputId = ns("exploration_variable_facet"),
            label = "Facet variable: ",
            multiple = FALSE,
            choices = c("None", categorical_names),
            selected = "None"
          ),
          bsPopover(id = ns('exploration_variable_facet'),
                    title = "Facet variable",
                    content = 'Faceting splits the data by one or more variables and then plots these subsets.',
                    placement = 'top'),
          conditionalPanel(
            condition = "input.exploration_variable_facet != 'None'",
            ns = ns,
            selectInput(
              inputId = ns("exploration_variable_facet_second"),
              label = "Second facet variable: ",
              multiple = FALSE,
              choices = c("None"),
              selected = "None"
            )
          ),
          conditionalPanel(
            condition = "input.exploration_select_plot_type == 'Scatter'",
            ns = ns,
            sliderInput(
              inputId = ns("exploration_variable_alpha"),
              label = "Opacity: ",
              min = 0.1,
              max = 1,
              value = 0.5,
              step = 0.1
            )
          )
        ),
        HTML('<details><summary>Advanced options</summary>'),
        checkboxInput(inputId = ns("exploration_check_jitter"),
                      label = "Jitter the points?",
                      value = FALSE),
        actionButton(inputId = ns("exploration_button_download"),
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
        absolutePanel(id = ns("exploration_loading_message"),
                      HTML("Please wait. Building plot..."),
                      style = "z-index: -2;"),
        plotOutput(outputId = ns('exploration_plot'),
                   height = 600,
                   brush = brushOpts(id = ns("plot1_brush"))),
        br(),
        htmlOutput(outputId = ns("brush_text")),
        DT::dataTableOutput(outputId = ns("brush_info"))
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
      exploration_plot <- reactive({
        
        # set which dataset to use
        plot_data <- input_data
        
        # # add kmeans cluster to data dataframe
        # if(input$exploration_select_plot_type == 'Scatter' & input$exploration_variable_fill == "Cluster"){
        #   
        #   if(input$exploration_variable_cluster == 'k-means'){
        #     # run kmeans algo
        #     km <- kmeans(x = plot_data[, c(input$exploration_variable_x, input$exploration_variable_y)],
        #                  centers = input$exploration_variable_n_clusters, iter.max = 50, nstart = 5)
        #     
        #     # add cluster assignment to the dataframe
        #     plot_data$Cluster <- as.factor(km$cluster)
        #   } else {
        #     # run hclust algo 
        #     dist_matrix <- dist(plot_data[, c(input$exploration_variable_x, input$exploration_variable_y)])
        #     clust <- hclust(d = dist_matrix,  method = 'ward.D2')
        #     
        #     # add cluster assignment to the dataframe
        #     plot_data$Cluster <- as.factor(cutree(clust, input$exploration_variable_n_clusters))
        #   }
        # }
        
        # create base ggplot object
        p <- ggplot(plot_data, aes_string(x = sym(input$exploration_variable_x)))
        
        # pairs plit
        if (input$exploration_select_plot_type == 'Pairs'){
          p <- GGally::ggpairs(plot_data[, input$exploration_variable_pairs_vars])
        }
        
        # scatter
        if (input$exploration_select_plot_type == 'Scatter'){
          
          if (input$exploration_check_jitter){
            p <- p +
              geom_jitter(aes_string(y = sym(as.character(input$exploration_variable_y)),
                                    fill = sym(input$exploration_variable_fill),
                                    size = sym(input$exploration_variable_size),
                                    color = sym(input$exploration_variable_fill)),
                         alpha = input$exploration_variable_alpha)
          } else {
            p <- p +
              geom_point(aes_string(y = sym(as.character(input$exploration_variable_y)),
                                    fill = sym(input$exploration_variable_fill),
                                    size = sym(input$exploration_variable_size),
                                    color = sym(input$exploration_variable_fill)),
                         alpha = input$exploration_variable_alpha)
          }

          # regression line
          if(input$exploration_variable_regression == 'Include'){
            p <- p + geom_smooth(
              aes_string(y = sym(input$exploration_variable_y)),
              method = "lm",
              formula = 'y ~ x',
              color = "grey20"
            )
          }

          # cluster centers
          # if(input$exploration_variable_fill == "Cluster"){
          #   p <- p +
          #     geom_point(data = as_tibble(km$centers),
          #                aes_string(x = sym(colnames(km$centers)[1]),
          #                           y = sym(colnames(km$centers)[2])),
          #                color = violet_col,
          #                shape = 4, size = 8, stroke = 1.5)
          # }

        }

        # histogram
        if (input$exploration_select_plot_type == 'Histogram'){
          p <- p + geom_histogram(color = 'white', bins = input$exploration_variable_n_bins,
                                  fill = violet_col, alpha = 0.9) +
            labs(y = NULL)
        }

        # density
        if (input$exploration_select_plot_type == 'Density'){
          p <- p + geom_density(fill = violet_col, alpha = 0.5) +
            labs(y = NULL)
        }

        # boxplot
        if (input$exploration_select_plot_type == 'Boxplot'){
          p <- p +
            geom_boxplot(fill = violet_col, alpha = 0.5,
                         if(input$exploration_variable_group != 'None') aes_string(y = sym(input$exploration_variable_group))
            ) +
            coord_flip() +
            scale_y_discrete()
        }

        # add faceting
        if (input$exploration_variable_facet != "None"){

          if (input$exploration_variable_facet_second == "None"){
            p <- p + facet_grid(sym(input$exploration_variable_facet), labeller = label_both)
          } else {
            p <- p + facet_grid(list(sym(input$exploration_variable_facet), sym(input$exploration_variable_facet_second)),
                                labeller = label_both)
          }
        }
        
        # show plot
        return(p)
      })
      
      # render site exploration plots
      output$exploration_plot <- renderPlot(exploration_plot())
      
      # text above the brush table
      output$brush_text <- renderText({
        
        if (input$exploration_variable_facet == "None" & input$exploration_select_plot_type == 'Scatter') {
          txt <- "<h4>Highlight data points on the above plot to view their information below</h4>"
        } else {
          txt <- NULL
        }
        
        return(txt)
      })
      
      # table of brushed data points from plot
      output$brush_info <- DT::renderDataTable(
        
        # show only if there isn't faceting
        if (input$exploration_variable_facet == "None" & input$exploration_select_plot_type == 'Scatter') {
          
          pretty_datatable(
            brushedPoints(user_data, input$plot1_brush),
            selection = "none"
          )
        })
      
      # update second facet options so user cannot double facet on the same variable
      # b/c that causes an error
      observeEvent(input$exploration_variable_facet, {
        if (input$exploration_variable_facet != "None") {
          updateSelectInput(
            session = session,
            inputId = "exploration_variable_facet_second",
            choices = setdiff(c("None", categorical_names), input$exploration_variable_facet)
          )
        }
      })
    }
  )
}

