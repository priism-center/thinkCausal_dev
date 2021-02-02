# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


  # next buttons ------------------------------------------------------------
  
  # data page
  observeEvent(input$analysis_data_load_button_next, {
    updateTabsetPanel(session, inputId ="analysis_data_tabs", selected ="Select Data")
  })
  observeEvent(input$analysis_data_select_button_next, {
    updateTabsetPanel(session, inputId ="analysis_data_tabs", selected = "Study Design")
  })
  observeEvent(input$analysis_data_design_button_next, {
    updateNavbarPage(session, inputId = "nav", selected = "Exploratory Plots")
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
  })
  
  # plotting page
  
  # model page
  
  # diagnostics page
  
  # 


  # upload data -------------------------------------------------------------

  # read in the CSV that was uploaded
  uploaded_df <- reactive({
    # TODO: modify so it can handle other datatypes
    # TODO: add parsing failures to log
    req(input$analysis_data_upload)
    tryCatch({
      upload_csv <- read_csv(
        file = input$analysis_data_upload$datapath,
        col_names = input$analysis_data_header)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs or if dataset isn't yet uploaded
      stop(safeError(e))
    })
    
    return(upload_csv)
  })  
  
  # table of selected dataset
  output$analysis_data_table <- DT::renderDataTable({
    custom_datatable(
      uploaded_df(),
      selection = "none"
    )
  })
  
  
  # EDA ---------------------------------------------------------------------

  output$analysis_plot_balance_plot <- renderPlot({
    
    # choose which variables to include
    if (isTRUE(input$all_balance)){
      selected_cols <- X_names
    } else {
      selected_cols <- input$balance_var
    }
    
    # plot it
    user_data %>% 
      # dplyr::select(-c('re78', 'u74', 'u75')) %>% 
      dplyr::select(all_of(c(selected_cols, 'z'))) %>% 
      pivot_longer(cols = -c('z')) %>% 
      group_by(name) %>% 
      mutate(value = scale(value)) %>% 
      group_by(name, z) %>% 
      summarize(mean = mean(value),
                .groups = 'drop') %>% 
      group_by(name) %>% 
      summarize(diff = mean - lag(mean),
                .groups = 'drop') %>% 
      na.omit() %>% 
      ggplot(aes(x=diff, y=name, color=abs(diff))) +
      geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray60') +
      geom_point(size=4) +
      scale_colour_gradient(low = 'gray30', high = 'red3') + #or should color be scaled to finite values?
      labs(title = 'Treatment and control balance',
           x = 'Scaled mean difference',
           y = NULL) +
      theme(legend.position = 'none')
  })
  
  # build the exploration plots
  exploration_plot <- reactive({
    
    # set which dataset to use
    plot_data <- user_data
    
    # add kmeans cluster to data dataframe
    if(input$exploration_select_plot_type == 'Scatter' & input$exploration_variable_fill == "Cluster"){
      
      if(input$exploration_variable_cluster == 'k-means'){
        # run kmeans algo
        km <- kmeans(x = plot_data[, c(input$exploration_variable_x, input$exploration_variable_y)],
                     centers = input$exploration_variable_n_clusters, iter.max = 50, nstart = 5)
        
        # add cluster assignment to the dataframe
        plot_data$Cluster <- as.factor(km$cluster)
      } else {
        # run hclust algo 
        dist_matrix <- dist(plot_data[, c(input$exploration_variable_x, input$exploration_variable_y)])
        clust <- hclust(d = dist_matrix,  method = 'ward.D2')
        
        # add cluster assignment to the dataframe
        plot_data$Cluster <- as.factor(cutree(clust, input$exploration_variable_n_clusters))
      }
    }
    
    # create base ggplot object
    p <- ggplot(plot_data, aes_string(x = sym(input$exploration_variable_x)))
    
    # scatter
    if (input$exploration_select_plot_type == 'Scatter'){
      p <- p +
        geom_point(aes_string(y = sym(input$exploration_variable_y),
                              fill = sym(input$exploration_variable_fill),
                              size = sym(input$exploration_variable_size),
                              color = sym(input$exploration_variable_fill)),
                   alpha = input$exploration_variable_alpha)
      
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
      
      custom_datatable(
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
  

  # concepts ----------------------------------------------------------------
  
  # add listeners that link the concepts title image to its article
  tab_titles <- c("Randomization", 'Fundamental problem', 'Assumptions', 'Regression methods')
  lapply(tab_titles, function(page_to_go_to) {
    page_id <- paste0("concepts_link_", page_to_go_to)
    observeEvent(input[[page_id]], {
      updateNavbarPage(session, "nav", page_to_go_to)
    })
  })
  
  # run the randomization module
  randomizationServer(id = 'concepts_randomization')

})
