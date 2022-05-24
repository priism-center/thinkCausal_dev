
server_eda <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # back next buttons
      observeEvent(input$analysis_plots_descriptive_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$verify)
      })

      observeEvent(input$analysis_plots_descriptive_button_next, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$balance)
      })

      # update variables on the eda page once the save button on the verify data page is clicked
      observeEvent(store$analysis$data$verify$analysis_verify_data_save, {
        new_col_names <- colnames(store$verified_df)
        cols_categorical <- store$column_types$categorical
        cols_continuous <- store$column_types$continuous
        
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_pairs_vars",
          choices = new_col_names,
          selected = new_col_names
        )
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_select_plot_type",
          selected = store$analysis$data$verify$plot_vars$plot_type
        )
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_x",
          choices = new_col_names,
          selected = store$analysis$data$verify$plot_vars$X
        )
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_y",
          choices = new_col_names,
          selected = store$analysis$data$verify$plot_vars$Y
        )
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_fill",
          choices = c("None", new_col_names),
          selected = store$analysis$data$verify$plot_vars$fill
        )
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_shape",
          choices = c("None", cols_categorical),
          selected = store$analysis$data$verify$plot_vars$shape
        )
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_size",
          choices = c("None", new_col_names),
          selected = store$analysis$data$verify$plot_vars$size
        )
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_group",
          choices = c("None", cols_categorical),
          selected = store$analysis$data$verify$plot_vars$grouping
        )
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_facet",
          choices = c("None", cols_categorical)
        )
        
        # update selects on balance plots
        X_cols <- grep("^X_", new_col_names, value = TRUE)
        X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
        
        # update options for balance
        updateSelectInput(session = session,
                          inputId = 'analysis_plot_balance_select_var',
                          choices = X_cols_continuous,
                          selected = X_cols_continuous
        )
        updateSelectInput(session = session,
                          inputId = 'analysis_plot_overlap_select_var',
                          choices = X_cols_continuous,
                          selected = X_cols_continuous
        )
      })
      
      # only show continuous variables if histogram, density, or boxplot is selected
      # only show categorical if barplot
      observeEvent(input$analysis_eda_select_plot_type, {
        
        plot_type <- input$analysis_eda_select_plot_type
        selection_current <- input$analysis_eda_variable_x
        
        if (plot_type %in% c("Histogram", "Density", "Boxplot")){
          
          # update the available variables to just continuous and keep the current
          # selection if its continuous
          vars_continuous <- store$column_types$continuous
          selection_new <- ifelse(selection_current %in% vars_continuous,
                                  selection_current,
                                  vars_continuous[1])
          
          updateSelectInput(
            session = session,
            inputId = "analysis_eda_variable_x",
            choices = vars_continuous,
            selected = selection_new
          )
        } else if (plot_type == "Barplot") {
          
          # update the available variables to just categorical and keep the current
          # selection if its categorical
          vars_categorical <- store$column_types$categorical
          selection_new <- ifelse(selection_current %in% vars_categorical,
                                  selection_current,
                                  vars_categorical[1])
          updateSelectInput(
            session = session,
            inputId = "analysis_eda_variable_x",
            choices = vars_categorical,
            selected = selection_new
          )
        } else {
          updateSelectInput(
            session = session,
            inputId = "analysis_eda_variable_x",
            choices = colnames(store$verified_df),
            selected = selection_current
          )
        }
      })
      
      output$render_analysis_eda_variable_x <- renderUI({
        new_col_names <- colnames(store$verified_df)
        cols_categorical <- store$column_types$categorical
        cols_continuous <- store$column_types$continuous
        selectInput(
          inputId = ns("analysis_eda_variable_x"),
          label = "X: ",
          multiple = FALSE,
          choices = new_col_names,
          selected = NULL
        )
      })
      
     
      find_levels <- reactive({
        # identify all logicals and indicators 
        cols_indicators <- names(store$verified_df)[-c(1:2)][store$current_simple_column_types[-c(1,2)] == 'Binary']
      
        #req(input$analysis_eda_variable_x) # to prevent rendering error
        req(input$analysis_eda_variable_x)
        
        if(input$analysis_eda_variable_x %in%  cols_indicators){
          levels <- identify_indicators(
            x = input$analysis_eda_variable_x, # level to test
            store$verified_df[, cols_indicators] # possible categorical variables
            )
          
          return(levels$best)
        } else return(NULL)
      })
      
      
      output$render_analysis_eda_x_levels <- renderUI({
        levels_ui <- find_levels()
        if(length(levels_ui) > 1) {
          selectInput(
            inputId = ns("analysis_eda_x_levels"),
            label = paste0("Contrast ", input$analysis_eda_variable_x, " with:"),
            multiple = TRUE,
            # removing selected .x from options
            choices = levels_ui[which(levels_ui != input$analysis_eda_variable_x)],
            selected = levels_ui[which(levels_ui != input$analysis_eda_variable_x)]
          )
        }else NULL
          
        })
    
  
      # create the descriptive plots
      # build the exploration plots
      descriptive_plot <- reactive({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        p <- tryCatch({
          plot_exploration(
            .data = store$verified_df,
            .plot_type = input$analysis_eda_select_plot_type,
            .x = input$analysis_eda_variable_x,
            .y = input$analysis_eda_variable_y,
            .levels = find_levels(),
            .fill = input$analysis_eda_variable_fill,
            .fill_static = 'grey20', #"#5c5980",
            .shape = input$analysis_eda_variable_shape,
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
        }
        # warning = function(e) NULL,
        # error = function(e) NULL
        )
        
        # add theme
        p <- p + store$options$theme_custom
        return(p)
        
      })
      output$analysis_eda_plot <- renderPlot(descriptive_plot())
      
      # to save the parameters of downloaded descriptive plots for reproducible script
      downloaded_descriptive_plot_parameters <- reactiveValues(df = list())
      
      # parameters of current descriptive plot
      descriptive_plot_parameters <-  reactive({
        list(.plot_type = input$analysis_eda_select_plot_type,
             .x = input$analysis_eda_variable_x,
             .y = input$analysis_eda_variable_y,
             .fill = input$analysis_eda_variable_fill,
             .fill_static = 'grey20', #"#5c5980",
             .shape = input$analysis_eda_variable_shape,
             .size = input$analysis_eda_variable_size,
             .alpha = input$analysis_eda_variable_alpha,
             .vars_pairs = input$analysis_eda_variable_pairs_vars,
             .n_bins = input$analysis_eda_variable_n_bins,
             .jitter = input$analysis_eda_check_jitter,
             .groups = input$analysis_eda_variable_group,
             .facet = input$analysis_eda_variable_facet,
             .facet_second = input$analysis_eda_variable_facet_second,
             .include_regression = input$analysis_eda_variable_regression)
      })
      
      output$download_descriptive_plot <- downloadHandler(
        filename = 'descriptive_plot.png',
        content = function(file) {
          ggsave(file,
                 plot = descriptive_plot(),
                 height = store$options$settings_options_ggplotHeight,
                 width = store$options$settings_options_ggplotWidth,
                 units = 'in',
                 device = 'png')
          # save the parameters of the downloaded descriptive plot
          downloaded_descriptive_plot_parameters$df <- rbind(downloaded_descriptive_plot_parameters$df, descriptive_plot_parameters())
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
      output$analysis_eda_brush_info <- DT::renderDataTable({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # show only if there isn't faceting
        if (input$analysis_eda_variable_facet == "None" & input$analysis_eda_select_plot_type == 'Scatter') {
          
          create_datatable(brushedPoints(store$verified_df,
                                         input$analysis_eda_plot_brush),
                           selection = "none")
        }
      })
      
      # update second facet options so user cannot double facet on the same variable
      # b/c that causes an error
      observeEvent(input$analysis_eda_variable_facet, {
        if (input$analysis_eda_variable_facet != "None") {
          updateSelectInput(
            session = session,
            inputId = "analysis_eda_variable_facet_second",
            choices = setdiff(c("None", store$column_types$categorical), input$analysis_eda_variable_facet)
          )
        }
      })
      
      # save into store for reproducible script
      downloaded_descriptive_plot_parameters_df <- reactive(downloaded_descriptive_plot_parameters$df)

      
      observeEvent(input$analysis_plots_descriptive_button_next, {
        store$analysis$eda$downloaded_descriptive_plot_parameters <- downloaded_descriptive_plot_parameters_df()
      })
      
      return(store = store)
    }
  )
}

