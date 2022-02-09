
server_eda <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # back next buttons
      observeEvent(input[['analysis_plots_descriptive_button_back']], {
        updateNavbarPage(global_session, inputId = "nav", selected = "Data")
        updateTabsetPanel(global_session, inputId = "analysis_data_tabs", selected = "Verify")
      })
      observeEvent(input[['analysis_plots_descriptive_button_next']], {
        updateTabsetPanel(global_session, inputId = "analysis_plot_tabs", selected = "Common Support Plots")
      })
      observeEvent(input$analysis_plots_support_button_back, {
        updateTabsetPanel(global_session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
      })
      observeEvent(input$analysis_plots_support_button_next, {
        updateTabsetPanel(global_session, inputId = "analysis_plot_tabs", selected = "Balance Plots")
      })
      observeEvent(input$analysis_plots_balance_button_back, {
        updateTabsetPanel(global_session, inputId = "analysis_plot_tabs", selected = "Common Support Plots")
      })
      observeEvent(input$analysis_plots_balance_button_next, {
        updateNavbarPage(global_session, inputId = "nav", selected = "Model")
      })
      
      # update variables on the eda page once the save button on the verify data page is clicked
      observeEvent(store$analysis$data$verify$analysis_data_save, {
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
      
      # create the descriptive plots
      # build the exploration plots
      descriptive_plot <- reactive( {
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        p <- tryCatch({
          plot_exploration(
            .data = store$verified_df,
            .plot_type = input$analysis_eda_select_plot_type,
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
      
      # calculate propensity scores
      pscores <- reactive({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # stop here if there are no numeric columns selected
        validate(need(length(input$analysis_plot_overlap_select_var) > 0,
                      "No continuous columns available or currently selected"))
        
        # get variables
        X <- store$verified_df
        col_names <- colnames(X)
        treatment_col <- grep("^Z_", col_names, value = TRUE)
        response_col <- grep("^Y_", col_names, value = TRUE)
        cols_continuous <- store$column_types$continuous
        confounder_cols <- grep("^X_", cols_continuous, value = TRUE)
        
        # calculate pscores
        pscores <- plotBart:::propensity_scores(
          .data = X,
          treatment = treatment_col,
          response = response_col,
          confounders = confounder_cols,
          seed = 44
        )
        
        return(pscores)
      })
      
      # create the overlap plot
      overlap_plot <- reactive({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # stop here if there are no numeric columns selected
        validate(need(length(input$analysis_plot_overlap_select_var) > 0,
                      "No continuous columns available or currently selected"))
        
        # get variables for input into plotting functions
        X <- store$verified_df
        col_names <- colnames(X)
        treatment_col <- grep("^Z_", col_names, value = TRUE)
        response_col <- grep("^Y_", col_names, value = TRUE)
        cols_continuous <- store$column_types$continuous
        confounder_cols <- grep("^X_", cols_continuous, value = TRUE)
        plt_type <- input$analysis_plot_overlap_method
      
        # plot either the variables or the 1 dimension propensity scores
        if(input$analysis_plot_overlap_type == 1){
          p <- tryCatch(
            plotBart::plot_overlap_vars(
              .data = X,
              treatment = treatment_col,
              confounders = input$analysis_plot_overlap_select_var,
              plot_type = plt_type
            ),
            error = function(e) NULL
          )
        }
        
        # TODO: should pscores include all vars or what is just included in the select input?
        else if(input$analysis_plot_overlap_type == 2){
          p <- tryCatch({
            plotBart::plot_overlap_pScores(
              .data = X,
              treatment = treatment_col,
              response = response_col,
              confounders = confounder_cols,
              plot_type = plt_type,
              pscores = pscores()
            )
          },
          error = function(e) NULL
          )
        }
        
        # add theme
        p <- p + store$options$theme_custom
        
        return(p)
      })
      output$analysis_plot_overlap_plot <- renderPlot({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # stop here if there are no numeric columns selected
        validate(need(length(input$analysis_plot_overlap_select_var) > 0,
                      "No continuous columns available or currently selected"))
        
        # add overlay
        div_id <- 'analysis_plot_overlap_plot'
        show_message_updating(div_id)
        
        # build plot
        p <- overlap_plot()
        
        # remove overlay
        close_message_updating(div_id)
        
        # stop if p is not a plot
        validate(need(
          inherits(p, 'ggplot'),
          'Error in building plot. Error likely occured in propensity score calculation.'
        ))
        
        return(p)
      })
      
      # to save the parameters of downloaded overlap plots for reproducible script
      downloaded_overlap_plot_parameters <- reactiveValues(df = list())
      
      # parameters of current overlap plot
      overlap_plot_parameters <- reactive({
        # get variables
        X <- store$verified_df
        col_names <- colnames(X)
        treatment_col <- grep("^Z_", col_names, value = TRUE)
        response_col <- grep("^Y_", col_names, value = TRUE)
        cols_continuous <- store$column_types$continuous
        confounder_cols <- grep("^X_", cols_continuous, value = TRUE)
        
        if(input$analysis_plot_overlap_type == 1){ # overlap plot by variables
          list(analysis_plot_overlap_type = 1,
               treatment = treatment_col,
               response = response_col,
               confounders = paste(input$analysis_plot_overlap_select_var, collapse = ','),
               plot_type = input$analysis_plot_overlap_method)
        }else{ # p-score plot
          list(analysis_plot_overlap_type = 2,
               treatment = treatment_col,
               response = response_col,
               confounders = paste(confounder_cols, collapse = ','),
               plot_type = input$analysis_plot_overlap_method)
        }
      })
      
      output$download_overlap_plot <- downloadHandler(
        filename = 'overlap_plot.png',
        content = function(file) {
          ggsave(file,
                 plot = overlap_plot(),
                 height = store$options$settings_options_ggplotHeight,
                 width = store$options$settings_options_ggplotWidth,
                 units = 'in',
                 device = 'png')
          # save the parameters of the downloaded overlap plot
          downloaded_overlap_plot_parameters$df <- rbind(downloaded_overlap_plot_parameters$df, overlap_plot_parameters())
        })
      
      # create the balance plot
      balance_plot <- reactive({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # stop here if there are no numeric columns selected
        validate(need(length(input$analysis_plot_balance_select_var) > 0,
                      "No continuous columns available or currently selected"))
        
        # plot it
        X <- store$verified_df
        col_names <- colnames(X)
        treatment_col <- grep("^Z_", col_names, value = TRUE)
        confounder_cols <- input$analysis_plot_balance_select_var
        p <- plotBart::plot_balance(.data = X,
                                    treatment = treatment_col,
                                    confounders = confounder_cols)
        
        # add theme
        p <- p + store$options$theme_custom
        
        return(p)
      })
      output$analysis_plot_balance_plot <- renderPlot({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # add overlay
        # div_id <- 'analysis_plot_balance_plot
        # show_message_updating(div_id)
        
        # build plot
        p <- balance_plot()
        
        # remove overlay
        # close_message_updating(div_id)
        
        return(p)
      })
      
      # to save the parameters of downloaded balance plots for reproducible script
      downloaded_balance_plot_parameters <- reactiveValues(df = list())
      
      # parameters of current balance plot
      balance_plot_parameters <- reactive({
        # get variables
        X <- store$verified_df
        col_names <- colnames(X)
        treatment_col <- grep("^Z_", col_names, value = TRUE)
        confounder_cols <- input$analysis_plot_balance_select_var
        
        list(treatment = treatment_col,
             confounders = paste(confounder_cols, collapse = ','))
        
      })
      
      output$download_balance_plot <- downloadHandler(
        filename = 'balance_plot.png',
        content = function(file) {
          ggsave(file,
                 plot = balance_plot(),
                 height = store$options$settings_options_ggplotHeight,
                 width = store$options$settings_options_ggplotWidth,
                 units = 'in',
                 device = 'png')
          # save the parameters of the downloaded overlap plot
          downloaded_balance_plot_parameters$df <- rbind(downloaded_balance_plot_parameters$df, balance_plot_parameters())
        })
      
      # save into store for reproducible script
      downloaded_descriptive_plot_parameters_df <- reactive(downloaded_descriptive_plot_parameters$df)
      downloaded_overlap_plot_parameters_df <- reactive(downloaded_overlap_plot_parameters$df)
      downloaded_balance_plot_parameters_df <- reactive(downloaded_balance_plot_parameters$df)
      
      observeEvent(input$analysis_plots_balance_button_next, {
        store$analysis$eda$downloaded_descriptive_plot_parameters <- downloaded_descriptive_plot_parameters_df()
        store$analysis$eda$downloaded_overlap_plot_parameters <- downloaded_overlap_plot_parameters_df()
        store$analysis$eda$downloaded_balance_plot_parameters <- downloaded_balance_plot_parameters_df()
      })
      
      return(store = store)
    }
  )
}

