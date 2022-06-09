server_overlap <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # back next buttons
      observeEvent(input$analysis_plots_support_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$balance)
      })
      observeEvent(input$analysis_plots_support_button_next, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$model)
      })
      
      
      # render varible options ordered by overlap problems
      observeEvent(input$analysis_overlap_type,{
      
        # get covariates
        new_col_names <- colnames(store$verified_df)
        X_cols <- grep("^X_", new_col_names, value = TRUE)
        
        overlap_df <- data.frame(sd.cf = pscores()[[2]], sd.obs = pscores()[[3]], store$verified_df[, X_cols])
        tree <- rpart::rpart(sd.cf/sd.obs ~ ., data = overlap_df)
        preds <- unique(rownames(tree$splits))
        overlap <- c(preds, X_cols[X_cols %notin% preds])
        
    
        # send them off to the UI
        updateSelectInput(session = session,
                          inputId = 'analysis_overlap_select_var',
                          choices = c('X_bw', overlap),
                          selected = 'X_bw' #overlap[1]
        )
        
      })

      # calculate propensity scores
      pscores <- reactive({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # get variables
        X <- store$verified_df
        col_names <- colnames(X)
        treatment_col <- grep("^Z_", col_names, value = TRUE)
        response_col <- grep("^Y_", col_names, value = TRUE)
        confounder_cols <- grep("^X_", col_names, value = TRUE)
        
         # calculate pscores
        fit <- fit_bart(
          .data = store$verified_df,
          block = store$column_assignments$block,
          .weights = store$column_assignments$weight, 
          ran_eff = store$column_assignments$ran_eff,
          .estimand = 'ate', 
          support = 'none'
        )
        
        overlap_data <- list(p.score = fit$p.score, sd.cf = fit$sd.cf, sd.obs = fit$sd.obs)
        return(overlap_data)
      })
      
      # create the overlap plot
      overlap_plot <- reactive({

        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # stop here if there are no numeric columns selected
        # validate(need(length(input$analysis_overlap_select_var) > 0,
        #               "No continuous columns available or currently selected"))
        
        # get variables for input into plotting functions
        X <- store$verified_df
        col_names <- colnames(X)
        treatment_col <- grep("^Z_", col_names, value = TRUE)
        response_col <- grep("^Y_", col_names, value = TRUE)
        cols_continuous <- store$column_types$continuous
        confounder_cols <- grep("^X_", cols_continuous, value = TRUE)
        plt_type <- input$analysis_overlap_method
      
        # plot either the variables or the 1 dimension propensity scores
        if(input$analysis_overlap_type == 2){
          
          if(plt_type == 'Density') validate(need((is.numeric(X[[input$analysis_overlap_select_var]])), 'Density plots are only avalable for continuous variables'))
          
          req(input$analysis_overlap_select_var)
          
          p <- temp_plot_overlap_vars(
              .data = X,
              treatment = treatment_col,
              confounders = input$analysis_overlap_select_var,
              plot_type = plt_type
            )
        }
        
        else if(input$analysis_overlap_type == 1){
          p <- tryCatch({
            plotBart::plot_overlap_pScores(
              .data = X,
              treatment = treatment_col,
              plot_type = plt_type,
              pscores = pscores()[[1]]
            )
          },
          error = function(e) NULL
          )
        }
        
        # add theme
        p <- p + store$options$theme_custom
        
        return(p)
      })
      output$analysis_overlap_plot <- renderPlot({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # stop here if there are no numeric columns selected
        # validate(need(length(input$analysis_overlap_select_var) > 0,
        #               "No continuous columns available or currently selected"))
        
        # add overlay
        div_id <- 'analysis_overlap_plot'
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
        
        if(input$analysis_overlap_type == 1){ # overlap plot by variables
          list(analysis_overlap_type = 1,
               treatment = treatment_col,
               response = response_col,
               confounders = paste(input$analysis_overlap_select_var, collapse = ','),
               plot_type = input$analysis_overlap_method)
        }else{ # p-score plot
          list(analysis_overlap_type = 2,
               treatment = treatment_col,
               response = response_col,
               confounders = paste(confounder_cols, collapse = ','),
               plot_type = input$analysis_overlap_method)
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
      
   
      # save into store for reproducible script
      downloaded_overlap_plot_parameters_df <- reactive(downloaded_overlap_plot_parameters$df)

      observeEvent(input$analysis_support_button_next, {
        store$analysis$eda$downloaded_overlap_plot_parameters <- downloaded_overlap_plot_parameters_df()
      })
      
      return(store = store)
    }
  )
}

