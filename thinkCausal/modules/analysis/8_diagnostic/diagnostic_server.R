
server_diagnostic <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # back next buttons
      observeEvent(input$analysis_diagnostics_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$model)
      })
      observeEvent(input$analysis_diagnostics_button_next, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$results)
      })
      
      # render either both the back and next buttons or just the back if its a bad
      # model fit
      output$analysis_diagnosis_buttons_ui <- renderUI({
        if (isTRUE(store$analysis$model$fit_good)){
          tagList(
            div(
              class = 'backNextContainer',
              actionButton(inputId = ns("analysis_diagnostics_button_back"),
                           label = "Back to specify model"),
              actionButton(inputId = ns("analysis_diagnostics_button_next"),
                           class = "nav-btn-focus",
                           label = "Model results")
            )
          )
        } else {
          # actionButton(inputId = "analysis_diagnostics_button_back",
          #              label = "Back to specify model")
          tagList(
            div(
              class = 'backNextContainer',
              actionButton(inputId = ns("analysis_diagnostics_button_back"),
                           label = "Back to specify model"),
              actionButton(inputId = ns("analysis_diagnostics_button_next"),
                           class = "nav-btn-focus",
                           label = "Proceed to model results")
            )
          )
          
          
        }
      })
      
      # update variables on the residual plot page once the model is fitted
      observe({
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        if(isTRUE(store$analysis$model$fit_good)){
          # new_col_names <- colnames(store$verified_df)
          # X_cols <- grep("^X_", new_col_names, value = TRUE)
          # cols_categorical <- store$column_types$categorical
          # cols_continuous <- store$column_types$continuous
          dat <- as.data.frame(store$analysis$model$model$data.rsp@x)
          col_names <- colnames(dat)[-ncol(dat)]
          
          updateSelectInput(
            session = session,
            inputId = "analysis_diagnostics_plot_residual_covariate",
            choices = c("None", col_names),
            selected = "None"
          )
        }
      })
      
      
      # trace plot
      analysis_diagnostics_plot_trace <- reactive({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        # call function
        p <- plotBart::plot_trace(.model = store$analysis$model$model)
        
        # add theme
        p <- p + store$options$theme_custom
        
        return(p)
      })
      output$analysis_diagnostics_plot_trace <- renderPlot(analysis_diagnostics_plot_trace())
      
      # common support plot
      analysis_diagnostics_plot_support <- reactive({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        bart_model <- store$analysis$model$model
        total_delete_sd <- sum(bart_model$sd.cf > max(bart_model$sd.obs) + sd(bart_model$sd.obs))
        total_delete_chi <- sum((bart_model$sd.cf / bart_model$sd.obs) ** 2 > 3.841)
        
        if(total_delete_sd > 0 | total_delete_chi > 0){
          # common support plot from plotbart
          p1 <- plotBart::plot_common_support(
            .model = bart_model,
            rule = 'both'
          )
          # plot classification tree on covariates in terms of overlap
          p2 <- plot_overlap_covariate_tree(.model = bart_model, rule = "sd")
          p3 <- plot_overlap_covariate_tree(.model = bart_model, rule = "chi")
        
          # patchwork package to combine the plots
          p <- p1 | (p2 / p3)
            
        }else{
          # plot it
          p <- plotBart::plot_common_support(
            .model = bart_model,
            rule = 'both'
          )
        }
        
        # add theme
        p <- p +
          store$options$theme_custom +
          theme(legend.position = 'bottom',
                strip.text = element_text(hjust = 0))
        
        return(p)
      })
      output$analysis_diagnostics_plot_support <- renderPlot(analysis_diagnostics_plot_support())
      
      # residual plot
      analysis_diagnostics_plot_residual <- reactive({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        # if "none" is selected, change it to NULL, otherwise remain the same
        covariates_selection <- switch(input$analysis_diagnostics_plot_residual_covariate, "None" = NULL, input$analysis_diagnostics_plot_residual_covariate)
        
        bart_model <- store$analysis$model$model
        # p1 <- plot_residual_observed_predicted(.model = bart_model, 
        #                                        covariate = covariates_selection)
        p2 <- plot_residual_density(.model = bart_model,
                                    covariate = covariates_selection)
        p3 <- plot_residual_observed_residual(.model = bart_model,
                                              covariate = covariates_selection)
        
        # patchwork package to combine the plots
        p <- p2 / p3
        
        # add theme
        p <- p + store$options$theme_custom
        
        return(p)
      })
      output$analysis_diagnostics_plot_residual <- renderPlot(analysis_diagnostics_plot_residual())
      
      # download plot
      output$download_diagnostic_plot <- downloadHandler(
        filename = function() {
          switch(
            req(input$analysis_diagnostics_tabs),
            "Trace plot" = 'diagnostic_trace_plot.png',
            "Common support" = 'diagnostic_common_support_plot.png',
            "Residual plot" = 'diagnostic_residual_plot.png'
          )
        },
        content = function(file) {
          
          # get the plot that is on the active tab
          active_plot <- switch(
            req(input$analysis_diagnostics_tabs),
            'Trace plot' = analysis_diagnostics_plot_trace(),
            'Common support' = analysis_diagnostics_plot_support(),
            "Residual plot" = analysis_diagnostics_plot_residual()
          )
          
          # write out plot
          ggsave(file,
                 plot = active_plot,
                 height = store$options$settings_options_ggplotHeight,
                 width = store$options$settings_options_ggplotWidth,
                 units = 'in',
                 device = 'png')
        }
      )
      return(store)
    }
  )
}

