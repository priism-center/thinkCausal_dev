
server_balance <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # back next buttons
      observeEvent(input$analysis_balance_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$eda)
      })
      observeEvent(input$analysis_balance_button_next, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$overlap)
      })
      
      
      # render default values when verified data is saved
      observeEvent(store$analysis$data$verify$analysis_verify_data_save,{
        
        # get covariates
        new_col_names <- colnames(store$verified_df)
        cols_categorical <- store$column_types$categorical
        cols_continuous <- store$column_types$continuous
        X_cols <- grep("^X_", new_col_names, value = TRUE)
        X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
        
        # send them off to the UI
        updateSelectInput(session = session,
                          inputId = 'analysis_balance_select_var',
                          choices = X_cols_continuous,
                          selected = X_cols_continuous
        )
        
      })

     
      # create the balance plot
      balance_plot <- reactive({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # stop here if there are no numeric columns selected
        validate(need(length(input$analysis_balance_select_var) > 0,
                      "No continuous columns available or currently selected"))
        
        # plot it
        X <- store$verified_df
        col_names <- colnames(X)
        treatment_col <- grep("^Z_", col_names, value = TRUE)
        confounder_cols <- input$analysis_balance_select_var
        p <- plotBart::plot_balance(.data = X,
                                    treatment = treatment_col,
                                    confounders = confounder_cols)
        
        # add theme
        p <- p + store$options$theme_custom
        
        return(p)
      })
      output$analysis_balance_plot <- renderPlot({
        
        # stop here if data hasn't been uploaded and selected
        validate_data_verified(store)
        
        # add overlay
        # div_id <- 'analysis_balance_plot
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
        confounder_cols <- input$analysis_balance_select_var
        
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
      downloaded_balance_plot_parameters_df <- reactive(downloaded_balance_plot_parameters$df)

      observeEvent(input$analysis_balance_button_next, {
        store$analysis$eda$downloaded_balance_plot_parameters <- downloaded_balance_plot_parameters_df()
      })
      
      return(store = store)
    }
  )
}

