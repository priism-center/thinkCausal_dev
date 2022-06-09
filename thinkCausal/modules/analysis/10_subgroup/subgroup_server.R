server_subgroup <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id, 
    function(input, output, session) {
      # navagation 
      observeEvent(input$analysis_moderator_check_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$results)
      })

      observeEvent(input$analysis_moderator_analyses_button_results, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$results)
      })
      
      observeEvent(input$analysis_moderator_analyses_button_reproduce, {
        updateNavbarPage(global_session, inputId = "nav", selected = "Reproduce")
      })
      
      
      
      # update pre-specifed moderators 
      observeEvent(store$analysis$subgroup$prespecified_subgroups, {
        options <- store$analysis$subgroup$prespecified_subgroups
        updateSelectInput(session = global_session, 
                          inputId = ns('analysis_subgroup_prespecifed'), 
                          choices = options, 
                          selected = options[1])
      })
      
      
      
      # pre-specifed subgroups 
      analysis_pre_specified_moderators <- reactive({
        validate_model_fit(store)
        validate_prespecifed_moderators(store)
        
        # clean input text 
        cols_categorical <- gsub('X_', '',store$column_types$categorical)
        cols_continuous <- gsub('X_', '', store$column_types$continuous)
        
        # add overlay
        div_id <- 'analysis_results_plot_prespecifed'
        show_message_updating(div_id)
        
        if(input$analysis_subgroup_prespecifed %in% cols_categorical){
          p <- plot_moderator_d_density(store$analysis$model$model, 
                                        moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_prespecifed)]])
        }else{
          p <- plot_moderator_c_loess(store$analysis$model$model, 
                                      moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_prespecifed)]])
        }
        
        p <- p + store$options$theme_custom
        
        # remove overlay
        close_message_updating(div_id)
        return(p)
        
      })
      
      output$analysis_subgroup_prespecified_plot <- renderPlot(analysis_pre_specified_moderators())
      
      # explore subgroups 
      observeEvent(store$analysis$model$model, {
        options <- gsub("X_", '',grep("^X_", colnames(store$verified_df), value = TRUE))
        updateSelectInput(session = global_session,
          inputId = ns('analysis_subgroup_explore'), 
                          label = 'Subgroup results by:',
                          choices = c('', options), 
                          selected = 1)  
      })
    
      
      
      analysis_explore_moderators <- reactive({
        validate_model_fit(store)
        validate(need(input$analysis_subgroup_explore  != '', "Choose a variable for exploratory subgroup analysis"))
        
        
        # add overlay
        div_id <- 'analysis_results_plot_exploratory'
        show_message_updating(div_id)
        
        cols_categorical <- gsub('X_', '',store$column_types$categorical)
        cols_continuous <- gsub('X_', '', store$column_types$continuous)
        
        if(input$analysis_subgroup_explore %in% cols_categorical){
          p <- plot_moderator_d_density(store$analysis$model$model, 
                                        moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_explore)]])
        }else{
          p <- plot_moderator_c_loess(store$analysis$model$model, 
                                      moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_explore)]])
        }
        
        p <- p + store$options$theme_custom
        
        # remove overlay
        close_message_updating(div_id)
        return(p)
        
      })
      
      output$analysis_subgroup_explore_plot <- renderPlot(analysis_explore_moderators())
      
      
      # ICATE plots
      analysis_moderators_icate_plot <- reactive({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        if(input$icate_plot_type == 'Histogram'){
          p <- plotBart::plot_ICATE(
            store$analysis$model$model,
            n_bins = input$plotBart_ICATE_n_bins)
          # add theme
          p <- p + store$options$theme_custom 
        }
        
        if(input$icate_plot_type == 'Waterfall'){
          # plot it
          p <- plotBart::plot_waterfall(
            .model = store$analysis$model$model
          )
          
          # add theme
          p <- p + store$options$theme_custom 
        }
        
        return(p)
      })
      
      
      output$analysis_moderators_icate_plot <- renderPlot(analysis_moderators_icate_plot())
      
      # 
      # plot explore tab regression tree
      analysis_subgroup_search_plot <- reactive({
        validate_model_fit(store)
        p <- plotBart::plot_moderator_search(store$analysis$model$model, max_depth = input$plotBart_tree_depth)
        return(p)
      })
      
      output$analysis_subgroup_search_plot <- renderPlot(analysis_subgroup_search_plot())
      
    
      # download plot
      output$download_subgroup_plot <- downloadHandler(
        filename = function() {
          switch(
            req(input$analysis_subgroup_tabs),
            "Pre-specified Subgroup Analysis" = 'pre_specified_subgroup_plot.png',
            "Search" = 'subgroup_regression_tree.png',
            "Exploratory Subgroup Analysis" = 'exploratory_subgroup_plot.png',
            "ICATE" = 'ICATE_plot.png'
          )
        },
        content = function(file) {
          
          # get the plot that is on the active tab
          active_plot <- switch(
            req(input$analysis_subgroup_tabs),
            'Pre-specified Subgroup Analysis' = analysis_pre_specified_moderators(),
            'Search' = analysis_subgroup_search_plot(),
            "Exploratory Subgroup Analysis" = analysis_explore_moderators(), 
            'ICATE' = analysis_moderators_icate_plot()
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