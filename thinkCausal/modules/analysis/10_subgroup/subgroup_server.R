server_subgroup <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id, 
    function(input, output, session) {
      # snavagation 
      observeEvent(input$analysis_moderator_check_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$results)
      })
      observeEvent(input$analysis_moderator_check_button_next, {
        updateTabsetPanel(global_session, inputId = "analysis_moderator_tabs", selected = "Explore")
      })
      
      observeEvent(input$analysis_moderator_explore_button_back, {
        updateTabsetPanel(global_session, inputId = "analysis_moderator_tabs", selected = "Check")
      })
      
      observeEvent(input$analysis_moderator_explore_button_next, {
        updateTabsetPanel(global_session, inputId = "analysis_moderator_tabs", selected = "Test")
      })
      
      observeEvent(input$analysis_moderator_test_button_back, {
        updateTabsetPanel(global_session, inputId = "analysis_moderator_tabs", selected = "Explore")
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
        
        if(input$analysis_results_prespecifed %in% cols_categorical){
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
      
      # exploratory moderators 
      
      # pre-specifed subgroups 
      observeEvent(input$analysis_model_button_next, {
        updateSelectInput(inputId = ns('analysis_subgroup_explore'), 
                          label = 'Subgroup results by:',
                          choices = gsub("X_", '',grep("^X_", colnames(store$verified_df), value = TRUE)), 
                          selected = NULL
        )  
      })
    
      
      
      analysis_explore_moderators <- reactive({
        validate_model_fit(store)
        validate(need(!is_null(nput$analysis_subgroup_explore), "Choose a variable for exploratory subgroup analysis"))
        
        
        # add overlay
        div_id <- 'analysis_results_plot_exploratory'
        show_message_updating(div_id)
        
        if(input$analysis_results_prespecifed %in% cols_categorical){
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
      # output$download_ICATE_plot <- downloadHandler(
      #   filename = function(){
      #     switch(
      #       input$check_type,
      #       "tree" = "ICATE_tree_plot.png",
      #       "histogram" = "ICATE_histogram_plot.png",
      #       "ordered" = "ICATE_ordered_plot.png"
      #     )
      #   },
      #   content = function(file) {
      #     
      #     if (input$icate_type == 'tree'){
      #       # TODO: this doesn't work
      #       grDevices::png(
      #         filename = file,
      #         height = input$settings_options_ggplotHeight,
      #         width = input$settings_options_ggplotWidth,
      #         units = 'in',
      #         res = 300
      #       )
      #       analysis_moderators_check_plot()
      #       dev.off()
      #     } else {
      #       ggsave(file,
      #              plot = analysis_moderators_check_plot(),
      #              height = input$settings_options_ggplotHeight,
      #              width = input$settings_options_ggplotWidth,
      #              units = 'in',
      #              device = 'png')
      #     }
      #   }
      # )
      # 
      # plot explore tab regression tree
      analysis_subgroup_search_plot <- reactive({
        validate_model_fit(store)
        p <- plotBart::plot_moderator_search(store$analysis$model$model, max_depth = input$plotBart_tree_depth)
        return(p)
      })
      
      output$analysis_subgroup_search_plot <- renderPlot(analysis_subgroup_search_plot())
      
      
      output$download_moderator_explore_plot <- downloadHandler(
        filename = function(){
          "ICATE_tree_plot.png"
        },
        content = function(file) {
          grDevices::png(
            filename = file,
            height = input$settings_options_ggplotHeight,
            width = input$settings_options_ggplotWidth,
            units = 'in',
            res = 300
          )
          analysis_moderator_explore_plot()
          dev.off()
        }
      )
      
      
      # plot the moderators
      analysis_moderators_test_plot <- reactive({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        # stop here if no variables selected in dropdown
        validate(need(input$analysis_moderator_vars, "Please select a variable to group by"))
        
        # add overlay
        div_id <- 'analysis_moderators_test_plot'
        show_message_updating(div_id)
        
        # make plot
        moderator_vars <- input$analysis_moderator_vars
        p <- plot_continuous_sub(.model = store$analysis$model$model,
                                 grouped_on = moderator_vars)
        
        # add theme
        p <- p + store$options$theme_custom 
        
        # remove overlay
        close_message_updating(div_id)
        
        return(p)
      })
      output$analysis_moderators_test_plot <- renderPlot(analysis_moderators_test_plot())
      output$download_ESA_plot <- downloadHandler(
        filename = "moderators_plot.png",
        content = function(file) {
          ggsave(file,
                 plot = analysis_moderators_test_plot(),
                 height = input$settings_options_ggplotHeight,
                 width = input$settings_options_ggplotWidth,
                 units = 'in',
                 device = 'png')
        }
      )
      
      # subgroup plots
      # TODO: this is a mess and prevents plot downloading;
      # TODO: there's an issue where the if the user makes the plot but then goes back
      # and adjusts the data (therefore removing the model), this plot fails to render even
      # though there is a validate* function
      observeEvent(input$analysis_moderator_fit, {
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        
        
        selected_moderator <- input$plotBart_moderator_vars
        #output$analysis_moderators_explore_plot <- renderPlot({
        # categorical plots
        if(input$plotBart_moderator_vars %in% gsub('X_', '', store$column_types$categorical)){
          if(input$categorical_exploratory_choice == 'Overlaid density'){
            output$analysis_moderators_test_plot <- renderPlot({
              div_id <- 'analysis_moderators_test_plot'
              show_message_updating(div_id)
              
              p <- plotBart::plot_moderator_d_density(
                .model = store$analysis$model$model,
                moderator = store$verified_df[[paste0('X_', selected_moderator)]]
              )
              
              p <- p + store$options$theme_custom  
              
              # remove overlay
              close_message_updating(div_id)
              
              return(p)
            })
          }
          if(input$categorical_exploratory_choice == 'Vertical intervals'){
            output$analysis_moderators_test_plot <- renderPlot({
              div_id <- 'analysis_moderators_test_plot'
              show_message_updating(div_id)
              
              p <- plotBart::plot_moderator_d_linerange(
                .model = store$analysis$model$model,
                moderator = store$verified_df[[paste0('X_', selected_moderator)]]
              )
              p <- p + store$options$theme_custom 
              
              # remove overlay
              close_message_updating(div_id)
              
              return(p)
            })
          }
          
        } # end of categorical block
        # continuous plots
        if(input$plotBart_moderator_vars %in% gsub('X_', '', store$column_types$continuous)){
          if(input$continuous_exploratory_choice == 'Loess'){
            output$analysis_moderators_test_plot <- renderPlot({
              div_id <- 'analysis_moderators_test_plot'
              show_message_updating(div_id)
              p <- plotBart::plot_moderator_c_loess(
                .model = store$analysis$model$model,
                moderator = store$verified_df[[paste0('X_', selected_moderator)]]
              )
              p <- p + store$options$theme_custom 
              
              # remove overlay
              close_message_updating(div_id)
              
              return(p)
            })
          }
          if(input$continuous_exploratory_choice == 'Partial dependency'){
            output$analysis_moderators_test_plot <- renderPlot({
              div_id <- 'analysis_moderators_test_plot'
              show_message_updating(div_id)
              p <- plotBart::plot_moderator_c_pd(
                .model = store$analysis$model$model,
                moderator = store$verified_df[[paste0('X_', selected_moderator)]]
              )
              p <- p + store$options$theme_custom 
              
              # remove overlay
              close_message_updating(div_id)
              
              return(p)
            })
          }
        }
        #})
      })
      
      return(store)
    }
  )
}