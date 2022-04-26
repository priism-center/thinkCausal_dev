
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
        if (isTRUE(store$model_fit_good)){
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
      
      # trace plot
      analysis_diagnostics_plot_trace <- reactive({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        # call function
        p <- plotBart::plot_trace(.model = store$model_results)
        
        # add theme
        p <- p + store$options$theme_custom
        
        return(p)
      })
      output$analysis_diagnostics_plot_trace <- renderPlot(analysis_diagnostics_plot_trace())
      
      # common support plot
      analysis_diagnostics_plot_support <- reactive({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        total_delete_sd <- sum(store$model_results$sd.cf > max(store$model_results$sd.obs) + sd(store$model_results$sd.obs))
        total_delete_chi <- sum((store$model_results$sd.cf / store$model_results$sd.obs) ** 2 > 3.841)
        
        if(total_delete_sd > 0 | total_delete_chi > 0){
          # common support plot from plotbart
          p1 <- plotBart::plot_common_support(
            .model = store$model_results,
            rule = 'both'
          )
          # save the cart results from rpart
          cart_sd <- plot_overlap_covariate_tree(.model = store$model_results, rule = "sd")
          cart_chi <- plot_overlap_covariate_tree(.model = store$model_results, rule = "chi")
          
          # clean data from the rpart result
          fitr_sd <- dendro_data(cart_sd)
          fitr_chi <- dendro_data(cart_chi)
          
          # plot tree based on sd rule
          p2 <- ggplot() +
            geom_segment(data = fitr_sd$segments, 
                         aes(x = x, y = y, xend = xend, yend = yend)) + 
            geom_rect(data = fitr_sd$labels,
              aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.03, ymax = y + 0.03),
              size = 0.5, fill = "#FFFFFFFF", color = "black") +
            geom_rect(data = fitr_sd$leaf_labels,
                      aes(xmin = x - 0.2, xmax = x + 0.2, ymin = y - 0.09, ymax = y - 0.02),
                      size = 0.5, fill = "#FFFFFFFF", color = "black") +
            geom_text(data = fitr_sd$labels, aes(x = x, y = y, label = label), size=3) +
            geom_text(data = fitr_sd$leaf_labels, aes(x = x, y = y - 0.05, label = label), size=3) +
            theme_dendro()
          
          # plot tree based on chi rule
          p3 <- ggplot() +
            geom_segment(data = fitr_chi$segments, 
                         aes(x = x, y = y, xend = xend, yend = yend)) + 
            geom_rect(data = fitr_chi$labels,
                      aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.03, ymax = y + 0.03),
                      size = 0.5, fill = "#FFFFFFFF", color = "black") +
            geom_rect(data = fitr_chi$leaf_labels,
                      aes(xmin = x - 0.2, xmax = x + 0.2, ymin = y - 0.09, ymax = y - 0.02),
                      size = 0.5, fill = "#FFFFFFFF", color = "black") +
            geom_text(data = fitr_chi$labels, aes(x = x, y = y, label = label), size=3) +
            geom_text(data = fitr_chi$leaf_labels, aes(x = x, y = y - 0.05, label = label), size=3) +
            theme_dendro()
          # patchwork pakage to combine the plots
          p <- p1 | (p2 / p3)
            
        }else{
          # plot it
          p <- plotBart::plot_common_support(
            .model = store$model_results,
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
      
      # download plot
      output$download_diagnostic_plot <- downloadHandler(
        filename = function() {
          switch(
            req(input$analysis_diagnostics_tabs),
            "Trace plot" = 'diagnostic_trace_plot.png',
            "Common support" = 'diagnostic_common_support_plot.png'
          )
        },
        content = function(file) {
          
          # get the plot that is on the active tab
          active_plot <- switch(
            req(input$analysis_diagnostics_tabs),
            'Trace plot' = analysis_diagnostics_plot_trace(),
            'Common support' = analysis_diagnostics_plot_support()
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

