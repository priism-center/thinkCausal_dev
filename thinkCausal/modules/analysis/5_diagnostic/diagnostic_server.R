
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
        
        # plot it
        p <- plotBart::plot_common_support(
          .model = store$model_results,
          rule = 'both'
        )
        
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

