
server_results <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # results page
      observeEvent(input$analysis_results_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$diagnostic)
      })
      observeEvent(input$analysis_results_button_next, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$subgroup)
      })
      
      # render the summary table
      output$analysis_results_table_summary <- DT::renderDataTable({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        # extract estimates and format
        # TODO: unclear if credible interval is 80 or 95
        tab <- summary(store$analysis$model$model, ci.style = 'quant')$estimates %>%
          as.data.frame() %>%
          mutate(rownames = rownames(.)) %>%
          dplyr::select(' ' = rownames, 1:4) %>%
          rename_all(tools::toTitleCase) %>%
          create_datatable(paging = FALSE, info = FALSE, selection = "none")
        
        return(tab)
      })
      
      # TODO: render the interpretation text
      output$results_text <- renderText({
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        text_out <- create_interpretation(.model = store$analysis$model$model,
                                          type = input$interpretation,
                                          treatment = store$analysis_design_treatment_name,
                                          units = store$analysis_design_treatment_units,
                                          participants = store$analysis_design_treatment_participants)

        return(text_out)
      })
      
      # update pre-specifed moderators 
      observeEvent(store$analysis$results$prespecified_subgroups, {
        browser()
        options <- store$analysis$results$prespecified_subgroups
        updateSelectInput(session = global_session, 
                          inputId = 'analysis_results_prespecifed', 
                          choices = options, 
                          selected = options[1])
      })
      
      
      
      # pre-specifed subgroups 
      analysis_pre_specified_moderators <- reactive({
        validate_model_fit(store)
        
        moderators <- store$analysis$model$prespecified_subgroups
        #TODO move to validate functions at some point 
        # add validation code
        
        
        
      })
      
      # PATE plot
      analysis_results_plot_PATE <- reactive({
        
        # stop here if model isn't fit yet
        validate_model_fit(store)
        
        # get value for reference bar
        reference_bar <- NULL
        if (input$show_reference == 'Yes') reference_bar <- req(input$reference_bar)
        
        # add overlay
        div_id <- 'analysis_results_plot_PATE'
        show_message_updating(div_id)
        
        # create plot
        p <- plotBart::plot_PATE(
          .model = store$analysis$model$model,
          type = input$plot_result_style,
          ci_80 = sum(input$show_interval == 0.80) > 0,
          ci_95 = sum(input$show_interval == 0.95) > 0,
          .mean = sum(input$central_tendency == 'Mean') > 0,
          .median = sum(input$central_tendency == 'Median') > 0,
          reference = reference_bar
        )
        
        # add theme
        p <- p +
          store$options$theme_custom +
          theme(legend.position = c(0.1, 0.9),
                legend.title = element_blank())
        
        # remove overlay
        close_message_updating(div_id)
        
        return(p)
      })
      
      
      output$analysis_results_plot_PATE <- renderPlot(analysis_results_plot_PATE())
      output$download_PATE_plot <- downloadHandler(
        filename = "PATE_plot.png",
        content = function(file) {
          ggsave(file,
                 plot = analysis_results_plot_PATE(),
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

