# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


  # next buttons ------------------------------------------------------------
  
  # data page
  observeEvent(input$analysis_data_load_button_next, {
    updateTabsetPanel(session, inputId ="analysis_data_tabs", selected ="Select Data")
  })
  observeEvent(input$analysis_data_select_button_next, {
    updateTabsetPanel(session, inputId ="analysis_data_tabs", selected = "Study Design")
  })
  observeEvent(input$analysis_data_design_button_next, {
    updateNavbarPage(session, inputId = "nav", selected = "Exploratory Plots")
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
  })
  
  # plotting page
  observeEvent(input$analysis_plots_descriptive_button_next, {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Common Support Plots")
  })
  observeEvent(input$analysis_plots_support_button_next, {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Balance Plots")
  })
  observeEvent(input$analysis_plots_balance_button_next, {
    updateNavbarPage(session, inputId = "nav", selected = "Model")
  })
  
  # model page
  
  # diagnostics page
  
  # 


  # upload data -------------------------------------------------------------

  # read in the CSV that was uploaded
  uploaded_df <- reactive({
    # TODO: modify so it can handle other datatypes
    # TODO: add parsing failures to log
    req(input$analysis_data_upload)
    tryCatch({
      upload_csv <- read_csv(
        file = input$analysis_data_upload$datapath,
        col_names = input$analysis_data_header)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs or if dataset isn't yet uploaded
      stop(safeError(e))
    })
    
    return(upload_csv)
  })  
  
  # table of selected dataset
  output$analysis_data_table <- DT::renderDataTable({
    custom_datatable(
      uploaded_df(),
      selection = "none"
    )
  })
  
  
  # EDA ---------------------------------------------------------------------

  output$analysis_plot_balance_plot <- renderPlot({
    
    # choose which variables to include
    if (isTRUE(input$all_balance)){
      selected_cols <- X_names
    } else {
      selected_cols <- input$balance_var
    }
    
    # plot it
    user_data %>% 
      # dplyr::select(-c('re78', 'u74', 'u75')) %>% 
      dplyr::select(all_of(c(selected_cols, 'z'))) %>% 
      pivot_longer(cols = -c('z')) %>% 
      group_by(name) %>% 
      mutate(value = scale(value)) %>% 
      group_by(name, z) %>% 
      summarize(mean = mean(value),
                .groups = 'drop') %>% 
      group_by(name) %>% 
      summarize(diff = mean - lag(mean),
                .groups = 'drop') %>% 
      na.omit() %>% 
      ggplot(aes(x=diff, y=name, color=abs(diff))) +
      geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray60') +
      geom_point(size=4) +
      scale_colour_gradient(low = 'gray30', high = 'red3') + #or should color be scaled to finite values?
      labs(title = 'Treatment and control balance',
           x = 'Scaled mean difference',
           y = NULL) +
      theme(legend.position = 'none')
  })
  
  # run the eda module
  edaServer(id = 'analysis_plots_descriptive', input_data = user_data)
  

  # concepts ----------------------------------------------------------------
  
  # add listeners that link the concepts title image to its article
  tab_titles <- c("Randomization", 'Fundamental problem', 'Assumptions', 'Regression methods')
  lapply(tab_titles, function(page_to_go_to) {
    page_id <- paste0("concepts_link_", page_to_go_to)
    observeEvent(input[[page_id]], {
      updateNavbarPage(session, "nav", page_to_go_to)
    })
  })
  
  # run the randomization module
  randomizationServer(id = 'concepts_randomization')

})
