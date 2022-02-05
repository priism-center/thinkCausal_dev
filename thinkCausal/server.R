# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # do this when app stops
  # onStop(fun = function() show_popup_crash())

  # initialize list to store variables
  store <- reactiveValues(
    uploaded_df = data.frame(), 
    log = list(as.character(Sys.time())),
    module_ids = module_ids
  )


  # back next buttons -------------------------------------------------------

  # plotting page
  observeEvent(input[['analysis_plots_descriptive_button_back']], {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Verify")
  })
  observeEvent(input[['analysis_plots_descriptive_button_next']], {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Common Support Plots")
  })
  observeEvent(input$analysis_plots_support_button_back, {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
  })
  observeEvent(input$analysis_plots_support_button_next, {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Balance Plots")
  })
  observeEvent(input$analysis_plots_balance_button_back, {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Common Support Plots")
  })
  observeEvent(input$analysis_plots_balance_button_next, {
    updateNavbarPage(session, inputId = "nav", selected = "Model")
  })

  # model page
  observeEvent(input$analysis_model_button_back, {
    updateNavbarPage(session, inputId = "nav", selected = "Exploratory plots")
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Balance Plots")
  })
  # observeEvent(input$analysis_model_button_popup, {
  #   updateNavbarPage(session, inputId = "nav", selected = "Data")
  #   updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load")
  #   shinyWidgets::closeSweetAlert()
  # })

  # diagnostics page
  observeEvent(input$analysis_diagnostics_button_back, {
    updateNavbarPage(session, inputId = "nav", selected = "Model")
  })
  observeEvent(input$analysis_diagnostics_button_next, {
    updateNavbarPage(session, inputId = "nav", selected = "Results")
  })

  # results page
  observeEvent(input$analysis_results_button_back, {
    updateNavbarPage(session, inputId = "nav", selected = "Model diagnostics")
  })
  observeEvent(input$analysis_results_button_subgroup, {
    updateNavbarPage(session, inputId = "nav", selected = "Subgroup results")
  })

  # subgroup/moderators page
  observeEvent(input$analysis_moderator_icate_button_back, {
    updateNavbarPage(session, inputId = "nav", selected = "Results")
  })
  observeEvent(input$analysis_moderator_icate_button_next, {
    updateTabsetPanel(session, inputId = "analysis_moderator_tabs", selected = "Exploratory Subgroup Analyses")
  })
  observeEvent(input$analysis_moderator_analyses_button_back, {
    updateTabsetPanel(session, inputId = "analysis_moderator_tabs", selected = "ICATE")
  })
  observeEvent(input$analysis_moderator_analyses_button_results, {
    updateNavbarPage(session, inputId = "nav", selected = "Results")
  })
  observeEvent(input$analysis_moderator_analyses_button_reproduce, {
    updateNavbarPage(session, inputId = "nav", selected = "Reproduce")
  })


  # design text  ------------------------------------------------------------
  
  # launch pop up if first time
  isolate({store$analysis$design$launched_first_time_popup <- FALSE})
  observeEvent(input$nav, {
    if (input$nav == 'Design' & isFALSE(store$analysis$design$launched_first_time_popup)){
      store$analysis$design$launched_first_time_popup <- TRUE
      show_popup_welcome(session = session)
    }
  })
  
  # run the design module server
  store <- server_design(store = store, id = isolate(store$module_ids$analysis$design), global_session = session)

  # upload data -------------------------------------------------------------

  store <- server_data(store = store, id = isolate(store$module_ids$analysis$data), global_session = session)

  # EDA ---------------------------------------------------------------------

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
    p <- p + theme_custom()

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
             height = input$settings_options_ggplotHeight,
             width = input$settings_options_ggplotWidth,
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
    p <- p + theme_custom()

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
             height = input$settings_options_ggplotHeight,
             width = input$settings_options_ggplotWidth,
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
    p <- p + theme_custom()

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
             height = input$settings_options_ggplotHeight,
             width = input$settings_options_ggplotWidth,
             units = 'in',
             device = 'png')
      # save the parameters of the downloaded overlap plot
      downloaded_balance_plot_parameters$df <- rbind(downloaded_balance_plot_parameters$df, balance_plot_parameters())
  })


  # model -------------------------------------------------------------------

  # when user runs the model, take a number of actions
  observeEvent(input$analysis_model_button_next, {

    # launch popup if data is not yet selected
    if (!is.data.frame(store$verified_df)) {
      show_popup_model_no_data_warning(session)
    }

    observeEvent(input$analysis_model_button_popup, {
      close_popup(session = session)
      updateNavbarPage(session, inputId = "nav", selected = "Data")
      updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Upload")
    })

    # spawn red text if selection isn't made
    if (isTRUE(is.null(input$analysis_model_radio_design))) {
      output$analysis_model_text_design_noinput <- renderUI({
        html_out <- tags$span(style = 'color: red;',
                              "Please make a selection",
                              br(), br())
        return(html_out)
      })
    }
    if (isTRUE(is.null(input$analysis_model_radio_estimand))) {
      output$analysis_model_text_estimand_noinput <- renderUI({
        html_out <- tags$span(style = 'color: red;',
                              "Please make a selection",
                              br(), br())
        return(html_out)
      })
    }
    if (isTRUE(is.null(input$analysis_model_radio_support))) {
      output$analysis_model_text_support_noinput <- renderUI({
        html_out <- tags$span(style = 'color: red;',
                              "Please make a selection",
                              br(), br())
        return(html_out)
      })
    }

    # stop here if data hasn't been uploaded and selected
    validate_data_verified(store)

    # stop here if inputs aren't found
    # TODO
    # req(input$)
    # print('Dataframe going into bartC: \n')
    # print(store$verified_df)
    # print('Column types of dataframe going into bartC: \n')
    # print(store$verified_df  %>% summarize_all(class))

    # remove current model if it exists
    store$model_results <- NULL
    store$model_fit_good <- NULL

    # insert popup to notify user of model fit process
    # TODO: estimate the time remaining empirically?
    # TODO: show console redirect
    show_popup_fitting_BART_waiting(session)


    # pull the response, treatment, and confounders variables out of the df
    # treatment_v <- store$verified_df[, 1]
    # response_v <- store$verified_df[, 2]
    # confounders_mat <- as.matrix(store$verified_df[, 3:ncol(store$verified_df)])
    # colnames(confounders_mat) <- str_sub(colnames(confounders_mat), start = 3)
    common_support_rule <- input$analysis_over_ride_common_support
    if (input$analysis_model_support == 'No') common_support_rule <- 'none'

    # run model
    # store$model_results <- withProgress(
    #   message = 'Fitting BART model',
    #   session = session,
    #   {
    #     fit_bart(
    #       .data = store$verified_df,
    #       support = common_support_rule,
    #       ran.eff = input$analysis_random_intercept,
    #       .estimand = base::tolower(input$analysis_model_estimand)
    #     )
    #   }
    # )
    store$model_results <- fit_bart(
      .data = store$verified_df,
      support = common_support_rule,
      ran.eff = input$analysis_random_intercept,
      .estimand = base::tolower(input$analysis_model_estimand)
    )

    # close the alert
    # shinyWidgets::closeSweetAlert()
    close_popup(session = session)

    # error handling
    # TODO: refine the popup; probably should pass the bart error to the popup somehow
    # TODO: is there a better way to detect if the model fit?
    did_model_fit <- !isTRUE(is.null(store$model_results))
    if (!did_model_fit){
      store$model_fit_good <- FALSE
      show_popup(session = session,
                 'Model did not fit',
                 close_button = shiny::modalButton("Close"))
    }
    req(did_model_fit)

    # store the results
    # TODO: need way to test if actually have a good fit
    store$model_fit_good <- TRUE

    # # update select on moderators page
    updateSelectInput(session = session,
                      inputId = 'analysis_moderator_vars',
                      choices = input$analysis_model_moderator_vars,
                      selected = input$analysis_model_moderator_vars[1])

    # add to log
    log_event <- paste0(
      'Ran BART model with following specification: \n',
      '\t', 'Experiment design: ', input$analysis_model_radio_design, '\n',
      '\t', 'Causal estimand: ', input$analysis_model_estimand, '\n',
      '\t', 'Common support rule: ', common_support_rule, '\n',
      '\t', 'Moderators: ', paste0(input$analysis_model_moderator_vars, collapse = "; "), '\n',
      '\t', 'Model outcome: ', input$analysis_model_outcome, '\n',
      '\t', 'Propensity score fit: ', input$analysis_model_pscore, '\n',
      '\t', 'Good model fit: ', store$model_fit_good
    )
    store$log <- append(store$log, log_event)

    # common support warning
    common_support_check <- check_common_support(store$model_results)

    # display popup if any observations would be removed
    any_points_removed <- common_support_check$proportion_removed_sd > 0 | common_support_check$proportion_removed_chi > 0
    if(any_points_removed & input$analysis_model_support == 'No'){
      show_popup_common_support_warning(session = session, common_support_check = common_support_check)
    }

    # nav buttons within the popup
    # TODO: the 'see common support diagnostics doesn't go anywhere
    observeEvent(input$common_support_new_rule, {
      updateNavbarPage(session, inputId = "nav", selected = "Model")
      close_popup(session = session)
    })
    observeEvent(input$common_support_continue, {
      updateNavbarPage(session, inputId = "nav", selected = "Results")
      close_popup(session = session)
    })

    # move to next page based on model fit
    no_points_removed <- common_support_check$proportion_removed_sd == 0 | common_support_check$proportion_removed_chi == 0
    if(no_points_removed & input$analysis_model_support == 'No'){
      updateNavbarPage(session, inputId = "nav", selected = "Results")
    }

    if( input$analysis_model_support != 'No'){
      updateNavbarPage(session, inputId = "nav", selected = "Results")
    }

  })


  # diagnostics -------------------------------------------------------------

  # render either both the back and next buttons or just the back if its a bad
  # model fit
  output$analysis_diagnosis_buttons_ui <- renderUI({
    if (isTRUE(store$model_fit_good)){
      tagList(
        div(
          class = 'backNextContainer',
          actionButton(inputId = "analysis_diagnostics_button_back",
                       label = "Back to specify model"),
          actionButton(inputId = "analysis_diagnostics_button_next",
                       label = "Model results")
        )
      )
    } else {
      # actionButton(inputId = "analysis_diagnostics_button_back",
      #              label = "Back to specify model")
      tagList(
        div(
          class = 'backNextContainer',
          actionButton(inputId = "analysis_diagnostics_button_back",
                       label = "Back to specify model"),
          actionButton(inputId = "analysis_diagnostics_button_next",
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
    p <- p + theme_custom()

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
      theme_custom() +
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
             height = input$settings_options_ggplotHeight,
             width = input$settings_options_ggplotWidth,
             units = 'in',
             device = 'png')
    }
  )


  # results -----------------------------------------------------------------

  # render the summary table
  output$analysis_results_table_summary <- DT::renderDataTable({

    # stop here if model isn't fit yet
    validate_model_fit(store)

    # extract estimates and format
    # TODO: unclear if credible interval is 80 or 95
    tab <- summary(store$model_results, ci.style = 'quant')$estimates %>%
      as.data.frame() %>%
      mutate(rownames = rownames(.)) %>%
      select(' ' = rownames, 1:4) %>%
      rename_all(tools::toTitleCase) %>%
      create_datatable(paging = FALSE, info = FALSE, selection = "none")

    return(tab)
  })

  # TODO: render the interpretation text
  output$results_text <- renderText({
    # stop here if model isn't fit yet
    validate_model_fit(store)

    text_out <- create_interpretation(.model = store$model_results,
                                      type = input$interpretation,
                                      treatment = store$analysis$design$treatment_name,
                                      units = store$analysis$design$treatment_units,
                                      participants = store$analysis$design$treatment_participants)

    return(text_out)
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
      .model = store$model_results,
      type = input$plot_result_style,
      ci_80 = sum(input$show_interval == 0.80) > 0,
      ci_95 = sum(input$show_interval == 0.95) > 0,
      .mean = sum(input$central_tendency == 'Mean') > 0,
      .median = sum(input$central_tendency == 'Median') > 0,
      reference = reference_bar
    )

    # add theme
    p <- p +
      theme_custom() +
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
             height = input$settings_options_ggplotHeight,
             width = input$settings_options_ggplotWidth,
             units = 'in',
             device = 'png')
    }
  )


  # moderators  -------------------------------------------------------------

  # render plot type options
  observeEvent(input$plotBart_moderator_vars, {
    if(input$plotBart_moderator_vars %in% gsub('X_', '', store$column_types$categorical)){
      output$sub_group_ui <- renderUI({
        
        selectInput('categorical_exploratory_choice',
                    label = 'Choose a plot type:',
                    choices = c('','Overlaid density', 'Vertical intervals'))
        
      })
        
        output$sub_group_pannel <- renderUI({
        radioButtons(inputId = 'panel', 
                     label = 'Panel plots:', 
                     choices = c('Yes', 'No'), 
                     selected = 'No', 
                     inline = T)
        
        })
      

    }

    if(input$plotBart_moderator_vars %in% gsub('X_', '', store$column_types$continuous)){
      output$sub_group_ui <- renderUI({
        selectInput('continuous_exploratory_choice',
                    label = 'Choose a plot type:',
                    choices = c('','Loess', 'Partial dependency'))})
    }

  })

  # ICATE plots
  analysis_moderators_icate_plot <- reactive({

    # stop here if model isn't fit yet
    validate_model_fit(store)

    if(input$icate_type == 'histogram'){
      if(input$plotBart_ICATE_color != 'None'){
        group <- store$verified_df[[paste0('X_', input$plotBart_ICATE_color)]]
      }
      else{
        group <- NULL
      }
      p <- plotBart::plot_ICATE(
        store$model_results,
        .group_by = group,
        n_bins = input$plotBart_ICATE_n_bins,
        .alpha = input$plotBart_ICATE_alpha
      )
      # add theme
      p <- p + theme_custom()
    }

    if(input$icate_type == 'ordered'){
      if(input$plotBart_waterfall_order != 'ICATE'){
        order <- store$verified_df[[paste0('X_', input$plotBart_waterfall_order)]]
      }
      else{
        order <- NULL
      }

      if(input$plotBart_waterfall_color!= 'None'){
        color.by <- store$verified_df[[paste0('X_', input$plotBart_waterfall_color)]]
      }
      else{
        color.by <- NULL
      }

      # plot it
      p <- plotBart::plot_waterfall(
        .model = store$model_results,
        .order = order,
        .color = color.by
      )

      # add theme
      p <- p + theme_custom()
    }

    if(input$icate_type == 'tree'){
      p <- plotBart::plot_moderator_search(store$model_results, max_depth = input$plotBart_tree_depth)
    }

    return(p)
  })
  output$analysis_moderators_icate_plot <- renderPlot(analysis_moderators_icate_plot())
  output$download_ICATE_plot <- downloadHandler(
    filename = function(){
      switch(
        input$icate_type,
        "tree" = "ICATE_tree_plot.png",
        "histogram" = "ICATE_histogram_plot.png",
        "ordered" = "ICATE_ordered_plot.png"
      )
    },
    content = function(file) {

      if (input$icate_type == 'tree'){
        # TODO: this doesn't work
        grDevices::png(
          filename = file,
          height = input$settings_options_ggplotHeight,
          width = input$settings_options_ggplotWidth,
          units = 'in',
          res = 300
        )
        analysis_moderators_icate_plot()
        dev.off()
      } else {
        ggsave(file,
               plot = analysis_moderators_icate_plot(),
               height = input$settings_options_ggplotHeight,
               width = input$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
      }
    }
  )

  # plot the moderators
  analysis_moderators_explore_plot <- reactive({

    # stop here if model isn't fit yet
    validate_model_fit(store)

    # stop here if no variables selected in dropdown
    validate(need(input$analysis_moderator_vars, "Please select a variable to group by"))

    # add overlay
    div_id <- 'analysis_moderators_explore_plot'
    show_message_updating(div_id)

    # make plot
    moderator_vars <- input$analysis_moderator_vars
    p <- plot_continuous_sub(.model = store$model_results,
                             grouped_on = moderator_vars)

    # add theme
    p <- p + theme_custom()

    # remove overlay
    close_message_updating(div_id)

    return(p)
  })
  output$analysis_moderators_explore_plot <- renderPlot(analysis_moderators_explore_plot())
  output$download_ESA_plot <- downloadHandler(
    filename = "moderators_plot.png",
    content = function(file) {
      ggsave(file,
             plot = analysis_moderators_explore_plot(),
             height = input$settings_options_ggplotHeight,
             width = input$settings_options_ggplotWidth,
             units = 'in',
             device = 'png')
    }
  )

  # subgroup plots
  # TODO: this is a mess and prevents plot downloading; Joe to rewrite
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
          output$analysis_moderators_explore_plot <- renderPlot({
            div_id <- 'analysis_moderators_explore_plot'
            show_message_updating(div_id)

            p <- plotBart::plot_moderator_d_density(
              .model = store$model_results,
              moderator = store$verified_df[[paste0('X_', selected_moderator)]]
            )
            
            p <- p + theme_custom() 

            # remove overlay
            close_message_updating(div_id)

            return(p)
          })
        }
        if(input$categorical_exploratory_choice == 'Vertical intervals'){
          output$analysis_moderators_explore_plot <- renderPlot({
          div_id <- 'analysis_moderators_explore_plot'
          show_message_updating(div_id)

          p <- plotBart::plot_moderator_d_linerange(
            .model = store$model_results,
            moderator = store$verified_df[[paste0('X_', selected_moderator)]]
          )
          p <- p + theme_custom()

          # remove overlay
          close_message_updating(div_id)

          return(p)
          })
        }

      } # end of categorical block
      # continuous plots
      if(input$plotBart_moderator_vars %in% gsub('X_', '', store$column_types$continuous)){
        if(input$continuous_exploratory_choice == 'Loess'){
          output$analysis_moderators_explore_plot <- renderPlot({
          div_id <- 'analysis_moderators_explore_plot'
          show_message_updating(div_id)
          p <- plotBart::plot_moderator_c_loess(
            .model = store$model_results,
            moderator = store$verified_df[[paste0('X_', selected_moderator)]]
          )
          p <- p + theme_custom()

          # remove overlay
          close_message_updating(div_id)

          return(p)
          })
        }
        if(input$continuous_exploratory_choice == 'Partial dependency'){
          output$analysis_moderators_explore_plot <- renderPlot({
          div_id <- 'analysis_moderators_explore_plot'
          show_message_updating(div_id)
          p <- plotBart::plot_moderator_c_pd(
            .model = store$model_results,
            moderator = store$verified_df[[paste0('X_', selected_moderator)]]
          )
          p <- p + theme_custom()

          # remove overlay
          close_message_updating(div_id)

          return(p)
          })
        }
      }
    #})
  })


  # concepts ----------------------------------------------------------------

  # add listeners that link the concepts title image to its article
  tab_titles <- c("Randomization", 'Fundamental problem', 'Assumptions', 'Regression methods', 'Decision trees', 'Post-treatment variables')
  lapply(tab_titles, function(page_to_go_to) {
    page_id <- paste0("concepts_link_", tolower(gsub('-| ', '_', page_to_go_to)))
    observeEvent(input[[page_id]], {
      shinyjs::runjs("window.scrollTo(0, 0)")
      updateNavbarPage(session, "nav", page_to_go_to)
    })
  })

  # run the modules
  server_learning_randomization(id = isolate(store$module_ids$learning$randomization), 
                                plot_theme = theme_custom)
  PotentialOutcomesServer(id = 'concepts_potentialoutcomes')
  server_learning_post_treatment(id = isolate(store$module_ids$learning$post_treatment),
                                 plot_theme = theme_custom)
  #poServer(id = 'potential_outcomes_test')


  # welcome page ------------------------------------------------------------

  # add listeners that link the front page images to their respective pages
  observeEvent(input$welcome_link_concepts, {
    updateNavbarPage(session, inputId = "nav", selected = "All concepts")
  })
  observeEvent(input$welcome_link_Analysis, {
    updateNavbarPage(session, inputId = "nav", selected = "Design")
  })


  # options -----------------------------------------------------------------

  # change plot theme, font size, and point size
  theme_custom <- reactive({

    # change theme
    theme_custom <- switch(
      input$settings_options_ggplotTheme,
      "Minimal" = theme_minimal_no_transparency,
      "Simple" = ggplot2::theme_bw,
      "Classic" = ggplot2::theme_classic,
      "Gray" = ggplot2::theme_gray
    )

    # change point and font size
    update_geom_defaults("point", list(size = input$settings_options_ggplotPointSize))
    theme_custom <- theme_custom(base_size = input$settings_options_ggplotTextSize)

    # change colors
    # theme_custom <- theme_custom %+replace% ggplot2::scale_color_brewer

    return(theme_custom)
  })

  # update plot theme preview
  output$settings_options_ggplot_preview <- renderPlot({

    # create dummy plot
    p <- ggplot(
      tibble(x = c(-19.0, 10.3, 8.4, 0.3, -1.8, 11.7, 9.6, 7.5, -13.0, 2.3),
             y = c(2.1, -7.5, 0.9, 2.8, -0.8, -1.2, 6.7, 8.1, 4.0, 18.9),
             shape = rep(LETTERS[1:5], 2)),
      aes(x = x, y = y, color = x, shape = shape)) +
      geom_point() +
      labs(title = "thinkCausal",
           color = 'color')

    # add theme
    p <- p + theme_custom()

    return(p)
  })


  # script ------------------------------------------------------------------

  # reproducible script
  # TODO: this hasn't been tested
  reproducible_script <- reactive({

    # these probably should be stored in realtime and then extracted here
    # this would prevent issues if user goes back and changes something but doesn't save it

    # file inputs
    uploaded_file_name <- store$analysis$data$upload$analysis_data_upload$name
    uploaded_file_type <-  tools::file_ext(uploaded_file_name)
    uploaded_file_header <- store$analysis$data$upload$analysis_data_header
    uploaded_file_delim <- store$analysis$data$upload$analysis_data_delim_value

    # get the selected columns and names
    selected_columns <- colnames(store$col_assignment_df)
    column_names <- colnames(store$user_modified_df)

    # data type changes
    change_data_type <- store$analysis$data$group$group_list
    
    # eda
    descriptive_plot <-
      if(!is.null(dim(downloaded_descriptive_plot_parameters$df))){
        as.data.frame(downloaded_descriptive_plot_parameters$df) %>% distinct()
      }else{
        NULL
      }
    
    # overlap
    overlap_plot <-
      if(!is.null(dim(downloaded_overlap_plot_parameters$df))){
        as.data.frame(downloaded_overlap_plot_parameters$df) %>% distinct()
      }else{
        NULL
      }
    
    # balance
    balance_plot <-
      if(!is.null(dim(downloaded_balance_plot_parameters$df))){
        as.data.frame(downloaded_balance_plot_parameters$df) %>% distinct()
      }else{
        NULL
      }
    
    # model
    common_support_rule <- input$analysis_over_ride_common_support
    if (input$analysis_model_support == 'No') common_support_rule <- 'none'
    
    BART_model <- 
      if(isTRUE(store$model_fit_good)){ # if a model successfully fitted
        data.frame(support = common_support_rule,
                   ran.eff = input$analysis_random_intercept,
                   estimand = base::tolower(input$analysis_model_estimand))
      }else{
        NULL
      }
    
    # create the script
    reproducible_script <- create_script(
      uploaded_file_name = uploaded_file_name,
      uploaded_file_type = uploaded_file_type,
      uploaded_file_header = uploaded_file_header,
      uploaded_file_delim = uploaded_file_delim,
      selected_columns = selected_columns,
      column_names = column_names,
      change_data_type = change_data_type,
      descriptive_plot = descriptive_plot,
      overlap_plot = overlap_plot,
      balance_plot = balance_plot,
      BART_model = BART_model
    )

    return(reproducible_script)
  })

  # download reproducible script
  output$analysis_results_button_download <- downloadHandler(
    filename <- function() {
      time <- gsub("-|:| ", "", Sys.time())
      paste0(time, '_thinkCausal_script.zip')
    },
    content <- function(filename){
      
      # prevent download if model is not yet fit
      validate_model_fit(store)
      
      # go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      # create README
      fileConn <- file("README.txt")
      writeLines(create_script_readme(), fileConn)
      close(fileConn)
      files <- c('README.txt', files)
      
      # write function code to individual files
      functions <- c(
        'clean_auto_convert_logicals', 
        'clean_dummies_to_categorical',
        'plot_exploration',
        'clean_detect_column_types',
        'clean_confounders_for_bart'
      )
      files <- write_function_files(files, functions)

      # create the script file
      fileConn <- file("thinkCausal_script.R")
      writeLines(reproducible_script(), fileConn)
      close(fileConn)
      files <- c('thinkCausal_script.R', files)

      # create the zip file
      zip(filename, files)
    }
  )


  # log ---------------------------------------------------------------------

  # print the log
  # the log is created by appending text descriptions of events to store$log
  output$settings_log_text <- renderText({
    log <- store$log
    if (length(log) == 0) log <- "No logged events to display"
    log <- paste0(log, collapse = '\n\n')
    return(log)
  })

  # download the log
  output$settings_log_download <- downloadHandler(
    filename <- function() {
      time <- gsub("-|:| ", "", Sys.time())
      paste0(time, '_thinkCausal_log.txt')
    },
    content <- function(filename){
      fileConn <- file(filename)
      log <- paste0(paste0(store$log, collapse = '\n\n'), "\n")
      writeLines(log, fileConn)
      close(fileConn)
    }
  )

  # # example for interactive table output
  # output$testytest <- renderText(get_table_values(input, 'mytable', ns = NS('yyp')))
  #
  # # example of new popup
  # observeEvent(input$test_popup, {
  #   show_popup_waiting(session = session)
  # })

})
