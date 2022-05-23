# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # initialize list to store variables
  store <- reactiveValues(
    uploaded_df = data.frame(), 
    log = list(as.character(Sys.time())),
    module_ids = module_ids,
    page_history = NULL,
    js = NULL
  )
  
  
  # javascript initiated actions --------------------------------------------

  # move page when JS says so
  # usually triggered by links in the help slideover
  observeEvent(input$js_open_page, {
    new_page <- input$js_open_page
    updateNavbarPage(session, inputId = "nav", selected = new_page)
  })
  
  # record if the user is on mobile or not
  # TODO: only works intermittently
  observeEvent(input$js_is_on_mobile, {
    store$js$is_on_mobile <- input$js_is_on_mobile
    # print(paste("is on mobile: ", store$js$is_on_mobile))
  })

  
  # back next buttons -------------------------------------------------------

  # observeEvent(input$analysis_model_button_popup, {
  #   updateNavbarPage(session, inputId = "nav", selected = "Data")
  #   updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load")
  #   shinyWidgets::closeSweetAlert()
  # })

  # subgroup/moderators page
  observeEvent(input$analysis_moderator_icate_button_back, {
    updateNavbarPage(session, inputId = "nav", selected = store$module_ids$analysis$results)
  })
  observeEvent(input$analysis_moderator_icate_button_next, {
    updateTabsetPanel(session, inputId = "analysis_moderator_tabs", selected = "Exploratory Subgroup Analyses")
  })
  observeEvent(input$analysis_moderator_analyses_button_back, {
    updateTabsetPanel(session, inputId = "analysis_moderator_tabs", selected = "ICATE")
  })
  observeEvent(input$analysis_moderator_analyses_button_results, {
    updateNavbarPage(session, inputId = "nav", selected = store$module_ids$analysis$results)
  })
  observeEvent(input$analysis_moderator_analyses_button_reproduce, {
    updateNavbarPage(session, inputId = "nav", selected = "Reproduce")
  })
  
  # store the page history
  # show/hide back button
  observeEvent(input$nav, {
    
    # store history
    store$page_history <- append(store$page_history, input$nav)
    
    # trigger icon if leaving analysis page
    if (!stringr::str_detect(input$nav, "^analysis") && 
        !identical(get_nav_previous_analysis_page(store), character(0))){
      shinyjs::show(selector = '.back-to-analysis-button')
    } else {
      shinyjs::hide(selector = '.back-to-analysis-button')
    }
  })
  
  # move to new page when button is clicked
  observeEvent(input$back_to_analysis_button, {
    new_page <- get_nav_previous_analysis_page(store)
    updateNavbarPage(session, inputId = "nav", selected = new_page)
  })


  # design text  ------------------------------------------------------------
  
  # launch pop up if first time
  isolate({store$analysis$design$launched_first_time_popup <- FALSE})
  observeEvent(input$nav, {
    if (input$nav == store$module_ids$analysis$design & 
        isFALSE(store$analysis$design$launched_first_time_popup)){
      store$analysis$design$launched_first_time_popup <- TRUE
      show_popup_welcome(session = session)
    }
  })
  

  # analysis modules --------------------------------------------------------

  store <- server_design(store = store, id = isolate(store$module_ids$analysis$design), global_session = session)
  store <- server_data(store = store, id = isolate(store$module_ids$analysis$data), global_session = session)
  store <- server_verify(store = store, id = isolate(store$module_ids$analysis$verify), global_session = session)
  store <- server_eda(store = store, id = isolate(store$module_ids$analysis$eda), global_session = session)
  store <- server_balance(store = store, id = isolate(store$module_ids$analysis$balance), global_session = session)
  store <- server_overlap(store = store, id = isolate(store$module_ids$analysis$overlap), global_session = session)
  store <- server_model(store = store, id = isolate(store$module_ids$analysis$model), global_session = session)
  store <- server_diagnostic(store = store, id = isolate(store$module_ids$analysis$diagnostic), global_session = session)
  store <- server_results(store = store, id = isolate(store$module_ids$analysis$results), global_session = session)
  

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


  # learning modules ----------------------------------------------------------------

  # add listeners that link the concepts title image to its article
  # this allows the actionLinks in concepts_page.R to work
  tab_titles <- c(
      "Randomization",
      'Fundamental problem',
      'Assumptions',
      'Regression methods',
      'Decision trees',
      'Post-treatment variables',
      'Causal estimands',
      'Bias and efficiency',
      'Potential outcomes'
    )
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
  # PotentialOutcomesServer(id = 'concepts_potentialoutcomes')
  server_learning_post_treatment(id = isolate(store$module_ids$learning$post_treatment),
                                 plot_theme = theme_custom)
  server_learning_bias_efficiency(id = isolate(store$module_ids$learning$bias_efficiency),
                                  plot_theme = theme_custom)
  server_learning_estimands(id = isolate(store$module_ids$learning$estimands),
                            plot_theme = theme_custom)
  server_learning_potential_outcomes(id = isolate(store$module_ids$learning$potential_outcomes), 
                                     plot_theme = theme_custom)
  #poServer(id = 'potential_outcomes_test')


  # welcome page ------------------------------------------------------------

  # add listeners that link the front page images to their respective pages
  observeEvent(input$welcome_link_concepts, {
    updateNavbarPage(session, inputId = "nav", selected = "All concepts")
  })
  observeEvent(input$welcome_link_Analysis, {
    updateNavbarPage(session, inputId = "nav", selected = store$module_ids$analysis$design)
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
  
  isolate(store$options$theme_custom <- theme_custom())

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
  
  # change plot download height and width
  isolate(store$options$settings_options_ggplotHeight <- input$settings_options_ggplotHeight)
  isolate(store$options$settings_options_ggplotWidth <- input$settings_options_ggplotWidth)


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
      if(!is.null(dim(store$analysis$eda$downloaded_descriptive_plot_parameters))){
        as.data.frame(store$analysis$eda$downloaded_descriptive_plot_parameters) %>% distinct()
      }else{
        NULL
      }
    
    # overlap
    overlap_plot <-
      if(!is.null(dim(store$analysis$eda$downloaded_overlap_plot_parameters))){
        as.data.frame(store$analysis$eda$downloaded_overlap_plot_parameters) %>% distinct()
      }else{
        NULL
      }
    
    # balance
    balance_plot <-
      if(!is.null(dim(store$analysis$eda$downloaded_balance_plot_parameters))){
        as.data.frame(store$analysis$eda$downloaded_balance_plot_parameters) %>% distinct()
      }else{
        NULL
      }
    
    # model
    common_support_rule <- store$analysis$model$analysis_over_ride_common_support
    if (store$analysis$model$analysis_model_support == 'No') common_support_rule <- 'none'
    
    BART_model <- 
      if(isTRUE(store$model_fit_good)){ # if a model successfully fitted
        
        if(!is.null(store$analysis$model$analysis_model_moderator_vars)){ # if moderators are specified
          data.frame(support = common_support_rule,
                     ran.eff = store$analysis$model$analysis_random_intercept,
                     estimand = base::tolower(store$analysis$model$analysis_model_estimand),
                     moderators = store$analysis$model$analysis_model_moderator_vars
          )
        }else{
          data.frame(support = common_support_rule,
                     ran.eff = store$analysis$model$analysis_random_intercept,
                     estimand = base::tolower(store$analysis$model$analysis_model_estimand),
                     moderators = NA
          )
        }
        
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
