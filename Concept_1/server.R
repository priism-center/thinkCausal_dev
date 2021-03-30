# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # initialize list to store variables  
  store <- reactiveValues(uploaded_df = data.frame())
  
  # back next buttons -------------------------------------------------------
  
  # data page
  observeEvent(input$analysis_data_load_button_next, {
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Select Data")
  })
  observeEvent(input$analysis_data_select_button_back, {
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load Data")
  })
  observeEvent(input$analysis_data_select_button_next, {
    
    # ensure data has been selected first
    data_has_been_selected <- isTRUE(nrow(store$selected_df) > 0)
    if (isFALSE(data_has_been_selected)){
      shinyWidgets::show_alert(
        title = 'Please select and save columns assignments',
        text = "Must be saved prior to proceeding",
        type = 'error'
      )
    }
    validate(need(data_has_been_selected, "No dataframe uploaded"))
    
    updateNavbarPage(session, inputId = "nav", selected = "Exploratory Plots")
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
  })
  
  # plotting page
  observeEvent(input[['analysis_plots_descriptive_button_back']], {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Select Data")
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
    updateNavbarPage(session, inputId = "nav", selected = "Exploratory Plots")
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Balance Plots")
  })
  observeEvent(input$analysis_model_button_popup, {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load")
    shinyWidgets::closeSweetAlert()
  })
  
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
  
  
  # upload data -------------------------------------------------------------
  
  # read in the uploaded file
  uploaded_df <- reactive({
    # TODO: test all the file types (e.g. stata is only stata 15 or older)
    # TODO: this behavior is really unintuitive; need to rethink it
    # TODO: add parsing failures to log
    
    req(input$analysis_data_upload)
    
    # extract the filepath and the filetype
    filepath <- input$analysis_data_upload$datapath
    filetype <- tools::file_ext(filepath)
    # practice <- input$create_practice
    
    tryCatch({
      
      # if it's a txt file then ask the user what the delimiter is  
      if (filetype == 'txt'){
        output$analysis_data_delim <- renderUI({ 
          textInput(inputId = 'analysis_data_delim_value',
                    label = "Column delimiter",
                    # value = ',',
                    placeholder = ", | - :")
        })
        req(input$analysis_data_delim_value)
      }
      
      # upload the file based on its filetype
      if (filetype == "csv"){
        uploaded_file <- readr::read_csv(
          file = filepath,
          col_names = input$analysis_data_header) 
      } else if (filetype == 'dta'){
        uploaded_file <- readstata13::read.dta13(file = filepath)
      } else if (filetype == 'xlsx'){
        uploaded_file <- xlsx::read.xlsx(file = filepath)
      } else if (filetype == 'txt'){
        delim <- input$analysis_data_delim_value
        if (delim == "") delim <- ","
        uploaded_file <- readr::read_delim(
          file = filepath,
          delim = delim,
          col_names = input$analysis_data_header
        )
      } else if (filetype == 'spss'){
        uploaded_file <- Hmisc::spss.get(file = filepath)
      } else stop("File type is invalid")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs or if dataset isn't yet uploaded
      stop(safeError(e))
    })
    
    # clean column names; bad csvs will crash the server
    colnames(uploaded_file) <- clean_names(colnames(uploaded_file))

    return(uploaded_file)
  })
  
  
  # # Practice df
  # uploaded_df <-  reactive({
  #   req(input$create_practice)
  #   sim <- make_dat_demo()
  #   return(sim)
  # })
  
  # add dataframe to store object
  # TODO: does this need to be eager or can it be lazy via reactive()? 
  observeEvent(uploaded_df(), {
    
    # retrieve the raw uploaded data frame
    uploaded_df <- uploaded_df()

    # auto convert all of the logical columns
    auto_cleaned_df <- clean_auto_convert_logicals(uploaded_df) 
    
    store$uploaded_df <- auto_cleaned_df
  })
  
  
  # maintain a user modified dataframe that is continuously updated
  # TODO: does this need to be eager or can it be lazy via reactive()? 
  observe({
    
    # stop here if columns haven't been assigned
    validate(need(nrow(store$col_assignment_df) > 0,
                  "Columns must first be assigned. Please see 'Load data' tab."))
    
    # use assigned dataframe as the template
    user_modified_df <- store$col_assignment_df

    # get input indices and current input values
    indices <- seq_along(user_modified_df)
    current_values <- reactiveValuesToList(input)
    
    ## change column names
    user_entered_names <- as.character(current_values[paste0("analysis_data_", indices, '_rename')])
    user_entered_names <- clean_names(user_entered_names)
    names(user_modified_df) <- user_entered_names
    
    # TODO
    # change data type
    user_entered_dataTypes <- as.character(current_values[paste0("analysis_data_", indices, '_changeDataType')])
    print(user_entered_dataTypes)
    
    # new_data_types <- 
    # user_modified_df <- convert_data_type_to_complex(user_modified_df, user_entered_dataTypes)
    
    # TODO?
    # change categorical levels


    # save the data to the store
    store$user_modified_df <- user_modified_df 
  })
  
  # reset dataframe back to original when user clicks button
  observeEvent(input$analysis_data_button_reset, {
    
    # reset dataframe
    store$user_modified_df <- store$col_assignment_df
    
    ## reset UI
    # set indices to map over
    all_col_names <- colnames(store$col_assignment_df)
    default_data_types <- convert_data_type_to_simple(store$col_assignment_df)
    indices <- seq_along(all_col_names)
    
    # update the inputs
    lapply(indices, function(i){
      updateTextInput(
        session = session,
        inputId = paste0("analysis_data_", i, "_rename"),
        value = all_col_names[i]
      )
      updateSelectInput(
        session = session,
        inputId = paste0("analysis_data_", i, "_changeDataType"),
        selected = default_data_types[i]
      )
    })
  })
  
  # table of selected dataset
  output$analysis_data_table <- DT::renderDataTable({
    
    # stop here if columns haven't been assigned
    validate(need(nrow(store$col_assignment_df) > 0,
                  "Columns must first be assigned. Please see 'Load data' tab."))
    
    # create JS datatable
    tab <- create_datatable(
      store$user_modified_df,
      selection = "none"
    )
    
    return(tab)
  })
  
  # vector of selector ids
  # analysis_data_select_selector_ids <-
  #   c(
  #     "analysis_data_select_select_zcol",
  #     "analysis_data_select_select_ycol",
  #     "analysis_data_select_select_xcol"
  #   )
  
  # update select inputs when the input data changes
  # TODO: does this need to be eager or can it lazy via reactive()?
  # observeEvent(store$uploaded_df, {
  #   
  #   # stop here if data hasn't been uploaded
  #   validate(need(nrow(store$uploaded_df) > 0,
  #                 "Data must be first uploaded. Please see 'Data' tab."))
  #   
  #   # infer which columns are Z, Y, and X columns for smart defaults
  #   auto_columns <- clean_detect_ZYX_columns(colnames(store$uploaded_df))
  #   
  #   all_colnames <- colnames(store$uploaded_df)
  #   
  #   # fill the dropdown options with the colnames
  #   for (i in 1:3){
  #     updateSelectInput(session = session,
  #                       inputId = analysis_data_select_selector_ids[i],
  #                       choices = all_colnames,
  #                       selected = auto_columns[[i]]
  #     )
  #   }
  # })
  
  # render the drag and drop UI
  output$analysis_data_UI_dragdrop <- renderUI({
    
    # stop here if data hasn't been uploaded
    validate(need(nrow(store$uploaded_df) > 0,
                  "Data must be first uploaded"))
    
    # infer which columns are Z, Y, and X columns (i.e. smart defaults)
    auto_columns <- clean_detect_ZYX_columns(colnames(store$uploaded_df))
    
    # render the UI
    drag_drop_html <- tagList(
      bucket_list(
        header = "Drag the variables to their respective roles",
        group_name = "analysis_data_dragdrop",
        orientation = "horizontal",
        add_rank_list(
          input_id = "analysis_data_dragdrop_covariates",
          text = strong("Covariates"),
          labels = auto_columns$X,
          options = sortable_options(multiDrag = TRUE)
        ),
        add_rank_list(
          input_id = "analysis_data_dragdrop_treatment",
          text = strong("Treatment"),
          labels = auto_columns$Z,
          options = sortable_options(multiDrag = TRUE)
        ),
        add_rank_list(
          input_id = "analysis_data_dragdrop_response",
          text = strong("Response"),
          labels = auto_columns$Y,
          options = sortable_options(multiDrag = TRUE)
        ),
        add_rank_list(
          input_id = "analysis_data_dragdrop_delete",
          text = strong("Exclude these variables"),
          labels = NULL,
          options = sortable_options(multiDrag = TRUE)
        )
      )
    )
    
    return(drag_drop_html)
  })
  
  # plot the DAG
  # output$analysis_data_plot_DAG <- renderPlot({
  #   
  #   # stop here if data hasn't been uploaded
  #   validate(need(nrow(store$uploaded_df) > 0,
  #                 "Data must be first uploaded. Please see 'Data' tab."))
  #   
  #   # get user inputs
  #   cols_z <- input$analysis_data_select_select_zcol
  #   cols_y <- input$analysis_data_select_select_ycol
  #   cols_x <- input$analysis_data_select_select_xcol
  #   
  #   # plot it
  #   p <- plot_DAG(cols_z, cols_y, cols_x)
  #   
  #   return(p)
  # })
  
  # create new dataframe when user saves column assignments
  observeEvent(input$analysis_data_button_columnAssignSave, {
    
    # get user inputs
    cols_z <- input$analysis_data_dragdrop_treatment #input$analysis_data_select_select_zcol
    cols_y <- input$analysis_data_dragdrop_response #input$analysis_data_select_select_ycol
    cols_x <- input$analysis_data_dragdrop_covariates #input$analysis_data_select_select_xcol
    all_cols <- unlist(c(cols_z, cols_y, cols_x))

    # are there duplicate selections?
    all_unique <- isTRUE(length(all_cols) == length(unique(all_cols)))
    z_is_only_one <- length(cols_z) == 1
    y_is_only_one <- length(cols_y) == 1
    x_more_than_zero <- length(cols_x) > 0
    all_good <- isTRUE(all(c(all_unique, z_is_only_one, y_is_only_one, x_more_than_zero)))
    
    # launch error message
    if (!all_good){
      shinyWidgets::show_alert(
        title = "Whoops, there's an issue with variable assignment",
        text = "Did you miss an variable assignment? Or either treatment or response have more than one column or somehow there's duplicate columns. Please correct before saving.",
        type = 'error'
      )
    }
    validate(need(all_good, "There are duplicate column selections"))
    
    # store the new dataframe using the uploaded df as the template
    store$col_assignment_df <- store$uploaded_df[, all_cols]
    
    # save columns assignments
    store$column_assignments <- NULL
    store$column_assignments$z <- cols_z
    store$column_assignments$y <- cols_y
    store$column_assignments$x <- cols_x

    # launch success message
    # shinyWidgets::show_alert(
    #   title = 'Column assignments saved',
    #   type = 'success'
    # )
    
    # move to next page
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Select Data")
  })
  
  
  # select data -------------------------------------------------------------
  
  # render UI for modifying the data
  output$analysis_data_modify_UI <- renderUI({

    # stop here if columns haven't been assigned
    validate(need(nrow(store$col_assignment_df) > 0,
                  "Columns must first be assigned. Please see 'Load data' tab."))
      
    # get default data types
    default_data_types <- convert_data_type_to_simple(store$col_assignment_df)
    
    # set indices to map over
    all_col_names <- colnames(store$col_assignment_df)
    indices <- seq_along(all_col_names)
    
    # create vector of column type names
    column_types <- c('Treatment', 'Response', rep('Covariate', length(all_col_names)-2))
    
    # render the header to the table
    UI_header <- fluidRow(
      column(2, h5('Role')),
      column(3, h5('Column name')),
      column(2, h5('Data type')),
      column(3, h5('Levels')),
      column(2, h5('Percent NA'))
    )
    
    # render the rows
    UI_grid <- lapply(indices, FUN = function(i){
      fluidRow(
        column(width = 2,
               shinyjs::disabled(
                 textInput(
                   inputId = paste0('analysis_data_', i, '_type'),
                   label = NULL,
                   value = column_types[i])
               )
        ),
        column(width = 3, 
               textInput(
                 inputId = paste0("analysis_data_", i, "_rename"),
                 label = NULL,
                 value = all_col_names[i])
        ),
        column(width = 2, 
               selectInput(
                 inputId = paste0("analysis_data_", i, "_changeDataType"),
                 label = NULL, 
                 choices = c('Continuous', 'Categorical', 'Binary'),
                 selected = default_data_types[i])
        ),
        column(width = 3, 
               shinyjs::disabled(
                 textInput(
                   inputId = paste0("analysis_data_", i, "_levels"),
                   label = NULL,
                   placeholder = 'TBD')
               )
        ),
        column(width = 2, 
               shinyjs::disabled(
                 textInput(
                   inputId = paste0("analysis_data_", i, "_percentNA"),
                   label = NULL, 
                   placeholder = 'TBD')
               )
        )
      )
    })
    
    # combine the header and the rows
    UI_table <- tagList(UI_header, UI_grid)
    
    # # add listeners to each data type dropdown that notify when the value changes
    # lapply(indices, function(i){
    #   
    #   # get id of this dropdown
    #   data_type_input <- paste0("analysis_data_", i, "_changeDataType")
    #   
    #   # add the listener
    #   observeEvent(input[[data_type_input]], {
    #     
    #     # TODO: resume here; this initiall launches a bunch of unneccesary alerts
    # 
    #     input_value <- input[[data_type_input]]
    #     column_values <- store$col_assignment_df[, i]
    # 
    #     # did the data type change to a binary value and is it coercible to binary?
    #     is_binary <- input_value == 'Binary'
    #     if (isTRUE(is_binary)){
    #       
    #       # coerce to binary
    #       coerced_values <- readr::parse_logical(as.character(column_values))
    #       is_not_coercible <- any(is.na(coerced_values))
    # 
    #       # launch alert if it is binary and not coercible
    #       if (isTRUE(is_not_coercible)){
    #         shinyWidgets::show_alert(
    #           title = 'Please specify the levels of the input?',
    #           text = tags$div(
    #             actionButton(
    #               inputId = 'analysis_model_button_popup',
    #               label = 'Take me to the Data tab')
    #           ),
    #           type = 'error',
    #           btn_labels = NA
    #         )
    #       }
    #     }
    #   })
    # })

    return(UI_table)
  })
  
  # update levels and percentNAs fields with actual data
  observeEvent(store$user_modified_df, {
    
    # stop here if columns haven't been assigned
    validate(need(nrow(store$col_assignment_df) > 0,
                  "Columns must first be assigned. Please see 'Load data' tab."))
    
    # original data column indices
    indices <- seq_along(colnames(store$col_assignment_df))
    
    lapply(X = indices, function(i) {
      
      # update the levels
      col_levels <- unique(store$col_assignment_df[[i]])
      updateTextInput(
        session = session,
        inputId = paste0("analysis_data_", i, "_levels"),
        value = col_levels
      )
      
      # update the percent NA
      percent_NA <- mean(is.na(store$user_modified_df[[i]]))
      percent_NA <- paste0(round(percent_NA, 3) * 100, "%")
      updateTextInput(
        session = session,
        inputId = paste0("analysis_data_", i, "_percentNA"),
        value = percent_NA
      )
    })
  })
  

  # when user hits 'save column assignments', create a new dataframe from store$uploaded_df
  # with the new columns
  # TODO: fixed issue caused by column name changes
  observeEvent(input$analysis_data_save, {
    
    # new column names
    # TODO: update these with better labels
    old_col_names <- colnames(store$user_modified_df)
    new_col_names <- paste0(c('Z', 'Y', rep('X', length(old_col_names)-2)),
                            "_", old_col_names)

    # save original column names
    store$selected_df_original_names <- old_col_names
        
    # create new dataframe of just the selected vars and rename them
    store$selected_df <- store$user_modified_df
    colnames(store$selected_df) <- new_col_names

    # save the column names by their respective class
    # TODO: UNIT TEST THIS!!!
    classes_categorical <- c('logical', 'character', 'factor')
    classes_continuous <- c('numeric', 'double', 'integer')
    cols_by_class <- split(names(store$selected_df), sapply(store$selected_df, function(x) paste(class(x), collapse = " ")))
    store$selected_df_categorical_vars <- as.vector(unlist(cols_by_class[classes_categorical]))
    store$selected_df_numeric_vars <- as.vector(unlist(cols_by_class[classes_continuous]))
    
    # update selects on Descriptive plots page
    col_names <- colnames(store$selected_df)
    categorical_names <- store$selected_df_categorical_var
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_pairs_vars",
      choices = col_names,
      selected = col_names
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_x",
      choices = col_names,
      selected = col_names[1]
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_y",
      choices = col_names,
      selected = col_names[2]
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_fill",
      choices = col_names,
      selected = col_names[12]
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_size",
      choices = col_names,
      selected = col_names[4]
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_group",
      choices = categorical_names
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_facet",
      choices = c("None", categorical_names) #TODO: this isn't updating
    )
    
    # update selects on balance plots
    # TODO: exclude categorical vars here???
    cols <- store$selected_df_numeric_vars
    X_cols <- cols[stringr::str_starts(col_names, "X")]
    updateSelectInput(session = session,
                      inputId = 'analysis_plot_balance_select_var',
                      choices = X_cols,
                      selected = X_cols
    )
    updateSelectInput(session = session,
                      inputId = 'analysis_plot_overlap_select_var',
                      choices = X_cols,
                      selected = X_cols
    )
    
    # TODO: move user to next page?
    
  })
  
  
  # EDA ---------------------------------------------------------------------
  
  # create the descriptive plots
  # build the exploration plots
  output$analysis_eda_plot <- renderPlot({
    
    # stop here if data hasn't been uploaded and selected
    # TODO: error messages here are hard to read
    validate(need(is.data.frame(store$selected_df), 
                  "Data must be first uploaded and selected. Please see 'Data' tab."))
    
    p <- plot_exploration(
      .data = store$selected_df, #TODO: formalize sampling?
      .plot_type = input$analysis_eda_select_plot_type,
      .x = input$analysis_eda_variable_x,
      .y = input$analysis_eda_variable_y,
      .fill = input$analysis_eda_variable_fill,
      .fill_static = "#5c5980",
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
    
    return(p)
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
  output$analysis_eda_brush_info <- DT::renderDataTable(
    
    # show only if there isn't faceting
    if (input$analysis_eda_variable_facet == "None" & input$analysis_eda_select_plot_type == 'Scatter') {
      
      create_datatable(
        brushedPoints(store$selected_df, input$analysis_eda_plot_brush),
        selection = "none"
      )
    })
  
  # update second facet options so user cannot double facet on the same variable
  # b/c that causes an error
  observeEvent(input$analysis_eda_variable_facet, {
    if (input$analysis_eda_variable_facet != "None") {
      updateSelectInput(
        session = session,
        inputId = "analysis_eda_variable_facet_second",
        choices = setdiff(c("None", categorical_names), input$analysis_eda_variable_facet)
      )
    }
  })
  
  # create the overlap plot
  output$analysis_plot_overlap_plot <- renderPlot({
    
    # stop here if data hasn't been uploaded and selected
    validate(need(is.data.frame(store$selected_df), 
                  "Data must be first uploaded and selected. Please see 'Data' tab."))
    
    # plot either the variables or the 1 dimension propensity scores
    if(input$dim.red == 1){
      plot_overlap_vars(.data = store$selected_df, 
                        selected_cols = input$analysis_plot_overlap_select_var, 
                        plt_type = input$overlap.type)
    }
    
    else if(input$dim.red == 2){
      plot_overlap_pScores(.data = store$selected_df, 
                           plt_type = input$overlap.type)
    }
  })
  
  # create the balance plot
  output$analysis_plot_balance_plot <- renderPlot({
    
    # stop here if data hasn't been uploaded and selected
    validate(need(is.data.frame(store$selected_df), 
                  "Data must be first uploaded and selected. Please see 'Data' tab."))
    
    # stop here if there are no numeric columns selected
    validate(need(length(input$analysis_plot_balance_select_var) > 0,
                  "No numeric columns selected"))
    
    # plot it
    p <- plot_balance(.data = store$selected_df, selected_cols = input$analysis_plot_balance_select_var)
    
    return(p)
  })
  
  # run the eda module server. the UI is rendered server side within an observeEvent function
  # edaServer(id = 'analysis_plots_descriptive', input_data = store$selected_df) #user_data) #
  
  
  # diagnostics -------------------------------------------------------------
  
  # render either both the back and next buttons or just the back if its a bad
  # model fit
  output$analysis_diagnosis_buttons_ui <- renderUI({
    if (isTRUE(store$good_model_fit)){
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
      actionButton(inputId = "analysis_diagnostics_button_back",
                   label = "Back to specify model")
    }
  })
  
  # trace plot
  output$analysis_diagnostics_plot_trace <- renderPlot({
    
    # stop here if model is not run yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be fitted on the 'Model' tab"))
    # call function
    p <- plot_trace(.model = store$model_results)
    
    return(p)
  })
  
  # common support plot 
  output$analysis_diagnostics_plot_support <- renderPlot({
    
    # stop here if model is not run yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be fitted on the 'Model' tab"))
    
    # plot it
    p <- plot_diagnostic_common_support(.model = store$model_results, 
                                        .rule = input$analysis_model_radio_support)
    
    return(p)
  })
  
 
  
  # specify model -----------------------------------------------------------
  
                           
  # pop ups for estimand and common support help 
  
  observeEvent(input$analysis_model_radio_estimand, {
    if(input$analysis_model_radio_estimand == 'unsure'){
      shinyWidgets::sendSweetAlert(
        session,
        title = "I would like to lean more about casual estimands:",
        text = NULL,
        type = NULL,
        btn_labels = c("Yes", "No"),
        btn_colors = "#3085d6",
        html = TRUE,
        closeOnClickOutside = FALSE,
        showCloseButton = FALSE,
        width = NULL
      )
    }
  })
   
     observeEvent(input$analysis_model_radio_estimand, {
    if(input$analysis_model_radio_support == 'unsure'){
      shinyWidgets::sendSweetAlert(
        session,
        title = "I would like to lean more about common support:",
        text = NULL,
        type = NULL,
        btn_labels = c("Yes", "No"),
        btn_colors = "#3085d6",
        html = TRUE,
        closeOnClickOutside = FALSE,
        showCloseButton = FALSE,
        width = NULL
      )
    }
  })
                           
  # render text output to summarize the users inputs
  output$analysis_model_summary <- renderText({
    
    # extract inputs
    design <- input$analysis_model_radio_design
    estimand <- input$analysis_model_radio_estimand
    support <- input$analysis_model_radio_support
    if (is.null(design)) design <- "None"
    if (is.null(estimand)) estimand <- "None"
    if (is.null(support)) support <- "None"
    
    # grab text from object
    design_text <- analysis_model_text$design[[design]]
    estimand_text <- analysis_model_text$estimand[[estimand]]
    support_text <- analysis_model_text$support[[support]]
    
    # paste together all the text
    custom_text <- paste0(
      "<h3>Design</h3>",
      design_text,
      "<br><br>",
      "<h3>Estimand</h3>",
      estimand_text,
      "<br><br>",
      "<h3>Common Support</h3>",
      support_text
    )
    
    return(custom_text)
  })
  
  # render text below the radio buttons
  output$analysis_model_text_design <- renderUI({
    
    if (isTRUE(input$analysis_model_radio_design == 'quasi')){
      html_out <- tags$span(
        style = 'color: red;', 
        "Natural experiment design is not currently supported",
        br(), br()
      )
      return(html_out)
    }
    
    html_out <- ''
    return(html_out)
  })
  
  # remove no text UI spawns if user makes a selection
  observeEvent(input$analysis_model_radio_design, {
    removeUI('#analysis_model_text_design_noinput')
  })
  observeEvent(input$analysis_model_radio_estimand, {
    removeUI('#analysis_model_text_estimand_noinput')
  })
  observeEvent(input$analysis_model_radio_support, {
    removeUI('#analysis_model_text_support_noinput')
  })
  
  # when user runs the model, take a number of actions
  observeEvent(input$analysis_model_button_next, {
    
    # launch popup if data is not yet selected
    if (!is.data.frame(store$selected_df)) {
      shinyWidgets::show_alert(
        title = 'Data must be first uploaded and columns selected',
        text = tags$div(
          actionButton(
            inputId = 'analysis_model_button_popup',
            label = 'Take me to the Data tab')
        ),
        type = 'error',
        btn_labels = NA
      ) 
    }
    
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
    
    # stop here if inputs aren't found
    validate(
      need(
        is.data.frame(store$selected_df),
        "Data must be first uploaded and selected. Please see 'Data' tab."
      ),
      need(
        isFALSE(input$analysis_model_radio_design == 'quasi'),
        'Natural experiment design is not currently supported'
      ),
      need(
        isFALSE(is.null(input$analysis_model_radio_design)),
        'Please select an assignment mechanism'
      ),
      need(
        isFALSE(is.null(input$analysis_model_radio_estimand)),
        'Please select an estimand and common support rule'
      ),
      need(
        isFALSE(is.null(input$analysis_model_radio_support)),
        'Please select a common support rule'
      )
    )
    
    # insert popup to notify user of model fit process
    # TODO estimate the time remaining empirically?
    shinyWidgets::show_alert(
      title = 'Fitting BART model...',
      text = tags$div(
        img(src = file.path('img', 'tree.gif'),
            width = "50%")
      ),
      html = TRUE,
      btn_labels = NA,
      closeOnClickOutside = FALSE
    )
    
    # pull the response, treatment, and confounders variables out of the df
    treatment_v <- store$selected_df[, 1]
    response_v <- store$selected_df[, 2]
    confounders_mat <- as.matrix(store$selected_df[, 3:ncol(store$selected_df)])
    
    # TODO: dummy code categorical vars??
    
    # run model    
    store$model_results <- bartCause::bartc(
      response = response_v,
      treatment = treatment_v,
      confounders = confounders_mat,
      estimand = base::tolower(input$analysis_model_radio_estimand),
      commonSup.rule = input$analysis_model_radio_support
    )
    
    # store the results
    # TODO: need way to test if actually have a good fit
    store$good_model_fit <- TRUE
    
    # close the alert
    shinyWidgets::closeSweetAlert()
    
    # move to next page based on model fit
    if (isTRUE(store$good_model_fit)){
      updateNavbarPage(session, inputId = "nav", selected = "Results")
    } else {
      updateNavbarPage(session, inputId = "nav", selected = "Model diagnostics")
    }
  })
  
  
  # results -----------------------------------------------------------------
  
  # render the summary table
  output$analysis_results_table_summary <- renderText({
    
    # stop here if model isn't fit yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be fitted on the 'Model' tab"))
    
    # extract estimates and format
    summary(store$model_results)$estimates %>% 
      t() %>% 
      knitr::kable(digits = 3, format = 'html') %>% 
      kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
  })
  
  # TODO: render the interpretation text
  
  
  # ITE plot
  output$analysis_results_plot_ITE <- renderPlot({
    
    # stop here if model isn't fit yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be fitted on the 'Model' tab"))
    
    # plot it
    p <- plot_ITE(.model = store$model_results)
    
    return(p)
  })
  
  # conditional individual treatment effects plot
  output$analysis_results_plot_CITE <- renderPlot({
    
    # stop here if model isn't fit yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be fitted on the 'Model' tab"))
    
    # retrieve all the confounder columns
    X <- as.matrix(store$selected_df[, 3:ncol(store$selected_df)])
    
    # plot it
    # TODO see TODOs for plot_cate_test()
    # p <- plot_cate_test(.model = store$model_results, confounders = X)
    p <- NULL
    
    return(p)
  })
  
  
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
  
  
  # welcome page ------------------------------------------------------------
  
  # add listeners that link the front page images to their respective pages
  observeEvent(input$welcome_link_concepts, {
    updateNavbarPage(session, inputId = "nav", selected = "All concepts")
  })
  observeEvent(input$welcome_link_Analysis, {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load Data")
  })
  
  
})
