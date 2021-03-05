# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # initialize list to store variables  
  store <- reactiveValues(uploaded_df = data.frame())
  
  # back next buttons -------------------------------------------------------
  
  # data page
  observeEvent(input$analysis_data_load_button_next, {
    updateTabsetPanel(session, inputId ="analysis_data_tabs", selected ="Select Data")
  })
  observeEvent(input$analysis_data_select_button_back, {
    updateTabsetPanel(session, inputId ="analysis_data_tabs", selected = "Load")
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
  observeEvent(input[[NS('analysis_plots_descriptive')('analysis_plots_descriptive_button_back')]], {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Select Data")
  })
  observeEvent(input[[NS('analysis_plots_descriptive')('analysis_plots_descriptive_button_next')]], {
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
    
    
    # if(length(filepath != 0) & practice%%2 == 0){
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
    # }
    
    # else if(practice%%2 != 0){
    #   uploaded_file <- make_dat_demo()
    # }
    
    # else{
    #   uploaded_file <- ''
    # }
    
    return(uploaded_file)
  })
  
  
  # # Practice df
  # uploaded_df <-  reactive({
  #   req(input$create_practice)
  #   sim <- make_dat_demo()
  #   return(sim)
  # })
  
  
  #TODO: need to clean column names during upload; bad csvs will crash the server
  
  # add dataframe to store object
  observeEvent(uploaded_df(), {
    store$uploaded_df <- uploaded_df()
  })
  
  observeEvent(input$analysis_data_check_auto_convert, {
    # ensure data is uploaded
    validate(need(nrow(store$uploaded_df) > 0, "No dataframe uploaded"))
    
    # auto convert 0:1s to logicals and save back
    if (isTRUE(input$analysis_data_check_auto_convert)){
      store$uploaded_df <- clean_auto_convert_logicals(store$uploaded_df) 
    }
  })
  
  # render UI for modifying the data
  # TODO there's a CSS padding or margin issue that causes the boxes to be misaligned on some browsers (linux firefox)
  output$analysis_data_modify_UI <- renderUI({
    
    # get default data types
    default_data_types <- simple_data_types(store$uploaded_df)
    
    # render the selectize HTML
    html_tags <- tagList(
      tags$div(
        class = 'backNextContainer',
        tags$div(
          class = 'dataSelectizeLeft',
          tagList(
            h5("Column name"),
            lapply(X = seq_along(colnames(store$uploaded_df)), FUN = function(i) {
              textInput(
                inputId = paste0("analysis_data_rename_", i),
                label = NULL, #colnames(store$uploaded_df)[i],
                value = colnames(store$uploaded_df)[i]
              )
            })
          )
        ),
        tags$div(
          class = 'dataSelectizeRight',
          tagList(
            h5("Data type"),
            lapply(X = seq_along(colnames(store$uploaded_df)), FUN = function(i) {
              selectInput(
                inputId = paste0("analysis_data_changeDataType_", i),
                label = NULL, #colnames(store$uploaded_df)[i],
                choices = c('Categorical', 'Continuous', 'Logical'),
                selected = default_data_types[i]
              )
            })
          )
        )
      )
    )
    
    # add listeners that launch a popup if the user changes the data to categorical
    # TODO this kind of works but is a mess; need to rethink the UI
    lapply(X = seq_along(colnames(store$uploaded_df)), FUN = function(i) {
      dropdown_ID <- paste0("analysis_data_changeDataType_", i)
      observeEvent(input[[dropdown_ID]], {
        
        if (input[[dropdown_ID]] == 'Logical'){
          
          # get the data levels to the current column
          current_levels <- sort(unique(store$uploaded_df[[i]]))
          
          shinyWidgets::show_alert(
            title = 'Choose the levels to the logical',
            text = tags$div(
              tagList(
                selectInput(
                  inputId = paste0('analysis_data_changeDataType_popup_TRUE_', i),
                  label = 'TRUE',
                  choices = current_levels
                ),
                selectInput(
                  inputId = paste0('analysis_data_changeDataType_popup_FALSE_', i),
                  label = 'FALSE',
                  choices = current_levels
                )
              )
            ),
            html = TRUE,
            btn_labels = "Save",
            closeOnClickOutside = TRUE
          )
        }
      })
    })
    
    return(html_tags)
  })
  
  # overwrite column names when user saves new names
  observeEvent(input$analysis_data_button_modify_save, {
    validate(need(length(colnames(store$uploaded_df)) > 0, "No dataframe uploaded"))
    
    # rename the columns
    input_ids <- paste0("analysis_data_rename_", seq_along(colnames(store$uploaded_df)))
    inputted_name_values <- reactiveValuesToList(input)[input_ids]
    colnames(store$uploaded_df) <- inputted_name_values
    
    # change the data types
    # TODO
    
  })
  
  
  # # render UI for renaming the columns
  # # TODO change CSS to render label side by side
  # output$analysis_data_rename_UI <- renderUI({
  #   tagList(
  #     lapply(X = seq_along(colnames(store$uploaded_df)), FUN = function(i) {
  #       textInput(
  #         inputId = paste0("analysis_data_rename_", i),
  #         label = colnames(store$uploaded_df)[i],
  #         value = colnames(store$uploaded_df)[i]
  #       )
  #     })
  #   )
  # })
  
  # # overwrite column names when user saves new names
  # observeEvent(input$analysis_data_button_rename_save, {
  #   validate(need(length(colnames(store$uploaded_df)) > 0, "No dataframe uploaded"))
  #   input_ids <- paste0("analysis_data_rename_", seq_along(colnames(store$uploaded_df)))
  #   inputted_name_values <- reactiveValuesToList(input)[input_ids]
  #   colnames(store$uploaded_df) <- inputted_name_values
  # })
  
  # # render UI for changing the data types
  # output$analysis_data_changeDataTypes_UI <- renderUI({
  #   
  #   # get default data types
  #   default_data_types <- simple_data_types(store$uploaded_df)
  #   
  #   # render the dropdowns
  #   tagList(
  #     lapply(X = seq_along(colnames(store$uploaded_df)), FUN = function(i) {
  #       selectInput(
  #         inputId = paste0("analysis_data_changeDataType_", i),
  #         label = colnames(store$uploaded_df)[i],
  #         choices = c('Categorical', 'Continuous'),
  #         selected = default_data_types[i]
  #       )
  #     })
  #   )
  # })
  
  # TODO: overwrite data types names when user saves new data types
  # observeEvent(input$analysis_data_button_changeDataTypes_save, {
  #   validate(need(length(colnames(store$uploaded_df)) > 0, "No dataframe uploaded"))
  #   input_ids <- paste0("analysis_data_rename_", seq_along(colnames(store$uploaded_df)))
  #   inputted_name_values <- reactiveValuesToList(input)[input_ids]
  #   colnames(store$uploaded_df) <- inputted_name_values
  # })
  
  # table of selected dataset
  output$analysis_data_table <- DT::renderDataTable({
    create_datatable(
      store$uploaded_df,
      selection = "none"
    )
  })
  
  
  # select data -------------------------------------------------------------
  
  # vector of selector ids
  analysis_data_select_selector_ids <-
    c(
      "analysis_data_select_select_zcol",
      "analysis_data_select_select_ycol",
      "analysis_data_select_select_xcol"
    )
  
  # update select inputs when the input data changes
  observeEvent(store$uploaded_df, {
    
    # stop here if data hasn't been uploaded 
    validate(need(nrow(store$uploaded_df) > 0, 
                  "Data must be first uploaded. Please see 'Data' tab."))
    
    # infer which columns are Z, Y, and X columns for smart defaults
    auto_columns <- clean_detect_ZYX_columns(colnames(store$uploaded_df))
    
    # fill the dropdown options with the colnames
    for (i in 1:3){
      updateSelectInput(session = session, 
                        inputId = analysis_data_select_selector_ids[i],
                        choices = colnames(store$uploaded_df),
                        selected = auto_columns[[i]]
      )
    }
  })
  
  # when user hits 'save column assignments', create a new dataframe from store$uploaded_df
  # with the new columns
  observeEvent(input$analysis_data_select_column_save, {
    
    # get the current values of the select inputs
    all_selected_vars <- reactiveValuesToList(input)[analysis_data_select_selector_ids]
    has_values <- all(!sapply(all_selected_vars, is.null))
    
    # stop here if all dropdowns are somehow not selected
    validate(need(has_values, "Need all dropdowns to be selected"))
    
    # if all the values have been selected then update the new dataframe
    Z <- all_selected_vars[1]
    Y <- all_selected_vars[2]
    X <- unlist(all_selected_vars[3])
    
    # stop here if there are overlapping column assignments
    inputs_are_all_unique <- length(unique(unlist(all_selected_vars))) == length(unlist(all_selected_vars))
    if (isFALSE(inputs_are_all_unique)){
      shinyWidgets::show_alert(
        title = 'Duplicative column assignment',
        text = "At least one column has been selected for two different assignments. Please correct before saving.",
        type = 'error'
      )
    }
    validate(need(inputs_are_all_unique, "There are duplicative input columns"))
    
    # new column names
    # TODO: update these with better labels
    new_col_names <- paste0(c('Z', 'Y', paste0('X', 1:length(X))),
                            "_",
                            unlist(all_selected_vars))
    
    # create new dataframe of just the selected vars and rename them
    store$selected_df <- store$uploaded_df[, unlist(all_selected_vars)]
    colnames(store$selected_df) <- new_col_names
    
    # save original column names
    store$selected_df_original_names <- all_selected_vars
    
    # save the column names by their respective class
    # TODO: UNIT TEST THIS!!!
    classes_categorical <- c('logical', 'character', 'factor')
    classes_continuous <- c('numeric', 'double', 'integer')
    cols_by_class <- split(names(store$selected_df), sapply(store$selected_df, function(x) paste(class(x), collapse = " ")))
    store$selected_df_categorical_vars <- as.vector(unlist(cols_by_class[classes_categorical]))
    store$selected_df_numeric_vars <- as.vector(unlist(cols_by_class[classes_continuous]))
    
    # render the UI eda with this data
    output$analysis_plots_descriptive_eda_module <- renderUI({
      edaUI(id = "analysis_plots_descriptive", 
            col_names = colnames(store$selected_df), 
            categorical_names = store$selected_df_categorical_vars)
    })
    
    # run the eda module server
    # TODO: remove or integrate random sampling of data; including for now for dev speed
    edaServer(id = 'analysis_plots_descriptive', input_data = slice_sample(store$selected_df, n = 50))
    
    # update selects on balance plots
    # TODO: exclude categorical vars here???
    cols <- store$selected_df_numeric_vars
    X_cols <- cols[stringr::str_starts(cols, "X")]
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
  })
  
  # table of selected dataset
  output$analysis_data_select_table <- DT::renderDataTable({
    
    # render the table
    create_datatable(
      store$selected_df,
      selection = "none"
    )
  })
  
  
  # EDA ---------------------------------------------------------------------
  
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
      plot_overlap_pScores(.data = store$selected_df)
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
      # text = "Please wait",
      # type = 'info',
      # text = tags$div(
      #   class = 'spinner-grow',
      #   role = 'status',
      #   tags$span(class = 'sr-only', "Loading...")
      # ),
      text = tags$div(
        img(src = file.path('img', 'tree.gif'),
            width = "50%")
      ),
      html = TRUE,
      btn_labels = NA,
      closeOnClickOutside = FALSE
    )
    
    # pull the response, treatment, and confounders variables out of the df
    response_v <- store$selected_df[, 2]
    treatment_v <- store$selected_df[, 1]
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
  
  # render the interpretation text
  
  
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
