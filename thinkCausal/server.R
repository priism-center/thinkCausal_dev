# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # initialize list to store variables  
  store <- reactiveValues(uploaded_df = data.frame(), log = list())
  
  # back next buttons -------------------------------------------------------
  
  # data page
  # observeEvent(input$analysis_data_load_button_next, {
  #   updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Select Data")
  # })
  observeEvent(input$analysis_data_select_button_back, {
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Pivot Data")
  })
  # observeEvent(input$analysis_data_select_button_next, {
  # 
  #   # ensure data has been selected first
  #   data_has_been_selected <- isTRUE(nrow(store$selected_df) > 0)
  #   if (isFALSE(data_has_been_selected)){
  #     shinyWidgets::show_alert(
  #       title = 'Please select and save columns assignments',
  #       text = "Must be saved prior to proceeding",
  #       type = 'error'
  #     )
  #   }
  #   validate(need(data_has_been_selected, "No dataframe uploaded"))
  #   
  #   updateNavbarPage(session, inputId = "nav", selected = "Exploratory Plots")
  #   updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
  # })
  
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
        uploaded_file <- xlsx::read.xlsx(file = filepath) #TODO: use xlsx or openxlsx?
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
    
    # add to log
    log_event <- paste0('Uploaded ', input$analysis_data_upload$name)
    store$log <- append(store$log, log_event)
    
    # retrieve the raw uploaded data frame
    uploaded_df <- uploaded_df()

    # auto convert all of the logical columns
    auto_cleaned_df <- clean_auto_convert_logicals(uploaded_df) 
    
    # add to store
    store$uploaded_df <- auto_cleaned_df
  })
  
  
  # maintain a user modified dataframe that is continuously updated
  # TODO: does this need to be eager or can it be lazy via reactive()? 
    # a few things would need to be modified, primarily the resetting of the df
  observe({
    
    # stop here if columns haven't been assigned
    validate_columns_assigned(store)
    
    # use assigned dataframe as the template
    user_modified_df <- store$categorical_df  #! input data changed to the result of pivot data
  
    if (isTRUE(nrow(store$user_modified_df) > 0)){
      
      # get input indices and current input values
      indices <- seq_along(user_modified_df)
      current_values <- reactiveValuesToList(input)
      
      ## change column names
      user_entered_names <- as.character(current_values[paste0("analysis_data_", indices, '_rename')])
      user_entered_names <- clean_names(user_entered_names)
      names(user_modified_df) <- user_entered_names
      
      # TODO: theres some issues here; may need to use promises: https://rstudio.github.io/promises/articles/motivation.html
      # change data type
      # current_dataTypes <- convert_data_type_to_simple(store$user_modified_df)
      # user_entered_dataTypes <- as.character(current_values[paste0("analysis_data_", indices, '_changeDataType')])
      # 
      # if (all(user_entered_dataTypes != 'NULL')) {
      #   pmap(list(colnames(store$user_modified_df), current_dataTypes, user_entered_dataTypes), 
      #        function(column_name, current_dataType, new_dataType){
      #          
      #          if (current_dataType != new_dataType){
      #            # c("Continuous", "Categorical", "Binary")
      #            
      #            # convert column to character
      #            current_col_as_char <- as.character(store$user_modified_df[[column_name]])
      #            
      #            if (new_dataType == "Binary"){
      #              auto_binary <- readr::parse_logical(as.character(current_col_as_char))
      #              if (any(is.na(auto_binary))){
      #                print("Issue auto converting column!!!")
      # 
      #                # launch popup so user can choose which values map to true/false
      #                shinyWidgets::show_alert(
      #                  title = "Please choose which value corresponds to true and false",
      #                  text = tags$div(
      #                    selectInput(
      #                      inputId = "analysis_data_select_TRUE",
      #                      label = "Value representing 'true'",
      #                      choices = unique(current_col_as_char)
      #                    ),
      #                    selectInput(
      #                      inputId = "analysis_data_select_FALSE",
      #                      label = "Value representing 'false'",
      #                      choices = unique(current_col_as_char)
      #                    )
      #                   ),
      #                  type = 'info',
      #                  btn_labels = c("Confirm"),
      #                  closeOnClickOutside = FALSE,
      #                  showCloseButton = FALSE
      #                )
      #              }
      #             }
      #            
      #            print(paste0("Changing ", column_name, " from ", current_dataType, " to ", new_dataType))
      #         }
      #   })
      # }

      
      # new_data_types <- 
      # user_modified_df <- convert_data_type_to_complex(user_modified_df, user_entered_dataTypes)
      
      # TODO?
      # change categorical levels
    }

    # save the data to the store
    store$user_modified_df <- user_modified_df 
  })
  
  # reset dataframe back to original when user clicks button
  observeEvent(input$analysis_data_button_reset, {
    
    # reset dataframe
    store$user_modified_df <- store$categorical_df #! input data changed to the result of pivot data
    
    ## reset UI
    # set indices to map over
    all_col_names <- colnames(store$categorical_df)  #! input data changed to the result of pivot data
    default_data_types <- convert_data_type_to_simple(store$categorical_df)  #! input data changed to the result of pivot data
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
  #   validate_data_uploaded(store)
  #   
  #   # infer which columns are Z, Y, and X columns for smart defaults
  #   auto_columns <- clean_detect_ZYX_columns(store$uploaded_df)
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
    validate_data_uploaded(store)
    
    # infer which columns are Z, Y, and X columns (i.e. smart defaults)
    auto_columns <- clean_detect_ZYX_columns(store$uploaded_df)

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
          labels = auto_columns$ID,
          options = sortable_options(multiDrag = TRUE),
          css_id = 'sortable-wide-upload'
        )
      )
    )
    
    return(drag_drop_html)
  })
  
  # plot the DAG
  # output$analysis_data_plot_DAG <- renderPlot({
  #   
  #   # stop here if data hasn't been uploaded
  #   validate_data_uploaded(store)
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
    cols_z <- input$analysis_data_dragdrop_treatment
    cols_y <- input$analysis_data_dragdrop_response 
    cols_x <- input$analysis_data_dragdrop_covariates
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
    
    # add to log
    log_event <- paste0('Assigned columns to roles: \n', 
                        '\ttreatment: ', cols_z, '\n',
                        '\tresponse: ', cols_y, '\n',
                        '\tcovariates: ', paste0(cols_x, collapse = '; '))
    store$log <- append(store$log, log_event)

    # launch success message
    # shinyWidgets::show_alert(
    #   title = 'Column assignments saved',
    #   type = 'success'
    # )
    
    # move to next page
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Pivot Data")
  })
  
  
  # pivot data -------------------------------------------------------------
  
  # initiate counter of number of groups to un-dummy
  store$n_dummy_groups <- 1
  
  output$analysis_data_UI_dragdrop_grouping <- renderUI({
    
    # stop here if data hasn't been uploaded and columns assigned
    validate_columns_assigned(store)
    
    # infer variables that are of logical other than the treatment 
    df <- store$col_assignment_df[, -c(1:2)]
    # cat_var_names <- colnames(df)[unlist(lapply(sapply(df, unique), function(x) length(x) == 2))]
    cat_var_names <- colnames(df)[sapply(df, clean_detect_logical)]
    
    # infer which columns are grouped (i.e. smart defaults)
    auto_groups <- clean_detect_dummy_cols_unique(df)
    store$n_dummy_groups <- max(store$n_dummy_groups, length(auto_groups))
    ungrouped_vars <- setdiff(cat_var_names, unlist(auto_groups))
    
    # show message to user indicating if we did or did not detect any variables
    # TODO: show_message('We detected some variables that may be related....')

    ## TODO: if detect the two values in a dummy are not of T/F or 0/1, then ask user to specify which is T and which is F
    ## TODO: 'add group' will overwrite the names
    
    # if there are more than one logical variables, show the grouping interface
    if(length(cat_var_names) > 1){
    
      drag_drop_html_grouping <- 
        tagList(
          bucket_list(
            header = "Drag the variables to their respective groups",
            group_name = "analysis_data_dragdrop_grouping",
            orientation = "horizontal",
            
            add_rank_list(
              input_id = "analysis_data_dragdrop_grouping_variables",
              text = strong("Ungrouped variables"),
              labels = ungrouped_vars,
              options = sortable_options(multiDrag = TRUE),
              css_id = 'sortable-wide-group'
            )
          ),
          # allow user to add groups
          lapply(c(1:store$n_dummy_groups), function(i){
              bucket_list(
                header = " ",
                group_name = "analysis_data_dragdrop_grouping",
                orientation = "horizontal",
                
                add_rank_list(
                  input_id = paste0('analysis_data_categorical_group_', i),
                  text = textInput(inputId = paste0("rename_group_", i), 
                                   label = NULL, 
                                   value = paste0("Group ", i)), # group names are editable
                  labels = tryCatch(auto_groups[[i]], error = function(e) NULL), #NULL,
                  options = sortable_options(multiDrag = TRUE)
                )
              )
          })
        )
      
    } else{ # if there is zero or one logical variable, show the prompt to go to the next page
      drag_drop_html_grouping <- tagList(
        p("Your dataset has <= 1 logical variable. No need to group variables. Click 'Save groupings' to the next page." )
      )
    }
    return(drag_drop_html_grouping)
  })
  
  
  # the number of group increases one when observe 'add a group' clicked
  observeEvent(input$analysis_data_add_group, {
    store$n_dummy_groups <- store$n_dummy_groups + 1
  })
  
  # create new dataframe when user saves variable grouping
  observeEvent(input$analysis_data_save_groupings, {
    
    store$categorical_df <- store$col_assignment_df
    
    for (i in 1:store$n_dummy_groups) {
      
      # find the column indexes of dummy variables in the same group 
      input_id <- paste0("analysis_data_categorical_group_", i)
      idx <- which(colnames(store$categorical_df) %in% input[[input_id]])
     
      # if there are more than one dummies in a group, convert the dummies to a categorical variable
      if(length(idx) > 1){
        tmp <- store$categorical_df[,idx]
        
        # if the sum of all the categories in a row is zero, then the reference group is missing for the categorical variable, filling with 'REFERENCE'
        # otherwise, filling with the column name of the binary variable whose values is TRUE 
        categorical <- apply(tmp, 1, function(x) ifelse(sum(x, na.rm = T) == 0, 'REFERENCE', colnames(tmp)[which(x == TRUE)]))
        
        # remove the multiple dummies
        store$categorical_df <- store$categorical_df[,-idx]
        
        # add the new categorical variable into the dataset
        store$categorical_df <- cbind(store$categorical_df, categorical)
        
        # clean the user input name
        name <- clean_names(input[[paste0("rename_group_", i)]])
        colnames(store$categorical_df)[ncol(store$categorical_df)] <- name
      }
      
    }
    
    # add to log
    log_event <- 'Assigned dummy coded variables to groups: \n'
    for (i in 1:store$n_dummy_groups){
      input_id <- paste0("analysis_data_categorical_group_", i)
      log_event <- paste0(log_event, '\tgroup', i, ': ', paste0(input[[input_id]], collapse = '; '), '\n')
    }
    print(log_event)
    store$log <- append(store$log, log_event)
    
    # move to next page
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Select Data")
  })
  
  observeEvent(input$analysis_data_pivot_button_back, {
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load Data")
  })
  
  
  # select data -------------------------------------------------------------
  
  # render UI for modifying the data
  output$analysis_data_modify_UI <- renderUI({

    # stop here if columns haven't been assigned and grouped
    validate_columns_assigned(store)
    validate_data_grouped(store)
    
    # get default data types
    default_data_types <- convert_data_type_to_simple(store$categorical_df) #! input data changed to the result of pivot data  
    
    # set indices to map over
    all_col_names <- colnames(store$categorical_df) #! input data changed to the result of pivot data
    indices <- seq_along(all_col_names)
    
    # create vector of column type names
    column_types <- c('Treatment', 'Response', rep('Covariate', length(all_col_names)-2))
    
    # render the header to the table
    UI_header <- fluidRow(
      column(2, h5('Role')),
      column(3, h5('Rename variable')),
      column(3, h5('Verify variable type')),
      column(2, h5('Levels')),
      column(2, h5('Percent NA'))
    )
    
    # render the rows
    UI_grid <- lapply(indices, function(i){
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
        column(width = 3, 
               selectInput(
                 inputId = paste0("analysis_data_", i, "_changeDataType"),
                 label = NULL, 
                 choices = c('Continuous', 'Categorical', 'Binary'),
                 selected = default_data_types[i])
        ),
        column(width = 2, 
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
    
    # add default column types to store
    store$current_simple_column_types <- default_data_types
    
    # # add observers to launch modal if user changes data type to binary
    # lapply(indices, function(i){
    #   input_id <- paste0("analysis_data_", i, "_changeDataType")
    #   observeEvent(input[[input_id]], {
    # 
    #     previous_value <- store$current_simple_column_types[i]
    #     new_value <- input[[input_id]]
    # 
    #     # update column types in store
    #     store$current_simple_column_types[i] <- new_value
    # 
    #     if (new_value == 'Binary'){
    #       shinyWidgets::show_alert(
    #         title = 'Please specify the levels',
    #         text = tags$div(
    #           
    #           actionButton(
    #             inputId = paste0('analysis_data_', i, '_button_confirmLevel'),
    #             label = 'Confirm')
    #           ),
    #         type = 'info',
    #         btn_labels = NA,
    #         closeOnClickOutside = FALSE
    #       )
    #     }
    # 
    #     print(store$current_simple_column_types)
    #   })
    # })
    # 
    # # add observers to record the input within the launched modals
    # lapply(indices, function(i){
    #   
    # })

    return(UI_table)
  })
  
  # update levels and percentNAs fields with actual data
  # TODO: this fails to update if user goes back and reassigns the dataset; if the user then clicks on 
    # on rename or data type then it updates
  observe_multiple <- reactive(list(store$user_modified_df, input$analysis_data_button_columnAssignSave))
  observeEvent(observe_multiple(), {

    # stop here if columns haven't been assigned and grouped
    validate_columns_assigned(store)
    validate_data_grouped(store)

    # original data column indices
    indices <- seq_along(colnames(store$categorical_df)) #! input data changed to the result of pivot data

    lapply(X = indices, function(i) {

      # update the levels
      col_levels <- unique(store$categorical_df[[i]]) #! input data changed to the result of pivot data
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
  
  # table of selected dataset
  output$analysis_data_table <- DT::renderDataTable({
    
    # stop here if columns haven't been assigned and grouped
    validate_columns_assigned(store)
    validate_data_grouped(store)
    
    # create JS datatable
    tab <- create_datatable(
      store$user_modified_df,
      selection = "none"
    )
    
    return(tab)
  })
  
  # # add listeners to each data type dropdown that notify when the value changes
  # observeEvent(nrow(store$col_assignment_df), {
  # 
  #   req(nrow(store$col_assignment_df) > 0)
  # 
  #   # set indices to map over
  #   indices <- seq_along(store$col_assignment_df)
  # 
  #   # require data types to render first
  #   data_type_values <- reactiveValuesToList(input)[paste0("analysis_data_", indices, "_changeDataType")]
  #   req(all(data_type_values %in% c('Continuous', 'Categorical', 'Binary')))
  # 
  #   # add the listeners
  #   lapply(indices, function(i){
  # 
  #     # get id of this dropdown
  #     data_type_input <- paste0("analysis_data_", i, "_changeDataType")
  # 
  #     # add the listener
  #     observeEvent(input[[data_type_input]], {
  # 
  #       # TODO: resume here; this initially launches a bunch of unnecessary alerts
  # 
  #       # get the current values
  #       input_value <- input[[data_type_input]]
  #       column_values <- store$col_assignment_df[, i]
  # 
  #       # did the data type change to a binary value and is it coercible to binary?
  #       is_binary <- input_value == 'Binary'
  #       if (isTRUE(is_binary)){
  # 
  #         # coerce to binary
  #         coerced_values <- readr::parse_logical(as.character(column_values))
  #         is_not_coercible <- any(is.na(coerced_values))
  # 
  #         # launch alert if it is binary and not coercible
  #         if (isTRUE(is_not_coercible)){
  #           shinyWidgets::show_alert(
  #             title = 'Please specify the levels of the input?',
  #             text = tags$div(
  #               # actionButton(
  #               #   inputId = 'analysis_model_button_popup',
  #               #   label = 'Take me to the Data tab')
  #             ),
  #             type = 'error',
  #             btn_labels = NA
  #           )
  #         }
  #       }
  #     })
  #   })
  # })

  # when user hits 'save column assignments', create a new dataframe from store$uploaded_df
  # with the new columns
  observeEvent(input$analysis_data_save, {
    
    # new column names
    old_col_names <- colnames(store$user_modified_df)
    new_col_names <- paste0(c('Z', 
                              'Y', 
                              rep('X', length(old_col_names)-2)),
                            "_", 
                            old_col_names)

    # save original column names
    store$selected_df_original_names <- old_col_names
        
    # create new dataframe of just the selected vars and rename them
    store$selected_df <- store$user_modified_df
    colnames(store$selected_df) <- new_col_names

    # save the column names by their respective class
    store$column_types <- clean_detect_column_types(store$selected_df)
    
    # add to log
    column_types <- convert_data_type_to_simple(store$selected_df)
    log_event <- paste0(
      'Saved columns with following specification: \n',
      paste0(paste0("\t", new_col_names), 
             ": ", 
             column_types,
             collapse = "\n")
      )
    store$log <- append(store$log, log_event)
    
    # update selects on Descriptive plots page
    col_names <- colnames(store$selected_df)
    cols_categorical <- store$column_types$categorical
    cols_continuous <- store$column_types$continuous
    
    # get smart defaults for the plotting variables
    column_treatment <- grep("^Z_", new_col_names, value = TRUE)
    column_response <- grep("^Y_", new_col_names, value = TRUE)
    plot_vars <- clean_detect_plot_vars(.column_types = store$column_types, 
                                        .treatment_column = column_treatment, 
                                        .response_column = column_response)
    
    # updateSelectInput(
    #   session = session,
    #   inputId = "analysis_eda_variable_pairs_vars",
    #   choices = new_col_names,
    #   selected = new_col_names
    # )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_select_plot_type",
      selected = plot_vars$plot_type
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_x",
      choices = new_col_names,
      selected = plot_vars$X
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_y",
      choices = new_col_names,
      selected = plot_vars$Y
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_fill",
      choices = c("None", new_col_names),
      selected = plot_vars$fill
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_shape",
      choices = c("None", cols_categorical),
      selected = plot_vars$shape
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_size",
      choices = c("None", new_col_names),
      selected = plot_vars$size
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_group",
      choices = c("None", cols_categorical),
      selected = plot_vars$grouping
    )
    updateSelectInput(
      session = session,
      inputId = "analysis_eda_variable_facet",
      choices = c("None", cols_categorical)
    )
    
    # update selects on balance plots
    X_cols <- grep("^X_", new_col_names, value = TRUE)
    X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
    
    # create moderator options 
    X_mods <- combn(X_cols, m = 2) %>% t() %>% as.data.frame()
    remove <- X_mods[X_mods$V1 %in% X_cols_continuous & X_mods$V2 %in% X_cols_continuous,]
    X_mods <- anti_join(X_mods, remove)
    X_mods <- mutate(X_mods, 
                     V1 = str_sub(V1, start = 3), 
                     V2 = str_sub(V2, start = 3))
    X_mods <- X_mods %>% 
      mutate(mod = paste(V1, V2, sep = ' x ')) %>% 
      pull(mod)
    X_mods <- c(str_sub(X_cols, start = 3), X_mods)
    
    
    # update options for balance 
    updateSelectInput(session = session,
                      inputId = 'analysis_plot_balance_select_var',
                      choices = X_cols_continuous,
                      selected = X_cols_continuous
    )
    updateSelectInput(session = session,
                      inputId = 'analysis_plot_overlap_select_var',
                      choices = X_cols_continuous,
                      selected = X_cols_continuous
    )
    
    #update moderator select on model page and moderator test page
    updateSelectInput(session = session,
                      inputId = 'analysis_model_moderator_vars',
                      choices = X_mods)

    updateSelectInput(session = session,
                      inputId = 'analysis_moderators_explore_select',
                      choices = X_mods)
    
    updateSelectInput(session = session,
                      inputId = 'analysis_moderators_explore_only',
                      choices = X_mods)
    

    # move to next page
    updateNavbarPage(session, inputId = "nav", selected = "Exploratory plots")
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
  })
  
  
  # EDA ---------------------------------------------------------------------
  
  # only show continuous variables if histogram, boxplot, or boxplot is selected
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
        choices = colnames(store$selected_df),
        selected = selection_current
      ) 
    }
  })
  
  # create the descriptive plots
  # build the exploration plots
  descriptive_plot <- reactive( {
    
    # stop here if data hasn't been uploaded and selected
    validate_data_selected(store)
    
    p <- tryCatch({
      plot_exploration(
        .data = store$selected_df, #TODO: formalize sampling?
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
    
    # TODO: this doesn't catch the error codes
    # validate(need(is.ggplot(p),
    #               "Variable selection is not valid. Please try another combination."))
    
    # add theme
    p <- p + theme_custom()
    
    return(p)
  })
  
  output$analysis_eda_plot <- renderPlot({
    descriptive_plot()
  })
  
  output$download_descriptive_plot <- downloadHandler(
    filename = 'descriptive_plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = descriptive_plot(), device = device)
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
    validate_data_selected(store)
    
    # show only if there isn't faceting
    if (input$analysis_eda_variable_facet == "None" & input$analysis_eda_select_plot_type == 'Scatter') {
      
      create_datatable(
        brushedPoints(store$selected_df, input$analysis_eda_plot_brush),
        selection = "none"
      )
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
  
  # create the overlap plot
  overlap_plot <- reactive({
    
    # stop here if data hasn't been uploaded and selected
    validate_data_selected(store)
    
    # stop here if there are no numeric columns selected
    validate(need(length(input$analysis_plot_overlap_select_var) > 0,
                  "No continuous columns available or currently selected"))
    
    # show message if there are many variables
    if (length(input$analysis_plot_overlap_select_var) > 8) show_message('Building the plot. May take a while.')
    
    # get variables for input into plotting functions
    X <- store$selected_df
    col_names <- colnames(X)
    treatment_col <- grep("^Z_", col_names, value = TRUE)
    response_col <- grep("^Y_", col_names, value = TRUE) 
    cols_continuous <- store$column_types$continuous
    confounder_cols <- grep("^X_", cols_continuous, value = TRUE) 
    plt_type <- input$analysis_plot_overlap_method
    
    # plot either the variables or the 1 dimension propensity scores
    if(input$analysis_plot_overlap_type == 1){
      p <- plotBart::plot_overlap_vars(
        .data = X,
        treatment = treatment_col,
        confounders = input$analysis_plot_overlap_select_var, 
        plot_type = plt_type
      )
    }
    
    else if(input$analysis_plot_overlap_type == 2){
      p <- plotBart::plot_overlap_pScores(
        .data = X,
        treatment = treatment_col,
        response = response_col,
        confounders = confounder_cols, 
        plot_type = plt_type
      )
    }
    
    # add theme
    p <- p + theme_custom()
    
    return(p)
  })
  
  output$analysis_plot_overlap_plot <- renderPlot({
    overlap_plot()
  })
  
  output$download_overlap_plot <- downloadHandler(
    filename = 'overlap_plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = overlap_plot(), device = device)
    })
  
  # create the balance plot
  balance_plot <- reactive({
    # stop here if data hasn't been uploaded and selected
    validate_data_selected(store)
    
    # stop here if there are no numeric columns selected
    validate(need(length(input$analysis_plot_balance_select_var) > 0,
                  "No continuous columns available or currently selected"))
    
    # show message if there are many variables
    if (length(input$analysis_plot_balance_select_var) > 8) show_message('Building the plot. May take a while.')
    
    # plot it
    X <- store$selected_df
    col_names <- colnames(X)
    treatment_col <- grep("^Z_", col_names, value = TRUE)
    confounder_cols <- input$analysis_plot_balance_select_var
    p <- plotBart::plot_balance(
      .data = X, 
      treatment = treatment_col, 
      confounders = confounder_cols
    )
    
    # add theme
    p <- p + theme_custom()
    
    return(p)
  })
  
  output$analysis_plot_balance_plot <- renderPlot({
    balance_plot()
  })
  
  output$download_balance_plot <- downloadHandler(
    filename = 'balance_plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = balance_plot(), device = device)
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
  output$analysis_diagnostics_plot_trace <- renderPlot({
    
    # stop here if model isn't fit yet
    validate_model_fit(store)
    
    # call function
    p <- plotBart::plot_trace(.model = store$model_results)
    
    # add theme
    p <- p + theme_custom()
    
    return(p)
  })
  
  # common support plot 
  output$analysis_diagnostics_plot_support <- renderPlot({
    
    # stop here if model isn't fit yet
    validate_model_fit(store)
    
    # plot it 
    p <- plotBart::plot_diagnostic_common_support(
      .model = store$model_results, 
      rule = 'none',
      plot_theme = theme_custom)
    
    return(p)
  })
  
  
  # specify model -----------------------------------------------------------
  
                           
  # pop ups for estimand and common support help 
  observeEvent(input$analysis_model_radio_estimand, {
    
    req(input$analysis_model_radio_estimand)
    
    if(input$analysis_model_radio_estimand == 'unsure'){
      shinyWidgets::sendSweetAlert(
        session,
        title = "I would like to learn more about causal estimands:",
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
  
  observeEvent(input$analysis_model_radio_support, {
    
    req(input$analysis_model_radio_support)
      
    if (input$analysis_model_radio_support == 'unsure'){
      shinyWidgets::sendSweetAlert(
        session,
        title = "I would like to learn more about common support:",
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
            width = "50%"),
        h5("...sometimes this takes a while..."),
      ),
      html = TRUE,
      btn_labels = NA,
      closeOnClickOutside = FALSE
    )
    
    # pull the response, treatment, and confounders variables out of the df
    treatment_v <- store$selected_df[, 1]
    response_v <- store$selected_df[, 2]
    confounders_mat <- as.matrix(store$selected_df[, 3:ncol(store$selected_df)])
    colnames(confounders_mat) <- str_sub(colnames(confounders_mat), start = 3)
    
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
    #store$good_model_fit <- TRUE
    
    # # update select on moderators page
    updateSelectInput(session = session,
                      inputId = 'analysis_moderator_vars',
                      choices = input$analysis_model_moderator_vars,
                      selected = input$analysis_model_moderator_vars[1])

    
    
    # add to log
    log_event <- paste0(
      'Ran BART model with following specification: \n',
      '\t', 'Experiment design: ', input$analysis_model_radio_design, '\n',
      '\t', 'Causal estimand: ', input$analysis_model_radio_estimand, '\n',
      '\t', 'Common support rule: ', input$analysis_model_radio_support, '\n',
      '\t', 'Moderators: ', paste0(input$analysis_model_moderator_vars, collapse = "; "), '\n',
      '\t', 'Model outcome: ', input$analysis_model_outcome, '\n',
      '\t', 'Propensity score fit: ', input$analysis_model_pscore, '\n',
      '\t', 'Good model fit: ', store$good_model_fit
    )
    store$log <- append(store$log, log_event)
    
    # close the alert
    shinyWidgets::closeSweetAlert()
    
    # common support warning
    common_support_check <- check_common_support(store$model_results)
    
    # display popup if any observations would be removed
    if((common_support_check$proportion_removed_sd > 0 | common_support_check$proportion_removed_chi > 0) & input$analysis_model_radio_support == 'none'){
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
    if((common_support_check$proportion_removed_sd == 0 | common_support_check$proportion_removed_chi == 0) & input$analysis_model_radio_support == 'none'){
      updateNavbarPage(session, inputId = "nav", selected = "Results")
    } 
    
    if( input$analysis_model_radio_support != 'none'){
      updateNavbarPage(session, inputId = "nav", selected = "Results")
    } 
    
    
  })
  
  
  # results -----------------------------------------------------------------
  
  # render the summary table
  output$analysis_results_table_summary <- DT::renderDataTable({

    # stop here if model isn't fit yet
    validate_model_fit(store)
    
    # extract estimates and format
    tab <- summary(store$model_results)$estimates %>% 
      t() %>%
      as.data.frame() %>% 
      create_datatable(., selection = "none")
    
    return(tab)
  })
  
  # TODO: render the interpretation text
  
  # PATE plot 
  output$analysis_results_plot_PATE <- renderPlot({
    
    # hold <- sum(input$show_interval == .8) > 0
    # print(hold == T)
    #print(input$show_interval == .8) 
    
    # stop here if model isn't fit yet
    validate_model_fit(store)
    
    if(input$show_reference == 'No'){
      # plot it
      # TODO: this is not in plotBart
      p <- plot_PATE(
          .model = store$model_results,
          type = input$plot_result_style,
          ci_80 = sum(input$show_interval == .8) > 0,
          ci_95 = sum(input$show_interval == .95) > 0,
          .mean = sum(input$central_tendency == 'Mean') > 0,
          .median = sum(input$central_tendency == 'Median') > 0,
          reference = NULL
        )
      
      # add theme
      p <- p + 
        theme_custom() + 
        theme(legend.position = c(0.1, 0.9),
              legend.title = element_blank())
    }
   
    if(input$show_reference != 'No'){
      # plot it
      p <- plot_PATE(
          .model = store$model_results,
          type = input$plot_result_style,
          ci_80 = sum(input$show_interval == .8) > 0,
          ci_95 = sum(input$show_interval == .95) > 0,
          .mean = sum(input$central_tendency == 'Mean') > 0,
          .median = sum(input$central_tendency == 'Median') > 0,
          reference = input$reference_bar
        )
      
      # add theme
      p <- p + 
        theme_custom() + 
        theme(legend.position = c(0.1, 0.9),
              legend.title = element_blank())
    }
    
    return(p)
  })

  # reproducible script
  # TODO: this hasn't been tested
  reproducible_script <- reactive({
    
    # these probably should be stored in realtime and then extracted here
    # this would prevent issues if user goes back and changes something but doesn't save it
    
    # file inputs
    uploaded_file_name <- input$analysis_data_upload$name
    uploaded_file_type <-  tools::file_ext(uploaded_file_name)
    uploaded_file_header <- input$analysis_data_header
    uploaded_file_delim <- input$analysis_data_delim_value
    
    # get the selected columns and names
    selected_columns <- colnames(store$col_assignment_df)
    column_names <- colnames(store$user_modified_df)
    
    # TODO: add data type changes
    
    # model
    estimand <- base::tolower(input$analysis_model_radio_estimand)
    common_support <- input$analysis_model_radio_support
    
    # create the script
    reproducible_script <- create_script(
      uploaded_file_name = uploaded_file_name,
      uploaded_file_type = uploaded_file_type,
      uploaded_file_header = uploaded_file_header,
      uploaded_file_delim = uploaded_file_delim,
      selected_columns = selected_columns,
      column_names = column_names,
      estimand = estimand,
      common_support = common_support
    )
    
    return(reproducible_script)
  })
  
  # download reproducible script
  output$analysis_results_button_download <- downloadHandler(
    filename <-  function() {
      time <- gsub("-|:| ", "", Sys.time())
      paste0(time, '_thinkCausal_script.zip')
    },
    content <- function(filename){
      
      # go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      # create file containing the clean_auto_convert_logicals function
      functionFile <- file("clean_auto_convert_logicals.R")
      writeLines(attributes(attributes(clean_auto_convert_logicals)$srcref)$srcfile$lines,
                 functionFile)
      close(functionFile)
      files <- "clean_auto_convert_logicals.R"
      
      # create the script file
      fileConn <- file("thinkCausal_script.R")
      writeLines(reproducible_script(), fileConn)
      close(fileConn)
      files <- c('thinkCausal_script.R', files)
      
      # create the zip file
      zip(filename, files)
    }
  )
  

  # moderators  -------------------------------------------------------------
  
  observeEvent(input$go_to_subgroup_results, {
    updateNavbarPage(session, inputId = "nav", selected = "Subgroup Results")
  })

    ## ICATE plots
  

  # histogram of icates
  output$histogram_icate <- renderPlot({
    
    # stop here if model isn't fit yet
    validate_model_fit(store)
    
    # TODO: this is not in plotBart
    p <- plot_individual_effects(store$model_results, type = input$icate_type)
    
    # add theme
    p <- p + theme_custom()
    
    return(p)
  })
  
  
  # single decision tree on icates
  output$analysis_moderator_single_tree <- renderPlot({
    
    # stop here if model isn't fit yet
    validate_model_fit(store)
    
    conf <- as.matrix(store$selected_df[, 3:ncol(store$selected_df)])
    colnames(conf) <- str_sub(colnames(conf, 3))
    
    # TODO: this is not in plotBart
    p <- plot_single_tree(store$model_results, confounders = conf, depth = input$set_tree_depth)
    
    return(p)
  })
  
  # plot the moderators
  output$analysis_moderators_explore_plot <- renderPlot({
    
    # stop here if model isn't fit yet
    validate_model_fit(store)
    
    # TODO: this does not close once plot is finished
    shinyWidgets::show_alert(
      title = 'Rendering Plot...',
      text = tags$div(
        img(src = file.path('img', 'tree.gif'),
            width = "20%"),
        h5("...sometimes this takes a while..."),
      ),
      html = TRUE,
      btn_labels = NA,
      closeOnClickOutside = T
    )

    # plot it
    # TODO: this is not in plotBart
    p <- plot_continuous_sub(.model = store$model_results, 
                             grouped_on = input$analysis_moderators_explore_select)
    
    # add theme
    p <- p + theme_custom()
    
    return(p)
  })
  
  
  # concepts ----------------------------------------------------------------
  
  # add listeners that link the concepts title image to its article
  tab_titles <- c("Randomization", 'Fundamental problem', 'Assumptions', 'Regression methods', 'Decision trees')
  lapply(tab_titles, function(page_to_go_to) {
    page_id <- paste0("concepts_link_", tolower(gsub(' ', '_', page_to_go_to)))
    observeEvent(input[[page_id]], {
      shinyjs::runjs("window.scrollTo(0, 0)")
      updateNavbarPage(session, "nav", page_to_go_to)
    })
  })
  
  # run the modules
  randomizationServer(id = 'concepts_randomization', plot_theme = theme_custom)
  PotentialOutcomesServer(id = 'concepts_potentialoutcomes')
  #poServer(id = 'potential_outcomes_test')
  
  # welcome page ------------------------------------------------------------
  
  # add listeners that link the front page images to their respective pages
  observeEvent(input$welcome_link_concepts, {
    updateNavbarPage(session, inputId = "nav", selected = "All concepts")
  })
  observeEvent(input$welcome_link_Analysis, {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load Data")
  })
  

  # options -----------------------------------------------------------------

  # change plot theme, font size, and point size
  theme_custom <- reactive({
    theme_custom <- switch(
      input$settings_options_ggplot_theme,
      "Minimal" = ggplot2::theme_minimal, 
      "Simple" = ggplot2::theme_bw, 
      "Classic" = ggplot2::theme_classic, 
      "Gray" = ggplot2::theme_gray
    )
    update_geom_defaults("point", list(size = input$settings_options_ggplotPointSize)) # is this a memory hog?
    theme_custom <- theme_custom(base_size = input$settings_options_ggplotTextSize)
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
    filename <-  function() {
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
  
  # example for interactive table output
  output$testytest <- renderText(get_table_values(input, 'mytable', ns = NS('yyp')))
  
  # example of new popup
  observeEvent(input$test_popup, {
    show_popup_waiting(session = session)
  })
  
})
