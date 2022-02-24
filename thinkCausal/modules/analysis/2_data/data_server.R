
server_data <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$analysis_data_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = "Design")
      })
      observeEvent(input$analysis_data_select_button_back, {
        updateTabsetPanel(global_session, inputId = "analysis_data_tabs", selected = "Group")
      })
      observeEvent(input$analysis_data_pivot_button_back, {
        updateTabsetPanel(global_session, inputId = "analysis_data_tabs", selected = "Upload")
      })
      
      # read in the uploaded file
      uploaded_df <- reactive({
        # TODO: test all the file types (e.g. stata is only stata 15 or older)
        # TODO: what about tsv files?
        # TODO: add parsing failures to log
        
        req(input$analysis_data_upload)
        
        # extract the filepath and the filetype
        filepath <- input$analysis_data_upload$datapath
        filetype <- tools::file_ext(filepath)
        
        # stop if not one of the accepted file types
        # this should be caught by fileInput() on the UI side
        accepted_filetypes <- c('csv', 'txt', 'xlsx', 'dta', 'sav')
        validate(need(
          filetype %in% accepted_filetypes,
          paste(
            'Filetype not accepted. Only accept ',
            paste0(accepted_filetypes, collapse = ', ')
          )
        ))
        
        tryCatch({
          
          # if it's a txt file then ask the user what the delimiter is
          if (filetype == 'txt'){
            output$show_delim <- reactive(TRUE)
            outputOptions(output, "show_delim", suspendWhenHidden = FALSE)
            req(input$analysis_data_delim_value)
          } else {
            output$show_delim <- reactive(FALSE)
          }
          
          # upload the file based on its filetype
          if (filetype == "csv"){
            uploaded_file <- readr::read_csv(
              file = filepath,
              col_names = input$analysis_data_header
            )
          } else if (filetype == 'dta'){
            uploaded_file <- readstata13::read.dta13(file = filepath)
          } else if (filetype == 'xlsx'){
            uploaded_file <- openxlsx::read.xlsx(xlsxFile = filepath)
          } else if (filetype == 'txt'){
            uploaded_file <- readr::read_delim(
              file = filepath,
              delim = input$analysis_data_delim_value,
              col_names = input$analysis_data_header
            )
          } else if (filetype == 'sav'){
            uploaded_file <- Hmisc::spss.get(file = filepath)
          } else stop("File type is invalid")
        },
        error = function(e) {
          # return a safeError if a parsing error occurs or if dataset isn't yet uploaded
          stop(safeError(e))
        })
        
        # clean column names; bad names will crash the server
        colnames(uploaded_file) <- clean_names(colnames(uploaded_file))
        
        return(uploaded_file)
      })
      
      # add dataframe to store object
      observeEvent(uploaded_df(), {
        
        # remove any previous dataframes from the store
        store$uploaded_df <- NULL
        store <- remove_downstream_data(store, page = 'upload')
        
        # stop here if uploaded data seems invalid
        validate(need(
          ncol(uploaded_df()) > 1,
          'Uploaded dataframe only has one column. Is the delimiter correct?'
        ))
        
        # add to log
        log_event <- paste0('Uploaded ', input$analysis_data_upload$name)
        store$log <- append(store$log, log_event)
        
        # retrieve the raw uploaded data frame
        uploaded_df <- uploaded_df()
        
        # auto convert all of the logical columns
        auto_cleaned_df <- clean_auto_convert_logicals(uploaded_df)
        
        # auto convert columns that are integers and few levels to factors
        auto_cleaned_df <- clean_auto_convert_integers(auto_cleaned_df)
        
        # add to store
        store$uploaded_df <- auto_cleaned_df
      })
      
      # render the drag and drop UI
      output$analysis_data_UI_dragdrop <- renderUI({
        
        # stop here if design hasn't been specified
        validate_design(store)
        
        # stop here if data hasn't been uploaded
        validate_data_uploaded(store)
        
        # render the drag-drop UI
        drag_drop_html <- create_drag_drop_roles(ns = ns, 
                                                 .data = store$uploaded_df,
                                                 ns_prefix = 'analysis_data',
                                                 design = store$analysis_design)
        
        return(drag_drop_html)
      })
      
      # create new dataframe when user saves column assignments and move to next page
      observeEvent(input$analysis_data_button_columnAssignSave, {
        
        req(store$uploaded_df)
        
        # remove any previous dataframes from the store
        store <- remove_downstream_data(store, page = 'upload')
        
        # get user inputs
        cols_z <- input$analysis_data_dragdrop_treatment
        cols_y <- input$analysis_data_dragdrop_response
        if(store$analysis_design == 'Block randomized treatment'){
          cols_x <- c(input$analysis_data_dragdrop_block, input$analysis_data_dragdrop_covariates)
        }
        else{
          cols_x <- input$analysis_data_dragdrop_covariates
        }
        
        all_cols <- unlist(c(cols_z, cols_y, cols_x))
        
        # are there duplicate selections?
        all_unique <- isTRUE(length(all_cols) == length(unique(all_cols)))
        z_is_only_one <- length(cols_z) == 1
        y_is_only_one <- length(cols_y) == 1
        x_more_than_zero <- length(cols_x) > 0
        
        # is treatment binary?
        is_treatment_binary <- tryCatch({
          treatment <- store$uploaded_df[[cols_z[1]]]
          is_binary <- isTRUE(clean_detect_logical(treatment))
          is_binary
        },
        error = function(e) FALSE,
        warning = function(w) FALSE
        )
        
        # is response continuous or binary?
        is_response_cont_binary <- tryCatch({
          response <- store$uploaded_df[[cols_y[[1]]]]
          is_cont_binary <- clean_detect_continuous_or_logical(response)
          is_cont_binary
        },
        error = function(e) FALSE,
        warning = function(w) FALSE
        )
        
        # is blocking variable categorical?
        is_block_categorical <- TRUE
        if (store$analysis_design == 'Block randomized treatment'){
          is_block_categorical <- purrr::map_lgl(input$analysis_data_dragdrop_block, function(var){
            is_cat_or_logical(store$uploaded_df[[var]])
          })
          is_block_categorical <- all(is_block_categorical)
        }
        
        # did it pass all checks?
        all_good <- isTRUE(all(
          c(
            all_unique,
            z_is_only_one,
            y_is_only_one,
            x_more_than_zero,
            is_treatment_binary,
            is_response_cont_binary,
            is_block_categorical
          )
        ))
        
        # launch error message
        if (!all_good) show_popup_variable_assignment_warning(session)
        
        validate(need(all_good, "There is an issue with column assignment"))
        
        # store the new dataframe using the uploaded df as the template
        store$col_assignment_df <- store$uploaded_df[, all_cols]
        
        # save columns assignments
        store$column_assignments <- NULL
        store$column_assignments$z <- cols_z
        store$column_assignments$y <- cols_y
        store$column_assignments$x <- cols_x
        store$column_assignments$blocks <- input$analysis_data_dragdrop_block
        
        # add to log
        log_event <- paste0('Assigned columns to roles: \n',
                            '\ttreatment: ', cols_z, '\n',
                            '\tresponse: ', cols_y, '\n',
                            '\tcovariates: ', paste0(cols_x, collapse = '; '))
        store$log <- append(store$log, log_event)
        
        # move to next page
        updateTabsetPanel(session = global_session, inputId = "analysis_data_tabs", selected = "Group")
      })
      
      
      # group data -------------------------------------------------------------
      
      # initiate counter of number of groups to un-dummy
      store$n_dummy_groups <- 1
      
      # to save user's grouping results
      group_list <- reactiveValues(data = list())
      
      # create UI for the groupings
      output$analysis_data_UI_dragdrop_grouping <- renderUI({
        
        # stop here if data hasn't been uploaded and columns assigned
        validate_columns_assigned(store)
        
        # stop here if no variables to group
        df <- store$col_assignment_df[, -c(1:2)]
        cat_var_names <- colnames(df)[sapply(df, clean_detect_logical)]
        validate(
          need(
            length(cat_var_names) > 1,
            "Your dataset has <= 1 logical variable. No need to group variables. Click 'Save groupings' to the next page."
          )
        )
        
        # add overlay
        div_id <- 'analysis_data_UI_dragdrop_grouping'
        show_message_updating(div_id)
        
        # create groupings: if 'add group' has not been clicked, show smart default grouping
        # if 'add group' is clicked, show last grouping results before clicking the button
        if (length(group_list$data) == 0) {
          drag_drop_groupings <- create_drag_drop_groups(
            ns = ns,
            .data = store$col_assignment_df,
            ns_prefix = 'analysis_data',
            n_dummy_groups = store$n_dummy_groups
          )
        } else{
          drag_drop_groupings <- create_drag_drop_groups(
            ns = ns,
            .data = store$col_assignment_df,
            ns_prefix = 'analysis_data',
            n_dummy_groups = store$n_dummy_groups,
            grouped_varibles = group_list$data
          )
        }
        
        # remove overlay
        close_message_updating(div_id)
        
        # update global var keeping track of the number of groups
        if(store$n_dummy_groups < 1){
          store$n_dummy_groups <- drag_drop_groupings$n_dummy_groups
        }
        
        # return the HTML code for the UI
        return(drag_drop_groupings$html)
      })
      
      # save the current grouping results and the number of group increases one when observe 'add a group' clicked
      observeEvent(input$analysis_data_add_group, {
        
        df <- store$col_assignment_df
        groups <- list()
        
        for (i in 1:store$n_dummy_groups) {
          # find the column indexes of dummy variables in the same group
          input_id <- paste0("analysis_data_categorical_group_", i)
          idx <- which(colnames(df) %in% input[[input_id]])
          # save the user input group name and dummies' names of the group into a list for each group
          groups <- c(groups, list(c(input[[paste0("rename_group_", i)]], colnames(df)[idx])))
        }
        group_list$data <- groups
        store$n_dummy_groups <- store$n_dummy_groups + 1
      })
      
      observeEvent(input$analysis_data_remove_group, {
        # save the current results when observe 'remove group' clicked
        df <- store$col_assignment_df
        groups <- list()
        
        for (i in 1:store$n_dummy_groups) {
          # find the column indexes of dummy variables in the same group
          input_id <- paste0("analysis_data_categorical_group_", i)
          idx <- which(colnames(df) %in% input[[input_id]])
          # save the user input group name and dummies' names of the group into a list for each group
          groups <- c(groups, list(c(input[[paste0("rename_group_", i)]], colnames(df)[idx])))
        }
        group_list$data <- groups
        
        # remove the last group from the results list and the number of group decreases one
        group_list$data <- group_list$data[-store$n_dummy_groups]
        store$n_dummy_groups <- store$n_dummy_groups - 1
      })
      
      # create new dataframe when user saves variable grouping
      observeEvent(input$analysis_data_save_groupings, {
        
        req(store$col_assignment_df)
        
        # remove any previous dataframes from the store
        store <- remove_downstream_data(store, page = 'group')
        
        store$grouped_df <- store$col_assignment_df
        problematic_group_names <- c()
        groups <- list()
        
        # save grouping results before clicking save changes button
        for (i in 1:store$n_dummy_groups) {
          # find the column indexes of dummy variables in the same group
          input_id <- paste0("analysis_data_categorical_group_", i)
          idx <- which(colnames(store$grouped_df) %in% input[[input_id]])
          # save the user input group name and dummies' names of the group into a list for each group
          groups <- c(groups, list(c(input[[paste0("rename_group_", i)]], colnames(store$grouped_df)[idx])))
        }
        group_list$data <- groups
        
        # only if there are grouped variables, proceed to convert dummies to categorical; otherwise, not update the dataframe
        if(length(group_list$data) > 0){
          
          for (i in 1:store$n_dummy_groups) {
            # find the column indexes of dummy variables in the same group
            input_id <- paste0("analysis_data_categorical_group_", i)
            cleaned_tmp <- clean_dummies_to_categorical_internal(i, store$grouped_df, input[[input_id]], input[[paste0("rename_group_", i)]], problematic_group_names)
            store$grouped_df <- cleaned_tmp[[2]]
            problematic_group_names <- cleaned_tmp[[1]]
          }
          
          # launch warning message:
          # if there are empty variable names, click ok will stay at the page
          if(length(problematic_group_names) != 0){
            show_popup_group_name_warning(session, problematic_group_names)
          }
          
          # add to log
          log_event <- 'Assigned dummy coded variables to groups: \n'
          for (i in 1:store$n_dummy_groups){
            input_id <- paste0("analysis_data_categorical_group_", i)
            log_event <- paste0(log_event, '\tgroup', i, ': ', paste0(input[[input_id]], collapse = '; '), '\n')
          }
          store$log <- append(store$log, log_event)
          
        }
        # if no variable names are empty, by clicking Save Grouping, move to next page
        if(length(problematic_group_names) == 0){
          updateTabsetPanel(session = global_session, inputId = "analysis_data_tabs", selected = "Verify")
        }
        
        # create a copy of the dataframe that the user can modify on the verify page
        colnames(store$grouped_df) <- clean_names(colnames(store$grouped_df))
        store$user_modifed_df <- store$grouped_df
      })
      
      # once new data is uploaded, grouped result is cleared, and the number of groups changes to either the number of smart defaul groups or 1
      observeEvent(input$analysis_data_button_columnAssignSave, {
        group_list$data <- c()
        auto_groups <- NULL#list(contains_dummy = FALSE, dummy_columns = NULL)#clean_detect_dummy_cols_unique(store$col_assignment_df)
        store$n_dummy_groups <- max(length(auto_groups), 1)
      })
      
      
      # verify data -------------------------------------------------------------
      
      # maintain a user modified dataframe that is continuously updated
      observe({
        
        # stop here if columns haven't been assigned
        validate_data_grouped(store)
        
        # stop here if analysis_data_modify_UI hasn't been rendered yet
        req(input$analysis_data_1_changeDataType)
        
        # use assigned dataframe as the template
        user_modified_df <- store$grouped_df
        
        # get input indices and current input values
        indices <- seq_along(user_modified_df)
        current_values <- reactiveValuesToList(input)
        
        ## change column names
        user_entered_names <- as.character(current_values[paste0("analysis_data_", indices, '_rename')])
        user_entered_names <- clean_names(user_entered_names)
        validate(need(length(user_entered_names) == length(unique(user_entered_names)),
                      'Cannot have duplicate column names'))
        
        names(user_modified_df) <- user_entered_names
        
        # change data types
        new_data_types <- as.character(current_values[paste0('analysis_data_', indices, '_changeDataType')])
        user_modified_df <- convert_data_types(.data = user_modified_df, new_data_types = new_data_types)
        
        # save the data to the store
        store$user_modified_df <- user_modified_df
      })
      
      # reset dataframe back to original when user clicks button
      observeEvent(input$analysis_data_button_reset, {
        
        # reset dataframe
        store$user_modified_df <- store$grouped_df
        
        ## reset UI
        # set indices to map over
        all_col_names <- colnames(store$grouped_df)
        default_data_types <- convert_data_type_to_simple(store$grouped_df)
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
      
      # render UI for modifying the data
      output$analysis_data_modify_UI <- renderUI({
        
        # stop here if columns haven't been assigned and grouped
        validate_columns_assigned(store)
        validate_data_grouped(store)
        
        # get default data types
        default_data_types <- convert_data_type_to_simple(store$grouped_df) #! input data changed to the result of group data
        
        # add default column types to store
        store$current_simple_column_types <- default_data_types
        
        # create UI table
        UI_table <- create_data_summary_grid(
          ns = ns,
          .data = store$grouped_df,
          default_data_types = default_data_types,
          ns_prefix = 'analysis_data',
          design = store$analysis_design,
          blocking_variables = store$column_assignments$blocks
        )
        
        return(UI_table)
      })
      
      # update percentNAs fields with actual data
      observeEvent(store$user_modified_df, {
        
        # stop here if columns haven't been assigned and grouped
        validate_columns_assigned(store)
        validate_data_grouped(store)
        
        # original data column indices
        indices <- seq_along(colnames(store$user_modified_df))
        
        # update the percent NA
        lapply(X = indices, function(i) {
          percent_NA_num <- mean(is.na(store$user_modified_df[[i]]))
          percent_NA <- paste0(round(percent_NA_num, 3) * 100, "%")
          updateTextInput(
            session = session,
            inputId = paste0("analysis_data_", i, "_percentNA"),
            value = percent_NA
          )
          # change font color to red if more than 10% NAs
          if(percent_NA_num > 0.1){
            shinyjs::runjs(
              paste0(
                'document.getElementById("',
                ns(paste0("analysis_data_", i, "_percentNA")),
                '").style.color = "#c92626"'
              ))
          } else {
            shinyjs::runjs(
              paste0(
                'document.getElementById("',
                ns(paste0("analysis_data_", i, "_percentNA")),
                '").style.color = null'
              ))
          }
        })
      })
      
      # table of selected dataset
      output$analysis_data_table <- DT::renderDataTable({
        
        # stop here if columns haven't been assigned and grouped
        validate_columns_assigned(store)
        validate_data_grouped(store)
        
        # stop here if analysis_data_modify_UI hasn't yet rendered
        # this works because colnames of user_modified_df is overwritten by the UI
        # which defaults to an empty string before it renders
        validate(need(
          unique(colnames(store$user_modified_df)) != '',
          'Loading...'
        ))
        
        # create JS datatable
        tab <- store$user_modified_df %>%
          na.omit() %>%
          create_datatable(selection = "none", pageLength = 10)
        
        return(tab)
      })
      
      # render the text indicating how many rows are being removed due to NAs
      output$analysis_data_text_na <- renderUI({
        
        # stop if previous steps aren't completed
        req(store$user_modified_df)
        
        # calculate stats
        n_rows_original <- nrow(store$user_modified_df)
        n_rows_removed <- n_rows_original - nrow(na.omit(store$user_modified_df))
        n_rows_percent <- scales::percent_format(0.1)(n_rows_removed / n_rows_original)
        n_rows_removed_text <- scales::comma_format()(n_rows_removed)
        
        # if no rows to be removed then don't show anything
        if (n_rows_removed == 0) return(NULL)
        
        # define text to be rendered
        text_out <- paste0(n_rows_removed_text, ' rows (', n_rows_percent, ') will be removed due to NAs in at least one column.')
        
        # make red
        if ((n_rows_removed / n_rows_original) > 0.1) text_out <- paste0('<p style="color: red">', text_out, "</p>")
        html_out <- HTML(text_out)
        
        return(html_out)
      })
      
      # when user hits 'save column assignments', create a new dataframe from store$uploaded_df
      # with the new columns
      # create updated options for plotting and modeling pages
      observeEvent(input$analysis_data_save, {
        
        req(store$user_modified_df)
        
        # remove saved dataframes if they exist
        store <- remove_downstream_data(store, 'verify')
        
        # new column names
        old_col_names <- colnames(store$user_modified_df)
        new_col_names <- paste0(c('Z',
                                  'Y',
                                  rep('X', length(old_col_names)-2)),
                                "_",
                                old_col_names)
        
        # save original column names
        store$verified_df_original_names <- old_col_names
        
        # create new dataframe of just the selected vars and rename them
        store$verified_df <- store$user_modified_df
        colnames(store$verified_df) <- new_col_names
        
        # remove rows with NAs
        store$verified_df <- na.omit(store$verified_df)
        
        # save the column names by their respective class
        store$column_types <- clean_detect_column_types(store$verified_df)
        
        # add to log
        column_types <- convert_data_type_to_simple(store$verified_df)
        log_event <- paste0(
          'Saved columns with following specification: \n',
          paste0(paste0("\t", new_col_names),
                 ": ",
                 column_types,
                 collapse = "\n")
        )
        store$log <- append(store$log, log_event)
        
        # update selects on Descriptive plots page
        col_names <- colnames(store$verified_df)
        cols_categorical <- store$column_types$categorical
        cols_continuous <- store$column_types$continuous
        
        # get smart defaults for the plotting variables
        column_treatment <- grep("^Z_", new_col_names, value = TRUE)
        column_response <- grep("^Y_", new_col_names, value = TRUE)
        plot_vars <- clean_detect_plot_vars(.column_types = store$column_types,
                                            .treatment_column = column_treatment,
                                            .response_column = column_response)
        store$analysis$data$verify$plot_vars <- plot_vars
        
        #update moderator select on model page and moderator test page
        
        #
        # updateSelectInput(session = session,
        #                   inputId = 'analysis_moderators_explore_select',
        #                   choices = X_mods)
        #
        # updateSelectInput(session = session,
        #                   inputId = 'analysis_moderators_explore_only',
        #                   choices = X_mods)
        
        
        # move to next page
        updateNavbarPage(global_session, inputId = "nav", selected = "Exploratory plots")
        updateTabsetPanel(global_session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
      })

      # update moderator options
      observeEvent(input$analysis_data_save, {
        cols_categorical <- store$column_types$categorical
        cols_continuous <- store$column_types$continuous
        X_cols_categorical <- grep("^X_", cols_categorical, value = TRUE)
        X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
        cols_categorical_cleaned <- gsub("X_", '', X_cols_categorical)
        cols_continuous_cleaned <- gsub("X_", '', X_cols_continuous)
        updateSelectInput(session = global_session, inputId = "plotBart_waterfall_order",
                          choices = c("ICATE", cols_continuous_cleaned))
        updateSelectInput(session = global_session, inputId = "plotBart_waterfall_color",
                          choices = c("None", cols_categorical_cleaned))
        updateSelectInput(session = global_session, inputId = "plotBart_ICATE_color",
                          choices = c("None", cols_categorical_cleaned))
        moderator_options <- gsub("X_", '', names(store$verified_df)[3:length(names(store$verified_df))])
        updateSelectInput(session = global_session, inputId = "plotBart_moderator_vars",
                          choices = c('',moderator_options))
      })
      
      # save into store for reproducible script
      group_list_data <- reactive(group_list$data)
      analysis_data_upload_name <- reactive(input$analysis_data_upload$name)
      analysis_data_header <- reactive(input$analysis_data_header)
      analysis_data_delim_value <- reactive(input$analysis_data_delim_value)
      analysis_data_save <- reactive(input$analysis_data_save)
      
      observeEvent(input$analysis_data_save, {
        store$analysis$data$group$group_list <- group_list_data()
        store$analysis$data$upload$analysis_data_upload$name <- analysis_data_upload_name()
        store$analysis$data$upload$analysis_data_header <- analysis_data_header()
        store$analysis$data$upload$analysis_data_delim_value <- analysis_data_delim_value()
        store$analysis$data$verify$analysis_data_save <- analysis_data_save()
      })
      
      return(store = store)
      
    })
}

