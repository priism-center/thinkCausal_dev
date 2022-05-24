
server_verify <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$analysis_verify_data_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$data)
      })
      
      # maintain a user modified dataframe that is continuously updated
      observe({
        # stop here if data hasn't been uploaded and columns assigned
        validate_columns_assigned(store)
        
        # stop here if analysis_verify_data_modify_UI hasn't been rendered yet
        req(input$analysis_verify_data_1_changeDataType)
        
        # use assigned dataframe as the template
        user_modified_df <- store$col_assignment_df
        
        # get input indices and current input values
        indices <- seq_along(user_modified_df)
        current_values <- reactiveValuesToList(input)
        
        ## change column names
        user_entered_names <- as.character(current_values[paste0("analysis_verify_data_", indices, '_rename')])
        user_entered_names <- clean_names(user_entered_names)
        validate(need(length(user_entered_names) == length(unique(user_entered_names)),
                      'Cannot have duplicate column names'))
        
        names(user_modified_df) <- user_entered_names
        
        # change data types
        new_data_types <- as.character(current_values[paste0('analysis_verify_data_', indices, '_changeDataType')])
        user_modified_df <- convert_data_types(.data = user_modified_df, new_data_types = new_data_types)
        
        # save the data to the store
        store$user_modified_df <- user_modified_df
      })
      
      # reset dataframe back to original when user clicks button
      observeEvent(input$analysis_verify_data_button_reset, {
        
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
            inputId = paste0("analysis_verify_data_", i, "_rename"),
            value = all_col_names[i]
          )
          updateSelectInput(
            session = session,
            inputId = paste0("analysis_verify_data_", i, "_changeDataType"),
            selected = default_data_types[i]
          )
        })
      })
      
      # render UI for modifying the data
      output$analysis_verify_data_modify_UI <- renderUI({
        
        # stop here if columns haven't been assigned
        validate_columns_assigned(store)

        # get default data types
        default_data_types <- convert_data_type_to_simple(store$col_assignment_df) 
        
        # add default column types to store
        store$current_simple_column_types <- default_data_types
        
        # create UI table
        UI_table <- create_data_summary_grid(
          ns = ns,
          .data = store$col_assignment_df,
          default_data_types = default_data_types,
          ns_prefix = 'analysis_verify_data',
          design = store$analysis_design,
          blocking_variables = store$column_assignments$blocks, 
          random_effect = store$column_assignments$ran_eff,
          survey_weight = store$column_assignments$weight
        )
        
        return(UI_table)
      })
      
      # update percentNAs fields with actual data
      observeEvent(store$user_modified_df, {
        
        # stop here if columns haven't been assigned and grouped
        validate_columns_assigned(store)

        # original data column indices
        indices <- seq_along(colnames(store$user_modified_df))
        
        # update the percent NA
        lapply(X = indices, function(i) {
          percent_NA_num <- mean(is.na(store$user_modified_df[[i]]))
          percent_NA <- paste0(round(percent_NA_num, 3) * 100, "%")
          updateTextInput(
            session = session,
            inputId = paste0("analysis_verify_data_", i, "_percentNA"),
            value = percent_NA
          )
          # change font color to red if more than 10% NAs
          if(percent_NA_num > 0.1){
            shinyjs::runjs(
              paste0(
                'document.getElementById("',
                ns(paste0("analysis_verify_data_", i, "_percentNA")),
                '").style.color = "#c92626"'
              ))
          } else {
            shinyjs::runjs(
              paste0(
                'document.getElementById("',
                ns(paste0("analysis_verify_data_", i, "_percentNA")),
                '").style.color = null'
              ))
          }
        })
      })
      
      # table of selected dataset
      output$analysis_verify_data_table <- DT::renderDataTable({
        
        # stop here if columns haven't been assigned and grouped
        validate_columns_assigned(store)

        # stop here if analysis_verify_data_modify_UI hasn't yet rendered
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
      output$analysis_verify_data_text_na <- renderUI({
        
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
      observeEvent(input$analysis_verify_data_save, {
        req(store$user_modified_df)
        
        # remove saved dataframes if they exist
        store <- remove_downstream_data(store, 'verify')
        
        # new column names
        old_col_names <- colnames(store$user_modified_df)
        new_col_names <- paste0(c('Z',
                                  'Y',
                                  rep('B', length(store$column_assignments$blocks)),
                                  rep('R', length(store$column_assignments$ran_eff)),
                                  rep('W', length(store$column_assignments$weight)), 
                                  rep('X', length(old_col_names)-2 + length(store$column_assignments$weight) + length(store$column_assignments$ran_eff) + length(store$column_assignments$blocks))),
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
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$eda)
        updateTabsetPanel(global_session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
      })

      # update moderator options
      observeEvent(input$analysis_verify_data_save, {
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
     # group_list_data <- reactive(group_list$data)
      analysis_verify_data_upload_name <- reactive(input$analysis_verify_data_upload$name)
      analysis_verify_data_header <- reactive(input$analysis_verify_data_header)
      analysis_verify_data_delim_value <- reactive(input$analysis_verify_data_delim_value)
      analysis_verify_data_save <- reactive(input$analysis_verify_data_save)
      
      observeEvent(input$analysis_verify_data_save, {
        #store$analysis$data$group$group_list <- group_list_data()
        store$analysis$data$upload$analysis_verify_data_upload$name <- analysis_verify_data_upload_name()
        store$analysis$data$upload$analysis_verify_data_header <- analysis_verify_data_header()
        store$analysis$data$upload$analysis_verify_data_delim_value <- analysis_verify_data_delim_value()
        store$analysis$data$verify$analysis_verify_data_save <- analysis_verify_data_save()
      })
      
      return(store = store)
      
    })
}

