
server_data <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$analysis_upload_data_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$design)
      })
      
      # read in the uploaded file
      uploaded_df <- reactive({
        # TODO: test all the file types (e.g. stata is only stata 15 or older)
        # TODO: what about tsv files?
        # TODO: add parsing failures to log
        req(input$analysis_upload_data_upload)
        
        # extract the filepath and the filetype
        filepath <- input$analysis_upload_data_upload$datapath
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
            req(input$analysis_upload_data_delim_value)
          } else {
            output$show_delim <- reactive(FALSE)
          }
          
          # upload the file based on its filetype
          if (filetype == "csv"){
            uploaded_file <- readr::read_csv(
              file = filepath,
              col_names = input$analysis_upload_data_header
            )
          } else if (filetype == 'dta'){
            uploaded_file <- readstata13::read.dta13(file = filepath)
          } else if (filetype == 'xlsx'){
            uploaded_file <- openxlsx::read.xlsx(xlsxFile = filepath)
          } else if (filetype == 'txt'){
            uploaded_file <- readr::read_delim(
              file = filepath,
              delim = input$analysis_upload_data_delim_value,
              col_names = input$analysis_upload_data_header
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
        log_event <- paste0('Uploaded ', input$analysis_upload_data_upload$name)
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
      output$analysis_upload_data_UI_dragdrop <- renderUI({
        
        validate_design(store)
        validate_data_uploaded(store)
        
        # render the drag-drop UI
        drag_drop_html <- create_drag_drop_roles(ns = ns, 
                                                 .data = store$uploaded_df,
                                                 ns_prefix = 'analysis_upload_data',
                                                 design = store$analysis_design, 
                                                 weights = store$analysis_weights, 
                                                 ran_eff = store$analysis_random_effects)
        
        return(drag_drop_html)
      })
      
      # create new dataframe when user saves column assignments and move to next page
      observeEvent(input$analysis_upload_data_button_columnAssignSave, {
        
        validate_design(store)
        req(store$uploaded_df)
        
        # remove any previous dataframes from the store
        #TODO check this
        store <- remove_downstream_data(store, page = 'upload')
        
        # get user inputs
        cols_z <- input$analysis_upload_data_dragdrop_treatment
        cols_y <- input$analysis_upload_data_dragdrop_response
        cols_x <- input$analysis_upload_data_dragdrop_covariates
        if(store$analysis_design != "Block randomized treatment") cols_block <- NULL
        else cols_block <- input$analysis_upload_data_dragdrop_block
        if(store$analysis_weights != 'Yes') cols_weight <- NULL
        else cols_weight <- input$analysis_upload_data_dragdrop_weight
        if (store$analysis_random_effects != 'Yes') cols_ran_eff <- NULL
        else cols_ran_eff <- input$analysis_upload_data_dragdrop_ran_eff
        
        # the order of this is very important for create_data_summary_grid.R
        all_cols <- unlist(c(cols_z, cols_y,cols_ran_eff, cols_weight,cols_block,cols_x))
        
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
          is_block_categorical <- purrr::map_lgl(input$analysis_upload_data_dragdrop_block, function(var){
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
        store$column_assignments$weight <- cols_weight
        store$column_assignments$ran_eff <- cols_ran_eff
        store$column_assignments$blocks <- cols_block
        
        
        # add to log
        log_event <- paste0('Assigned columns to roles: \n',
                            '\ttreatment: ', cols_z, '\n',
                            '\tresponse: ', cols_y, '\n',
                            '\tcovariates: ', paste0(cols_x, collapse = '; '), 
                            '\tsurvey weight:', cols_weight, 
                            '\trandom intercepts:', paste0(cols_ran_eff, collapse = '; '), 
                            '\tblocking variable(s):', paste0(cols_block, collapse = '; '))
        
        store$log <- append(store$log, log_event)
        
        # move to next page
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$verify)
        
      })
      
      return(store = store)
      
    })
}

