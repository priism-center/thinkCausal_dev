
server_model <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # back next button
      observeEvent(input$analysis_model_button_back, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$overlap)
      })
      
      
      # update variables on the model page once the save button on the verify data page is clicked
      observeEvent(store$analysis$data$verify$analysis_verify_data_save, {
        
        # create list of moderator combinations on model page
        cols_categorical <- store$column_types$categorical
        X_cols_categorical <- grep("^X_", cols_categorical, value = TRUE)
        cols_categorical_cleaned <- gsub("X_", '', X_cols_categorical)
        
        # # update options for random intercept
        # updateSelectInput(
        #   session = session,
        #   inputId = "analysis_random_intercept",
        #   choices = c("None", cols_categorical_cleaned),
        #   selected = "None"
        # )
        
        # create moderator options
        cols_continuous <- store$column_types$continuous
        X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
        X_cols <- grep("^X_", colnames(store$verified_df), value = TRUE)
        X_mods <- combn(X_cols, m = 2) %>% t() %>% as.data.frame()
        remove <- X_mods[X_mods$V1 %in% X_cols_continuous & X_mods$V2 %in% X_cols_continuous,]
        X_mods <- anti_join(X_mods, remove)
        X_mods <- mutate(X_mods,
                         V1 = gsub("X_", '', V1),
                         V2 = gsub("X_", '', V2))
        X_mods <- X_mods %>%
          mutate(mod = paste(V1, V2, sep = ' x ')) %>%
          pull(mod)
        X_mods <- c(gsub("X_", '', X_cols), X_mods)
        
        updateSelectInput(session = session,
                          inputId = 'analysis_model_moderator_vars',
                          choices = X_mods,
                          selected = NULL)
      })
      
      
      # when user runs the model, take a number of actions
      observeEvent(input$analysis_model_button_next, {
        
        # launch popup if data is not yet selected
        if (!is.data.frame(store$verified_df)) {
          show_popup_model_no_data_warning(session, ns = ns)
          
          observeEvent(input$analysis_model_button_popup, {
            close_popup(session = session)
            updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$data)
            updateTabsetPanel(global_session, inputId = "analysis_data_tabs", selected = "Upload")
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
          block = store$column_assignments$block,
          .weights = store$column_assignments$weight, 
          ran_eff = store$column_assignments$ran_eff,
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
                     close_button = shiny::actionButton(
                       inputId = 'analysis_model-analysis_model_button_closeModal', 
                       class = 'nav-btn-focus', 
                       `data-dismiss` = "modal",
                       `data-bs-dismiss` = "modal",
                       label = "Close"
                     )) #shiny::modalButton("Close"))
        }
        req(did_model_fit)
        
        # store the results
        # TODO: need way to test if actually have a good fit
        store$model_fit_good <- TRUE
        
        # update select on moderators page
        updateSelectInput(session = global_session,
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
          show_popup_common_support_warning(session = session, common_support_check = common_support_check, ns = ns)
        }
        
        # nav buttons within the popup
        # TODO: the 'see common support diagnostics doesn't go anywhere
        observeEvent(input$common_support_new_rule, {
          updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$model)
          close_popup(session = session)
        })
        observeEvent(input$common_support_continue, {
          updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$results)
          close_popup(session = session)
        })
        
        # move to next page based on model fit
        no_points_removed <- common_support_check$proportion_removed_sd == 0 | common_support_check$proportion_removed_chi == 0
        if(no_points_removed & input$analysis_model_support == 'No'){
          updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$results)
        }
        
        if( input$analysis_model_support != 'No'){
          updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$results)
        }
        
      })
      
      # save into store for reproducible script
      analysis_model_support <- reactive(input$analysis_model_support)
      analysis_over_ride_common_support <-  reactive(input$analysis_over_ride_common_support)
      analysis_model_moderator_vars <- reactive(input$analysis_model_moderator_vars)
      analysis_random_intercept <- reactive(input$analysis_random_intercept)
      analysis_model_estimand <- reactive(input$analysis_model_estimand)
      
      observeEvent(input$analysis_model_button_next, {
        store$analysis$model$analysis_model_support <- analysis_model_support()
        store$analysis$model$analysis_over_ride_common_support <- analysis_over_ride_common_support()
        store$analysis$model$analysis_model_moderator_vars <- analysis_model_moderator_vars()
        store$analysis$model$analysis_random_intercept <- analysis_random_intercept()
        store$analysis$model$analysis_model_estimand <- analysis_model_estimand()
      })
      
      return(store = store)
      
    }
  )
  
}

