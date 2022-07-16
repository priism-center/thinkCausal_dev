#' analysis_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_model_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::box(
        width = 4,
        collapsible = FALSE,
        title = '1. Specify model',
        selectInput(ns('analysis_model_estimand'),
                    label = 'Confirm causal estimand',
                    choices = c('ATE - Average treatment effect' = 'ATE',
                               'ATC - Average treatment effect on the control' = 'ATC',
                               'ATT - Average treatment effect on the treated' = 'ATT')),
        selectInput(ns('analysis_model_support'),
                    label = 'Remove observations without overlap',
                    choices = c('', 'Unsure', 'Yes', 'No')),
        HTML('<details><summary>Advanced modeling options</summary>'),
        selectInput(ns("analysis_over_ride_common_support"),
                    label = 'Overlap rule:',
                    choices = c('Standard deviation' = 'sd', 'Chi squared' = 'chisq'))
        ),
      bs4Dash::box(
        width = 4,
        collapsible = FALSE,
        title = '2. Specify secondary analyses',
        selectInput(ns('analysis_model_moderator_yes_no'),
                    label = 'Would you like to pre-specify subgroup analyses?',
                    choices = c("No", "Yes", 'Unsure')),
        conditionalPanel(
          condition = "input.analysis_model_moderator_yes_no == 'Yes'",
          ns = ns,
          selectInput(ns('analysis_model_moderator_vars'),
                      label = 'Create subgroups by:',
                      choices = NULL,
                      multiple = TRUE))
        ),
      bs4Dash::box(
        width = 4,
        collapsible = FALSE,
        title = '3. Fit model',
        actionButton(inputId = ns("analysis_model_button_next"),
                     class = "nav-path",
                     label = "Fit model"),
        actionButton(inputId = ns('analysis_model_help'),
                     label = 'Help me')
        )
      )
    )
}

#' analysis_model Server Functions
#'
#' @noRd
mod_analysis_model_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_model_help, {
      open_help_sidebar(store, 'Model')
    })

    # update estimand dropdown based on selected value in the design section
    observeEvent(store$analysis_design_estimand, {
      updateSelectInput(
        inputId = 'analysis_model_estimand',
        selected = store$analysis_design_estimand
      )
    })

    # update variables on the model page once the save button on the verify data page is clicked
    observeEvent(store$analysis$data$verify$analysis_verify_data_save, {

      # create list of moderator combinations on model page
      cols_categorical <- store$column_types$categorical
      X_cols_categorical <- grep("^X_", cols_categorical, value = TRUE)
      cols_categorical_cleaned <- gsub("X_", '', X_cols_categorical)

      # create moderator options
      cols_continuous <- store$column_types$continuous
      X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)

      X_cols <- gsub("X_", '',grep("^X_", colnames(store$verified_df), value = TRUE))
      X_mods <- X_cols
      #TODO update plot bart to allow for 3 way interactions
      # X_mods <- combn(X_cols, m = 2) %>% t() %>% as.data.frame()
      # remove <- X_mods[X_mods$V1 %in% X_cols_continuous & X_mods$V2 %in% X_cols_continuous,]
      # X_mods <- anti_join(X_mods, remove)
      # X_mods <- mutate(X_mods,
      #                  V1 = gsub("X_", '', V1),
      #                  V2 = gsub("X_", '', V2))
      # X_mods <- X_mods %>%
      #   mutate(mod = paste(V1, V2, sep = ' x ')) %>%
      #   pull(mod)
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
          bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_upload')
          # updateNavbarPage(store$session_global, inputId = "nav", selected = module_ids$analysis$upload)
        })
      }

      # make sure required inputs have values
      local({
        req_inputs <- c(
          'analysis_model_support',
          'analysis_model_moderator_yes_no'
        )
        req_values <- reactiveValuesToList(input)[req_inputs]

        # trigger animation if any inputs is unsure or blank
        inputs_to_animate <- req_inputs[which(req_values == 'Unsure' | req_values == '')]
        inputs_to_animate_selectors <- paste0("#", ns(inputs_to_animate), "+ div", collapse = ', ')
        shinyjs::runjs(glue::glue('$("<<inputs_to_animate_selectors>>").effect("shake", {times: 4, distance: 3})',
                         .open = "<<", .close = ">>"))

        # stop here if any unsures or blank inputs
        req(!isTRUE(length(inputs_to_animate) > 0))
      })

      # stop here if data hasn't been uploaded and selected
      validate_data_verified(store)

      # stop here if inputs aren't found
      # TODO
      # req(input$)
      # print('Dataframe going into bartC: \n')
      # print(store$verified_df)
      # print('Column types of dataframe going into bartC: \n')
      # print(store$verified_df  %>% summarize_all(class))

      # save the estimand (again, also saved on the design page)
      store$analysis_design_estimand <- input$analysis_model_estimand

      # remove current model if it exists
      store$analysis$model$model <- NULL
      store$analysis$model$fit_good <- NULL

      # save prespecified moderators
      store$analysis$subgroup$prespecified_subgroups <- input$analysis_model_moderator_vars
      if(rlang::is_null(input$analysis_model_moderator_yes_no)) store$analysis$results$prespecified_subgroups <- NULL

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
      # store$analysis$model$model <- withProgress(
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

      bart_model <- fit_bart(
        .data = store$verified_df,
        support = common_support_rule,
        block = store$column_assignments$block,
        .weights = store$column_assignments$weight,
        ran_eff = store$column_assignments$ran_eff,
        .estimand = base::tolower(input$analysis_model_estimand)
      )
      store$analysis$model$model <- bart_model

      # close the alert
      # shinyWidgets::closeSweetAlert()
      close_popup(session = session)

      # error handling
      # TODO: refine the popup; probably should pass the bart error to the popup somehow
      # TODO: is there a better way to detect if the model fit?
      did_model_fit <- !isTRUE(is.null(bart_model))
      if (!did_model_fit){
        store$analysis$model$fit_good <- FALSE
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
      store$analysis$model$fit_good <- TRUE

      # update exploratory moderators
      updateSelectInput(session = store$session_global,
                        inputId = 'analysis_moderator_vars',
                        choices = input$analysis_model_moderator_vars,
                        selected = input$analysis_model_moderator_vars[1])



      # add to log
      log_event <- paste0(
        'Ran BART model with following specification: \n',
        # '\t', 'Experiment design: ', input$analysis_model_radio_design, '\n',
        '\t', 'Causal estimand: ', input$analysis_model_estimand, '\n',
        '\t', 'Remove observations without overlap: ', input$analysis_model_support, '\n',
        '\t', 'Common support rule: ', common_support_rule, '\n',
        '\t', 'Moderators: ', paste0(input$analysis_model_moderator_vars, collapse = "; "), '\n',
        # '\t', 'Model outcome: ', input$analysis_model_outcome, '\n',
        # '\t', 'Propensity score fit: ', input$analysis_model_pscore, '\n',
        '\t', 'Good model fit: ', store$analysis$model$fit_good
      )
      store$log <- append(store$log, log_event)

      # common support warning
      common_support_check <- check_common_support(bart_model)
      # display popup if any observations would be removed
      any_points_removed <- common_support_check$proportion_removed_sd > 5 | common_support_check$proportion_removed_chi > 5
      if(any_points_removed & input$analysis_model_support == 'No'){
        show_popup_common_support_warning(session = session, common_support_check = common_support_check, ns = ns)
      }

      # nav buttons within the popup
      observeEvent(input$common_support_opt3, {
        bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_diagnostic')
        # updateNavbarPage(store$session_global, inputId = "nav", selected = store$module_ids$analysis$diagnostic)
        # updateTabsetPanel(store$session_global, inputId = "analysis_diagnostic-analysis_diagnostics_tabs", selected = "Overlap")
        close_popup(session = session)
      })

      observeEvent(input$common_support_opt2, {
        shinyjs::runjs('openHelpSection("help-model")')
      })

      observeEvent(input$common_support_new_rule, {
        bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_model')
        # updateNavbarPage(store$session_global, inputId = "nav", selected = store$module_ids$analysis$model)
        close_popup(session = session)
      })
      observeEvent(input$common_support_continue, {
        bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_results')
        # updateNavbarPage(store$session_global, inputId = "nav", selected = store$module_ids$analysis$results)
        close_popup(session = session)
      })

      # move to next page based on model fit
      no_points_removed <- common_support_check$proportion_removed_sd == 0 | common_support_check$proportion_removed_chi == 0
      if(no_points_removed & input$analysis_model_support == 'No'){
        bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_results')
        # updateNavbarPage(store$session_global, inputId = "nav", selected = store$module_ids$analysis$results)
      }

      if( input$analysis_model_support != 'No'){
        bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_results')
        # updateNavbarPage(store$session_global, inputId = "nav", selected = store$module_ids$analysis$results)
      }

    })

    # save into store for reproducible script
    # analysis_model_support <- reactive(input$analysis_model_support)
    # analysis_over_ride_common_support <-  reactive(input$analysis_over_ride_common_support)
    # analysis_model_moderator_vars <- reactive(input$analysis_model_moderator_vars)
    # analysis_random_intercept <- reactive(input$analysis_random_intercept)
    # analysis_model_estimand <- reactive(input$analysis_model_estimand)

    observeEvent(input$analysis_model_button_next, {
      # store$analysis$model$analysis_model_support <- analysis_model_support()
      # store$analysis$model$analysis_over_ride_common_support <- analysis_over_ride_common_support()
      # store$analysis$model$analysis_model_moderator_vars <- analysis_model_moderator_vars()
      # store$analysis$model$analysis_random_intercept <- analysis_random_intercept()
      # store$analysis$model$analysis_model_estimand <- analysis_model_estimand()
      store$analysis$model$analysis_model_support <- isolate(input$analysis_model_support)
      store$analysis$model$analysis_over_ride_common_support <- isolate(input$analysis_over_ride_common_support)
      store$analysis$model$analysis_model_moderator_vars <- isolate(input$analysis_model_moderator_vars)
      store$analysis$model$analysis_random_intercept <- isolate(input$analysis_random_intercept)
      store$analysis$model$analysis_model_estimand <- isolate(input$analysis_model_estimand)
    })


    # open slide over if answer is unsure
    dropdown_inputs <- c("analysis_model_support", "analysis_model_moderator_yes_no")
    purrr::map(dropdown_inputs, function(input_id){
      observeEvent(input[[input_id]], {
        if (input[[input_id]] == "Unsure") shinyjs::runjs('openHelpSection("help-model")')
      })
    })

    return(store)
  })
}

