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
      column(
        width = 12,
      bs4Dash::box(
        title = 'Fit Your Model',
        width = 12,
        collapsible = FALSE,
      selectInput(ns('analysis_model_moderator_yes_no'),
                  label = 'Before fitting your model, would you like to pre-specify a subgroup analyses:',
                  choices = c("", "No", "Yes", 'Unsure')),


      conditionalPanel(
        condition = "input.analysis_model_moderator_yes_no == 'Yes'",
        ns = ns,
        selectInput(ns('analysis_model_moderator_vars'),
                    label = 'Create subgroups by:',
                    choices = NULL,
                    multiple = TRUE)),

        # conditionalPanel(condition = "output.value == '0'",
        #                  ns = ns,
        #                  title = 'Step 1: Choose a Causal Estimand',
        #                  selectInput(ns('analysis_model_estimand'),
        #                              label = 'Select a causal estimand:',
        #                              choices = c('',
        #                                          'ATE - Average treatment effect' = 'ATE',
        #                                          'ATT - Average treatment effect on the treated' = 'ATT',
        #                                          'ATC - Average treatment effect on the control' = 'ATC'))
        #                  ),
        # conditionalPanel(condition = "output.value == '1'",
        #                  ns = ns,
        #                  title = 'Step 2: Choose Pre-specified sug-group analyses',
        #                  selectInput(ns('analysis_model_moderator_yes_no'),
        #                              label = 'Pre-specify subgroup analyses:',
        #                              choices = c("", "No", "Yes", 'Unsure'))
        #
        #                  ),
        # conditionalPanel(
        #   condition = "input.analysis_model_moderator_yes_no == 'Yes' & output.value == '1'",
        #   ns = ns,
        #   selectInput(ns('analysis_model_moderator_vars'),
        #               label = 'Create subgroups by:',
        #               choices = NULL,
        #               multiple = TRUE)),
        br(),
        actionButton(inputId = ns("analysis_model_button_next"),
                     class = "nav-path",
                     label = "Fit model"),
        #p("BART models are nonparametric. This means that any interactions or non-linear relationships in the model for the outcome conditional on the treatment and covariates are automatically learned and included in the model. Therefore, using this approach, there is no need to manually include interaction terms, squared terms, or transformations of variables."),
        br(),
        actionButton(inputId = ns('analysis_model_help'),
                     label = 'Help me')
       # p("thinkCausal can also automatically identify important moderators of the treatment effect. Thus if a treatment effect varies or changes across different values of one of our covariates, thinkCausal can automatically detect this treatment effect heterogeneity and help you identify which variables are driving it. That means that if there are variables you believe are moderators you are encouraged to pre-specify them in the 'specify model' options.")

        #,
        #verbatimTextOutput(ns('review'))
      )
      ),
      # column(width = 6,
      #        bs4Dash::box(
      #          width = 12,
      #          collapsible = FALSE,
      #          title = 'Review your model',
      #          tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
      #          h5('Causal Question:'),
      #          verbatimTextOutput(ns('causal_q')),
      #          h5('Design:'),
      #          verbatimTextOutput(ns('design')),
      #          h5('Causal Estimand:'),
      #          verbatimTextOutput(ns('estimand'))
      #
      #
      #        )
      #        )
      )
    )
}

#' analysis_model Server Functions
#'
#' @noRd
mod_analysis_model_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # reactive Val to track what is showing on the page
    position <- reactiveValues(value = 0)

    output$value <- reactive({
      return(as.character(position$value))
    })
    outputOptions(output, "value", suspendWhenHidden = FALSE)


    # open help on button click
    observeEvent(input$analysis_model_help, {
      open_help_sidebar(store, 'Model')
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

      X_cols <- gsub("X_", '', grep("^X_", colnames(store$verified_df), value = TRUE))
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

    # model review page on left pannel
    output$original <- renderText({
      covariates <- paste0(store$column_assignments$x, collapse = ', ')

      excluded <- paste0(names(store$analysis_data_uploaded_df)[names(store$analysis_data_uploaded_df) %notin% c(store$column_assignments$x, store$column_assignments$z, store$column_assignments$y)], collapse = ', ')
      design <- store$analysis_select_design
      if(design == 'Observational Study (Treatment not Randomized)'){
        design <- paste('an', design)
      }else{
        design <- paste('a', design)
      }
      estimand <- ifelse(input$analysis_model_estimand == '',
                         'You have not selected a causal estimand. You will need to make a selection before fitting your model.',
                         paste0('You are estimating the ', input$analysis_model_estimand)
      )

      treatment <- store$column_assignments$z
      outcome <- store$column_assignments$y

      X <- ifelse(design == 'Observational Study (Treatment not Randomized)', 'Confounders', 'Covariates')
      glue::glue(
        "
      Does the treatment variable {treatment} causes changes in outcome variable {outcome}?\n\t

      Your data is from {design}\n\t
      Estimand: {estimand}\n\t

      You are including the following variables in your model:\n\t
      {covariates}

      You are not controlling for:\n\t
      {excluded}

      ")
    })


    output$causal_q <- renderText({
      treatment <- store$column_assignments$z
      outcome <- store$column_assignments$y

      glue::glue(
      "Does the treatment variable {treatment} causes changes in outcome variable {outcome}?\n\t")
    })

    output$design <- renderText({

      design <- store$analysis_select_design
      if(design == 'Observational Study (Treatment not Randomized)'){
        design <- paste('an', design)
      }else{
        design <- paste('a', design)
      }


      X <- ifelse(design == 'Observational Study (Treatment not Randomized)', 'Confounders', 'Covariates')
      glue::glue(
        "
      Your data is from {design}\n\t
      ")
    })


    output$estimand <- renderText({
      req(input$analysis_model_estimand)
      treatment <- store$column_assignments$z
      outcome <- store$column_assignments$y

      # estimand <- ifelse(input$analysis_model_estimand == '',
      #                    'You have not selected a causal estimand. You need to select an estimand to define who you are considering the casual effect for.',
      #                    paste0('You are estimating the ', input$analysis_model_estimand)
      # )
      estimand <- input$analysis_model_estimand
      causal_group <- switch (estimand,
        'ATE' = 'For all the individuals in your sample, did recieving the treatment {treatment} casuse values of the outcome {outcome} to be differant than they would have been had they not recived the treatment {treatment}?',
        'ATT' = 'For the individuals in your sample that recived the treatment {treatment}, did recieving the treatment {treatment} casuse values of the outcome {outcome} to be differant than they would have been had they not recived the treatment {treatment}?',
        'ATC' = 'For the individuals in your sample that did not recive the treatment {treatment}, did recieving the treatment ({treatment}) casuse values of the outcome {outcome} to be differant than they would have been had they not recived the treatment {treatment}?'

      )

      glue::glue(
        "
      You are estimating the {estimand}\n\t

      {causal_group} \n\t
      ")
    })




    # when user runs the model, take a number of actions
    observeEvent(input$analysis_model_button_next, {
      # update position value on next or back button click
      position$value <- position$value + 1
      # this value is set based on the number of sub-pages + 1
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

        # remove current model if it exists
        store$analysis$model$model <- NULL
        store$analysis$model$fit_good <- NULL

        # save prespecified moderators
        store$analysis$subgroup$prespecified_subgroups <- input$analysis_model_moderator_vars
        if(rlang::is_null(input$analysis_model_moderator_yes_no)) store$analysis$results$prespecified_subgroups <- NULL

        # insert popup to notify user of model fit process
        # TODO: estimate the time remaining empirically?
        # TODO: show console redirect

        # fit BART model
        show_popup_fitting_BART_waiting(session)

        bart_model <- fit_bart(
          .data = store$verified_df,
          .weights = store$column_assignments$weight,
          ran_eff = store$column_assignments$ran_eff,
          .estimand = store$analysis$model$analysis_model_estimand, # estimand handled latter in post processing
          rct = store$analysis_select_design == 'Completely Randomized Experiment'
        )
        store$analysis$model$model <- bart_model

        # check common support
        store$analysis$model$overlap_checks <- check_overlap_rules(.model = bart_model)


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



         bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_results')
        # updateNavbarPage(store$session_global, inputId = "nav", selected = store$module_ids$analysis$results)
        close_popup(session = session)

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

