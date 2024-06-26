#' analysis_design UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_variable_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
    # column(
    #   width = 3,
    #   bs4Dash::box(
    #     width = 12,
    #     collapsible = FALSE,
    #     title = "Variable Selection",
    #     actionButton(
    #       inputId = ns('analysis_select_help'),
    #       label = 'Help me'
    #     ),
    #     actionButton(
    #       inputId = ns('analysis_select_button_columnAssignSave'),
    #       class = 'nav-path',
    #       label = 'Save variable selection & continue'
    #     )
    #   )
    # ),
    column(
      width = 12,
      bs4Dash::box(
        width = 12,
        collapsible = TRUE,
        title = 'Select covariates',
        uiOutput(outputId = ns('analysis_select_UI_dragdrop')),
        checkboxInput(
          inputId = ns('analysis_select_include_all'),
          label = 'Move all covariates to include box',
          value = FALSE
        ),
        actionButton(
          inputId = ns('analysis_select_help'),
          label = 'Help me'
        ),
        actionButton(
          inputId = ns('analysis_select_button_columnAssignSave'),
          class = 'nav-path',
          label = 'Save variable selection & continue'
        )
      )
    )
  )
  )

}

#' analysis_design Server Functions
#'
#' @noRd
mod_analysis_variable_selection_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_select_help, {
      open_help_sidebar(store, 'Data')
    })

    output$weightOption <- reactive({
      return(input$analysis_weights == 'Yes')
    })
    outputOptions(output, 'weightOption', suspendWhenHidden=FALSE)

    output$ran_effOption <- reactive({
      return(input$analysis_random_effects == 'Yes')
    })
    outputOptions(output, 'ran_effOption', suspendWhenHidden=FALSE)


   # work in progress
   # observeEvent(input$analysis_select_outcome, {
   #   req(input$analysis_select_outcome)
   #    passed <- clean_eval_outcome(store$analysis_data_uploaded_df[[input$analysis_select_outcome]]) == 'pass'
   #    if(passed){
   #      shinyjs::addClass("analysis_select_outcome", class = "success-input")
   #    }
   # })


  defaults <- reactiveValues(.default_blocks = NULL,
                               .default_random_effects = NULL,
                               .default_weight = NULL)

    observeEvent(input$analysis_design, {
      defaults$.default_blocks <- NULL
      defaults$.default_random_effects = NULL
      defaults$.default_weight = NULL
    })

    # render covariate info
    # output$covariate_selection_info <- renderUI({
    #   #validate(need(input$analysis_design != '', "Before selecting covariates, you'll need to provide information about the study design."))
    #   tagList(
    #   HTML("<details open><summary><b>Information about variable selection</b></summary>"),
    #   p('For observational studies, include all potential confounders in the analysis.'),
    #   p('If you are unsure whether or not a variable is a confounder, it is recommended to assume that it is a confounder.'),
    #   p('Colinearity between preditors is not problematic for treatment effect estimation.'),
    #   p('Do not include post-treatment variables as covariates in the analysis.'),
    #   #includeMarkdown('inst/app/www/md/go_to_post_treatment.md'),
    #   HTML("</details>"),
    #   checkboxInput(
    #     inputId = ns('analysis_select_include_all'),
    #     label = 'Move all covariates to include box',
    #     value = FALSE
    #   )
    #   )
    # })

    observeEvent(input$analysis_select_dragdrop_random_effects,{
      defaults$.default_random_effects <- store$column_assignments$ran_eff
    })

    observeEvent(input$analysis_select_dragdrop_blocks,{
      defaults$.default_blocks <- store$column_assignments$blocks
    })

    observeEvent(input$analysis_select_dragdrop_weight,{
      defaults$.default_weight <- store$column_assignments$weight
    })

    # render the drag and drop UI
    output$analysis_select_UI_dragdrop <- renderUI({
      #req(input$analysis_design)
      .exclude <- c(store$column_assignments$y,
                    store$column_assignments$z)
      drag_drop_html <- create_drag_drop_roles(ns = ns,
                                               .data = store$analysis_data_uploaded_df,
                                               z = store$column_assignments$z,
                                               y = store$column_assignments$y,
                                               ns_prefix = 'analysis_select',
                                               exclude = .exclude,
                                               include_all = input$analysis_select_include_all,
                                               blocks = if(isTRUE(store$analysis_select_design == 'Block randomized treatment')) TRUE else FALSE,
                                               default_blocks = if(isTRUE(store$analysis_select_design == 'Block randomized treatment')) defaults$.default_blocks else NULL,
                                               random_effect = if(isTRUE(store$column_assignments$ran_eff == 'Yes')) TRUE else FALSE,
                                               default_random_effects = defaults$.default_random_effects,
                                               weight = if(isTRUE(store$column_assignments$weight == 'Yes')) TRUE else FALSE,
                                               default_weight = if(isTRUE(store$column_assignments$weight == 'Yes')) defaults$.default_weight else NULL)

      return(drag_drop_html)
    })


    # reactive function to run checks over selected varaibles
    check_variable_assignment <- reactive({
      # remove any previous dataframes from the store
      store <- remove_downstream_data(store, page = 'select')

      # get user inputs
      cols_x <- input$analysis_select_dragdrop_covariates
      if(isTRUE(store$column_assignments$weight != 'Yes')) cols_weight <- NULL
      else cols_weight <- input$analysis_dragdrop_weight
      if (isTRUE(store$column_assignments$ran_eff != 'Yes')) cols_ran_eff <- NULL
      else cols_ran_eff <- input$analysis_select_dragdrop_random_effects
      # the order of this is very important for create_data_summary_grid.R
      all_cols <- unlist(c(store$column_assignments$z, store$column_assignments$y, cols_ran_eff, cols_weight,  store$column_assignments$blocks, cols_x))
      # are there duplicate selections?
      all_unique <- isTRUE(length(all_cols) == length(unique(all_cols)))
      #z_is_only_one <- length(cols_z) == 1
      #y_is_only_one <- length(cols_y) == 1
      x_more_than_zero <- length(cols_x) > 0

      # is treatment binary?
      # is_treatment_binary <- tryCatch({
      #   treatment <- store$analysis_data_uploaded_df[[cols_z[1]]]
      #   is_binary <- isTRUE(clean_detect_logical(treatment))
      #   if (isFALSE(is_binary)) {
      #     is_binary <- clean_detect_binary(treatment)
      #     if (isTRUE(is_binary)) {
      #       # get user entered level
      #       treatment_level <- input$treatment_level
      #       # clean it up to match df
      #       treatment_level <-
      #         gsub(
      #           pattern = paste(input$analysis_select_treatment, '= '),
      #           replacement = '',
      #           treatment_level
      #         )
      #     }
      #   } else{
      #     treatment_level <- NULL
      #   }
      #
      #   is_binary
      # },
      # error = function(e) FALSE,
      # warning = function(w) FALSE
      # )

      # is response continuous or binary?
      # is_response_cont_binary <- tryCatch({
      #   response <- store$analysis_data_uploaded_df[[cols_y[[1]]]]
      #   is_cont_binary <- clean_detect_continuous_or_logical(response)
      #   if(isFALSE(is_cont_binary)){
      #     is_cont_binary <- clean_detect_binary(response)
      #     if (isTRUE(is_cont_binary)) {
      #       # get user entered level
      #       outcome_level <- input$outcome_level
      #       # clean it up to match df
      #       outcome_level <-
      #         gsub(
      #           pattern = paste(input$analysis_select_outcome, '= '),
      #           replacement = '',
      #           outcome_level
      #         )
      #     }
      #   }else{
      #     outcome_level <- NULL
      #   }
      #   is_cont_binary
      # },
      # error = function(e) FALSE,
      # warning = function(w) FALSE
      # )

      # is blocking variable categorical?
      # is_block_categorical <- TRUE
      # if (input$analysis_design == 'Block randomized treatment'){
      #   is_block_categorical <- purrr::map_lgl(input$analysis_upload_data_dragdrop_blocks, function(var){
      #     is_cat_or_logical(store$analysis_data_uploaded_df[[var]])
      #   })
      #   is_block_categorical <- all(is_block_categorical)
      # }

      # did it pass all checks?
      all_good <- isTRUE(all(
        c(
          all_unique,
          #z_is_only_one,
          #y_is_only_one,
          x_more_than_zero
          #is_treatment_binary,
          #is_response_cont_binary,
          #is_block_categorical
        )
      ))

      # launch error message
      if (!all_good) show_popup_variable_assignment_warning(session)

      validate(need(all_good, "There is an issue with column assignment"))

      # save design info
      store$analysis_select_weights <- input$analysis_weights
      store$analysis_select_random_effects <- input$analysis_random_effects

      # store the new dataframe using the uploaded df as the template
      store$analysis_data_assigned_df <- store$analysis_data_uploaded_df[, all_cols]

      # save columns assignments
      store$column_assignments$x <- NULL
      store$column_assignments$weight <- NULL
      store$column_assignments$ran_eff <- NULL

      #store$column_assignments$z <- cols_z
      #store$column_assignments$y <- cols_y
      store$column_assignments$x <- cols_x
      store$column_assignments$weight <- cols_weight
      store$column_assignments$ran_eff <- cols_ran_eff
      #store$column_assignments$blocks <- cols_block

      # add to log
      log_event <- paste0('Assigned columns to roles: ',
                          '\n\tcovariates: ', paste0(cols_x, collapse = '; '),
                          '\n\tsurvey weight: ', cols_weight,
                          '\n\trandom intercepts: ', paste0(cols_ran_eff, collapse = '; '))
      #,'\n\tblocking variable(s): ', paste0(cols_block, collapse = '; '))
      store$log <- append(store$log, log_event)

    })



    # create new dataframe when user saves column assignments and move to next page
    observeEvent(input$analysis_select_button_columnAssignSave, {
      req(store$analysis_data_uploaded_df)
      pass_variable <- reactiveVal(length(input$analysis_select_dragdrop_avalable) == 0)
      # check that all predictors are included, if not launch popup
      if (isFALSE(pass_variable()) & isTRUE(store$analysis_design == "Observational Study (Treatment not Randomized)")) {
        show_popup_variable_selection_warning(x = length(input$analysis_select_dragdrop_avalable),
                                              session, ns = ns)

      }

      validate(need(pass_variable(), ''))
      check_variable_assignment()
      # move to next page
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_verify')

    })

    ## pop up buttons if variable selection warning is activated.
    ## This means a user has tried to proceed without including all variables in the analysis.
    # first button adjust variables
    observeEvent(input$analysis_model_variable_selection_popup_stop, {
      close_popup(session = session)
    })

    # second button continue and override warning
    observeEvent(input$analysis_model_variable_selection_popup_continue, {
      close_popup(session = session)
      check_variable_assignment()
      # move to next page
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_verify')

    })

    # third button move to learn post-treatment page
    observeEvent(input$analysis_model_variable_selection_popup_posttreatment, {
      close_popup(session = session)
      store$analysis_origin <- 'analysis_select'
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'learn_post_treatment')
    })

  })
  return(store)
}
