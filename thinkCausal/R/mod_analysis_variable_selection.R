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
  tagList(fluidRow(
    column(
      width = 3,
      bs4Dash::box(
        width = 12,
        collapsible = FALSE,
        title = "Variable Selection",
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
    ),
    column(
      9,
      bs4Dash::box(
        collapsible = FALSE,
        width = 12,
        title = textOutput(outputId = ns("specify_title")),
        uiOutput(outputId = ns("readySpecifyVariables")),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns('analysis_select_outcome'),
              label = 'Select the outcome variable from you data',
              choices = NULL,
              selected = NULL
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = ns('analysis_select_treatment'),
              label = 'Select the treatment variable from you data',
              choices = NULL,
              selected = NULL
            )
          )
        )
      ),
      bs4Dash::box(
        width = 12,
        collapsible = FALSE,
        title = 'Select covariates',
        uiOutput(outputId = ns("readyCovariates")),
        conditionalPanel(
          condition = "output.coeffReady == true",
          ns = ns,
          HTML("<details open><summary><b>Information about variable selection</b></summary>"),
          p('For observational studies, include all potential confounders in the analysis.'),
          p('If you are unsure whether or not a variable is a counfounder, it is recomended to assume that it is a confounder.'),
          p('Co-linearity between variables is not a problem in causal inference.'),
          p('Do not include post-treatment variables as covariates in the analysis.'),
          #includeMarkdown('inst/app/www/md/go_to_post_treatment.md'),
          HTML("</details>"),
          checkboxInput(
            inputId = ns('analysis_select_include_all'),
            label = 'Move all covariates to include box',
            value = FALSE
          ),
          uiOutput(outputId = ns('analysis_select_UI_dragdrop'))
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

    message_argument_title <- reactive({
      if(is.null(store$analysis_design_random_effects) | is.null(store$analysis_design_weights )){
        out <- 'Select variables'
      } else if(store$analysis_design_random_effects != 'Yes' & store$analysis_design_weights != 'Yes'){
        out <- 'Select outcome and treatment'
      } else if(store$analysis_design_random_effects == 'Yes' & store$analysis_design_weights != 'Yes'){
        out <- 'Select the outcome, treatment and random effects'
      } else if(store$analysis_design_random_effects != 'Yes' & store$analysis_design_weights == 'Yes'){
        out <- 'Select outcome, treatment and survey weight variables'
      } else if(store$analysis_design_random_effects == 'Yes' & store$analysis_design_weights == 'Yes'){
        out <- 'Select outcome, treatment, survey weight and random effects'
      } else{
        out <- 'Select variables'
      }

      return(out)
    })

    output$specify_title <- renderText({
      return(message_argument_title())
    })

    output$weightOption <- reactive({
      return(store$analysis_design_weights == 'Yes')
    })
    outputOptions(output, 'weightOption', suspendWhenHidden=FALSE)

    output$ran_effOption <- reactive({
      return(store$analysis_design_random_effects == 'Yes')
    })
    outputOptions(output, 'ran_effOption', suspendWhenHidden=FALSE)

    output$coeffReady <- reactive({
      req_inputs <- sum(2, store$analysis_design_weights == 'Yes', store$analysis_design_random_effects == 'Yes')
      entered_inputs <-
        sum(
          input$analysis_select_outcome != '',
          input$analysis_select_treatment != '',
          input$analysis_select_weights != '',
          input$analysis_select_random_effects != ''
        )

      eval <- sum(req_inputs == entered_inputs)
      return(eval)
    })
    outputOptions(output, 'coeffReady', suspendWhenHidden=FALSE)


    message_argument_variables <- reactive({
      if(store$analysis_design_random_effects != 'Yes' & store$analysis_design_weights != 'Yes'){
        out <- 'Before selecting covariates, specify the outcome and treatment variables.'
      } else if(store$analysis_design_random_effects == 'Yes' & store$analysis_design_weights != 'Yes'){
        out <- 'Before selecting covariates, specify the outcome, treatment and random effects variables.'
      } else if(store$analysis_design_random_effects != 'Yes' & store$analysis_design_weights == 'Yes'){
        out <- 'Before selecting covariates, specify the outcome, treatment and survey weigth variables.'
      } else{
        out <- 'Before selecting covariates, specify the outcome, treatment, survey weigth and random effects variables.'
      }

      return(out)
    })

    output$readySpecifyVariables <- renderUI({
      validate_design(store)
      validate_data_uploaded(store)
    })



    output$readyCovariates <- renderUI({
      validate_design(store)
      validate_data_uploaded(store)

      req_inputs <- sum(2, store$analysis_design_weights == 'Yes', store$analysis_design_random_effects == 'Yes')
      entered_inputs <-
        sum(
          input$analysis_select_outcome != '',
          input$analysis_select_treatment != '',
          input$analysis_select_weights != '',
          input$analysis_select_random_effects != ''
        )

      eval <- req_inputs == entered_inputs
      validate(need(eval, message_argument_variables()))

    })

    # update outcome, treatment, weight and ran_eff options
    observeEvent(store$analysis_data_uploaded_df, {
      browser()
      # update outcome
      updateSelectInput(
        inputId = 'analysis_select_outcome',
        selected = NULL,
        choices = c('',  names(store$analysis_data_uploaded_df))
      )

      updateSelectInput(
        inputId = 'analysis_select_treatment',
        selected = NULL,
        choices = c('', names(store$analysis_data_uploaded_df))
      )
    })

    observeEvent(input$analysis_select_outcome, {
      updateSelectInput(
        inputId = 'analysis_select_treatment',
        selected = NULL,
        choices = c('', names(store$analysis_data_uploaded_df)[names(store$analysis_data_uploaded_df) != input$analysis_select_outcome])
      )
    })

    observeEvent(input$analysis_select_outcome, {
      updateSelectInput(
        inputId = 'analysis_select_weights',
        selected = NULL,
        choices = c('', names(store$analysis_data_uploaded_df)[names(store$analysis_data_uploaded_df) != input$analysis_upload_outcome])
      )
    })

    observeEvent(input$analysis_select_treatment, {
      updateSelectInput(
        inputId = 'analysis_select_weights',
        selected = NULL,
        choices = c('', names(store$analysis_data_uploaded_df)[names(store$analysis_data_uploaded_df) != input$analysis_upload_treatment])
      )
    })

    # render the drag and drop UI
    output$analysis_select_UI_dragdrop <- renderUI({
      .exclude <- c(input$analysis_select_outcome,
                    input$analysis_select_treatment,
                    input$analysis_select_random_effects,
                    input$analysis_select_weights)

      drag_drop_html <- create_drag_drop_roles(ns = ns,
                                               .data = store$analysis_data_uploaded_df,
                                               ns_prefix = 'analysis_select',
                                               exclude = .exclude,
                                               include_all = input$analysis_select_include_all)

      return(drag_drop_html)
    })

    # create new dataframe when user saves column assignments and move to next page
    observeEvent(input$analysis_select_button_columnAssignSave, {

      validate_design(store)
      req(store$analysis_data_uploaded_df)

      # remove any previous dataframes from the store
      store <- remove_downstream_data(store, page = 'select')

      # get user inputs
      cols_z <- input$analysis_select_treatment
      cols_y <- input$analysis_select_outcome
      cols_x <- input$analysis_select_dragdrop_covariates
      if(store$analysis_design_design != "Block randomized treatment") cols_block <- NULL
      else cols_block <- input$analysis_select_data_dragdrop_block
      if(store$analysis_design_weights != 'Yes') cols_weight <- NULL
      else cols_weight <- input$analysis_select_weights
      if (store$analysis_design_random_effects != 'Yes') cols_ran_eff <- NULL
      else cols_ran_eff <- analysis_select_random_effects

      # the order of this is very important for create_data_summary_grid.R
      all_cols <- unlist(c(cols_z, cols_y, cols_ran_eff, cols_weight, cols_block, cols_x))

      # are there duplicate selections?
      all_unique <- isTRUE(length(all_cols) == length(unique(all_cols)))
      z_is_only_one <- length(cols_z) == 1
      y_is_only_one <- length(cols_y) == 1
      x_more_than_zero <- length(cols_x) > 0

      # is treatment binary?
      is_treatment_binary <- tryCatch({
        treatment <- store$analysis_data_uploaded_df[[cols_z[1]]]
        is_binary <- isTRUE(clean_detect_logical(treatment))
        is_binary
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
      )

      # is response continuous or binary?
      is_response_cont_binary <- tryCatch({
        response <- store$analysis_data_uploaded_df[[cols_y[[1]]]]
        is_cont_binary <- clean_detect_continuous_or_logical(response)
        is_cont_binary
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
      )

      # is blocking variable categorical?
      is_block_categorical <- TRUE
      if (store$analysis_design_design == 'Block randomized treatment'){
        is_block_categorical <- purrr::map_lgl(input$analysis_upload_data_dragdrop_block, function(var){
          is_cat_or_logical(store$analysis_data_uploaded_df[[var]])
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
      store$analysis_data_assigned_df <- store$analysis_data_uploaded_df[, all_cols]

      # save columns assignments
      store$column_assignments <- NULL
      store$column_assignments$z <- cols_z
      store$column_assignments$y <- cols_y
      store$column_assignments$x <- cols_x
      store$column_assignments$weight <- cols_weight
      store$column_assignments$ran_eff <- cols_ran_eff
      store$column_assignments$blocks <- cols_block

      # add to log
      log_event <- paste0('Assigned columns to roles: ',
                          '\n\ttreatment: ', cols_z,
                          '\n\tresponse: ', cols_y,
                          '\n\tcovariates: ', paste0(cols_x, collapse = '; '),
                          '\n\tsurvey weight: ', cols_weight,
                          '\n\trandom intercepts: ', paste0(cols_ran_eff, collapse = '; '),
                          '\n\tblocking variable(s): ', paste0(cols_block, collapse = '; '))
      store$log <- append(store$log, log_event)

      # move to next page
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_verify')

    })

  })
  return(store)
}
