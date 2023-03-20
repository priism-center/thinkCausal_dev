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
        collapsible = TRUE,
        width = 12,
        title = 'Select outcome and treatment',
        uiOutput(ns('outcome_treatment_ui')),
        fluidRow(
          column(width = 6,
        conditionalPanel(
          "output.need_outcome_info == 'show'",
          ns = ns,
          selectInput(
            inputId = ns('outcome_level'),
            label = 'Define success as:',
            choices =  NULL,
            selected = NULL
          )
        )),
        column(width = 6,
        conditionalPanel(
          "output.need_treatment_info == 'show'",
          ns = ns,
          selectInput(
            inputId = ns('treatment_level'),
            label = 'Select a level to respresent the treated group:',
            choices =  NULL,
            selected = NULL
          )
        )))
      ),
      bs4Dash::box(
        width = 12,
        collapsible = TRUE,
        title = 'Study design',
        uiOutput(ns('design_ui'))
      ),
      bs4Dash::box(
        width = 12,
        collapsible = TRUE,
        title = 'Select covariates',
        uiOutput(outputId = ns("covariate_selection_info")),
        uiOutput(outputId = ns('analysis_select_UI_dragdrop'))
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

   output$outcome_treatment_ui <- renderUI({
      validate_data_uploaded(store,
                             message = "You need to upload a dataset on the upload data page before you can select and outcome and treatment variable.")
     fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = ns('analysis_select_outcome'),
            label = 'Select an outcome variable from you data',
            choices = c('', names(store$analysis_data_uploaded_df)),
            selected = NULL
          )
        ),
        column(
          width = 6,
          selectInput(
            inputId = ns('analysis_select_treatment'),
            label = 'Select a treatment variable from you data',
            choices = c('', names(store$analysis_data_uploaded_df)),
            selected = NULL
          )
        )
      )
    })

   # work in progress
   # observeEvent(input$analysis_select_outcome, {
   #   req(input$analysis_select_outcome)
   #    passed <- clean_eval_outcome(store$analysis_data_uploaded_df[[input$analysis_select_outcome]]) == 'pass'
   #    if(passed){
   #      shinyjs::addClass("analysis_select_outcome", class = "success-input")
   #    }
   # })

   # identify if we need user to indicate an outcome level
   output$need_outcome_info <- reactive({
     req(input$analysis_select_outcome)
     ifelse(
       clean_eval_outcome(store$analysis_data_uploaded_df[[input$analysis_select_outcome]]) == 'need information',
       'show',
       'hide'
     )
   })

   outputOptions(output, "need_outcome_info", suspendWhenHidden = FALSE)

   observeEvent(input$analysis_select_outcome, {
     updateSelectInput(inputId = 'outcome_level',
                       choices = c('', paste(input$analysis_select_outcome, '=', unique(store$analysis_data_uploaded_df[[input$analysis_select_outcome]])))
                       )
   })

   # identify if we need user to provide a treatment level
   output$need_treatment_info <- reactive({
     req(input$analysis_select_treatment)
     ifelse(
       clean_eval_treatment(store$analysis_data_uploaded_df[[input$analysis_select_treatment]]) == 'need information',
       'show',
       'hide'
     )
   })

   outputOptions(output, "need_treatment_info", suspendWhenHidden = FALSE)

   observeEvent(input$analysis_select_treatment, {
     updateSelectInput(inputId = 'treatment_level',
                       choices = c('', paste(input$analysis_select_treatment, '=', unique(store$analysis_data_uploaded_df[[input$analysis_select_treatment]])))
     )
   })

   observeEvent(feedback_outcome(), {
     feedback_outcome()
   })

   feedback_outcome <- reactive({
     req(input$analysis_select_outcome)

     isContinuous_Logical <- clean_detect_continuous_or_logical(store$analysis_data_uploaded_df[[input$analysis_select_outcome]])
     isBinary <- clean_detect_binary(store$analysis_data_uploaded_df[[input$analysis_select_outcome]])
     if(isTRUE(isBinary)){
       possibleLevels <- unique(store$analysis_data_uploaded_df[[input$analysis_select_outcome]])
       currentLevel <- gsub(paste(input$analysis_select_outcome,'= '), '', input$outcome_level)
     }else{
       possibleLevels <- TRUE
       currentLevel <- FALSE
     }

     status <- dplyr::case_when(
       isTRUE(isContinuous_Logical) ~ 'pass',
       isTRUE(isBinary) & isFALSE(currentLevel %in% possibleLevels) ~ 'warn',
       isTRUE(isBinary) & isTRUE(currentLevel %in% possibleLevels) ~ 'pass',
       T ~ 'fail'
     )

     shinyFeedback::hideFeedback("analysis_select_outcome")

     if(status == 'pass'){
       shinyFeedback::showFeedbackSuccess(inputId = 'analysis_select_outcome', text = paste(input$analysis_select_outcome, 'is either a continuous or binary variable.'))
     }
     if(status == 'warn'){
       shinyFeedback::showFeedbackWarning(inputId = 'analysis_select_outcome', text = paste(input$analysis_select_outcome, "appears to be a catagorical variable with two levels.<br>Before moving on, you'll need to indicate which level represents a sucess."))

     }
     if(status == 'fail'){
       shinyFeedback::showFeedbackDanger(inputId = 'analysis_select_outcome', text = 'At this point in time, thinkCausal only handles continuous or binary outcomes.<br>In order to continue, select a continuous or binary outcome.')
     }

     if(input$analysis_select_treatment == input$analysis_select_outcome){
       shinyFeedback::showFeedbackDanger(inputId = 'analysis_select_outcome', text = 'The treatment and outcome must be different variables. Select a different treatment or a different outcome before continuing.')
     }

     shinyFeedback::hideFeedback("outcome_level")
     if(currentLevel %in% possibleLevels){
       shinyFeedback::showFeedbackSuccess(inputId = 'outcome_level', text = paste(input$outcome_level, 'will be automatically recoded so that a success = TRUE and a non-success = FALSE.'))
     }

   })



   observeEvent(feedback_treatment(), {
     feedback_treatment()
   })

   feedback_treatment <- reactive({
     req(input$analysis_select_treatment)
      isLogical <- clean_detect_logical(store$analysis_data_uploaded_df[[input$analysis_select_treatment]])
      isBinary <- clean_detect_binary(store$analysis_data_uploaded_df[[input$analysis_select_treatment]])
      if(isTRUE(isBinary)){
        possibleLevels <- unique(store$analysis_data_uploaded_df[[input$analysis_select_treatment]])
        currentLevel <- gsub(paste(input$analysis_select_treatment,'= '), '', input$treatment_level)
      }else{
        possibleLevels <- TRUE
        currentLevel <- FALSE
      }

     status <- dplyr::case_when(
       isTRUE(isLogical) ~ 'pass',
       isTRUE(isBinary) & isFALSE(currentLevel  %in% possibleLevels) ~ 'warn',
       isTRUE(isBinary) & currentLevel %in% possibleLevels ~ 'pass',
       T ~ 'fail'
     )

     shinyFeedback::hideFeedback("analysis_select_treatment")

     if(status == 'pass'){
       shinyFeedback::showFeedbackSuccess(inputId = 'analysis_select_treatment', text = paste(input$analysis_select_treatment, 'is a binary variable.'))
     }
     if(status == 'warn'){
       shinyFeedback::showFeedbackWarning(inputId = 'analysis_select_treatment', text = paste(input$analysis_select_treatment, "appears to be a catagorical variable with two levels.<br>Before moving on, you'll need to indicate which level represents a success."))

     }
     if(status == 'fail'){
       shinyFeedback::showFeedbackDanger(inputId = 'analysis_select_treatment', text = 'At this point in time, thinkCausal only handles binary treatments.<br>In order to continue, select a binary treatment.')
     }

     if(input$analysis_select_treatment == input$analysis_select_outcome){
       shinyFeedback::showFeedbackDanger(inputId = 'analysis_select_treatment', text = 'The treatment and outcome must be different variables. Select a different treatment or a different outcome before continuing.')
     }

     shinyFeedback::hideFeedback("treatment_level")
     if(currentLevel %in% possibleLevels){
       shinyFeedback::showFeedbackSuccess(inputId = 'treatment_level', text = paste(input$analysis_select_treatment, 'will be automatically recoded so that', currentLevel, '= TRUE and', possibleLevels[possibleLevels!=currentLevel],'= FALSE.'))
     }

   })


    output$design_ui <- renderUI({
      validate(need(input$analysis_select_treatment != '' & input$analysis_select_treatment != '',
                    "Before describing the study design, you'll need to finish describing the outcome and treatment variables."))

      if(clean_eval_treatment(store$analysis_data_uploaded_df[[input$analysis_select_treatment]]) == 'need information'){
        validate(need(input$treatment_level != '',
                      "Before describing the study design, you'll need to finish describing the outcome and treatment variables."))
      }

      if(clean_eval_outcome(store$analysis_data_uploaded_df[[input$analysis_select_outcome]]) == 'need information'){
        validate(need(input$outcome_level != '',
                      "Before describing the study design, you'll need to finish describing the outcome and treatment variables."))
      }

        tagList(
      selectInput(
        inputId = ns('analysis_design'),
        label = 'Indicate the study design:',
        choices = c(
          "",
          "Unsure",
          'Observational',
          'Randomized treatment',
          'Block randomized treatment'
        )
      ),
      HTML('<details><summary>Advanced options (random effects & survey weights)</summary>'),
      selectInput(
        ns('analysis_random_effects'),
        label = create_info_icon(
          'Include random effects:',
          'Random effects often account for nested/clustered data: classes within schools or patients within medical practices.'
        ),
        choices = c("No", "Yes")
      ),
      selectInput(
        inputId = ns('analysis_weights'),
        label = create_info_icon(
          'Include survey weights:',
          'Survey weights are used in survey research when samples are not randomly drawn from the population of interest.'
        ),
        choices = c('No', 'Yes')
      ),
      HTML('</details><br>')
        )

    })

  defaults <- reactiveValues(.default_blocks = NULL,
                               .default_random_effects = NULL,
                               .default_weight = NULL)

    observeEvent(input$analysis_design, {
      defaults$.default_blocks <- NULL
      defaults$.default_random_effects = NULL
      defaults$.default_weight = NULL
    })

    # render covariate info
    output$covariate_selection_info <- renderUI({
      validate(need(input$analysis_design != '', "Before selecting covariates, you'll need to provide information about the study design."))
      tagList(
      HTML("<details open><summary><b>Information about variable selection</b></summary>"),
      p('For observational studies, include all potential confounders in the analysis.'),
      p('If you are unsure whether or not a variable is a confounder, it is recommended to assume that it is a confounder.'),
      p('Colinearity between preditors is not problematic for treatment effect estimation.'),
      p('Do not include post-treatment variables as covariates in the analysis.'),
      #includeMarkdown('inst/app/www/md/go_to_post_treatment.md'),
      HTML("</details>"),
      checkboxInput(
        inputId = ns('analysis_select_include_all'),
        label = 'Move all covariates to include box',
        value = FALSE
      )
      )
    })

    observeEvent(input$analysis_select_dragdrop_random_effects,{
      defaults$.default_random_effects <- input$analysis_select_dragdrop_random_effects
    })

    observeEvent(input$analysis_select_dragdrop_blocks,{
      defaults$.default_blocks <- input$analysis_select_dragdrop_blocks
    })

    observeEvent(input$analysis_select_dragdrop_weight,{
      defaults$.default_weight <- input$analysis_select_dragdrop_weight
    })

    # render the drag and drop UI
    output$analysis_select_UI_dragdrop <- renderUI({
      req(input$analysis_design)
      .exclude <- c(input$analysis_select_outcome,
                    input$analysis_select_treatment)

      drag_drop_html <- create_drag_drop_roles(ns = ns,
                                               .data = store$analysis_data_uploaded_df,
                                               ns_prefix = 'analysis_select',
                                               exclude = .exclude,
                                               include_all = input$analysis_select_include_all,
                                               blocks = if(input$analysis_design == 'Block randomized treatment') TRUE else FALSE,
                                               default_blocks = if(input$analysis_design == 'Block randomized treatment') input$analysis_select_dragdrop_blocks else NULL,
                                               random_effect = if(isTRUE(input$analysis_random_effects == 'Yes')) TRUE else FALSE,
                                               default_random_effects = defaults$.default_random_effects,
                                               weight = if(isTRUE(input$analysis_weights == 'Yes')) TRUE else FALSE,
                                               default_weight = if(isTRUE(input$analysis_weights == 'Yes')) input$analysis_select_dragdrop_weight else NULL)

      return(drag_drop_html)
    })

    # create new dataframe when user saves column assignments and move to next page
    observeEvent(input$analysis_select_button_columnAssignSave, {

      req(store$analysis_data_uploaded_df)

      # remove any previous dataframes from the store
      store <- remove_downstream_data(store, page = 'select')

      # get user inputs
      cols_z <- input$analysis_select_treatment
      cols_y <- input$analysis_select_outcome
      cols_x <- input$analysis_select_dragdrop_covariates
      if(input$analysis_design != "Block randomized treatment") cols_block <- NULL
      else cols_block <- input$analysis_select_dragdrop_blocks
      if(input$analysis_weights!= 'Yes') cols_weight <- NULL
      else cols_weight <- input$analysis_dragdrop_weight
      if (input$analysis_random_effects != 'Yes') cols_ran_eff <- NULL
      else cols_ran_eff <- input$analysis_select_dragdrop_random_effects

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
        if (isFALSE(is_binary)) {
          is_binary <- clean_detect_binary(treatment)
          if (isTRUE(is_binary)) {
            # get user entered level
            treatment_level <- input$treatment_level
            # clean it up to match df
            treatment_level <-
              gsub(
                pattern = paste(input$analysis_select_treatment, '= '),
                replacement = '',
                treatment_level
              )
            # recode data
            store$analysis_data_uploaded_df[[cols_z[1]]] <-
              ifelse(store$analysis_data_uploaded_df[[cols_z[1]]] == treatment_level,
                     TRUE,
                     FALSE)
          }
        } else{
          treatment_level <- 'variable is already logical or coded as 1/0'
        }

        is_binary
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
      )

      # is response continuous or binary?
      is_response_cont_binary <- tryCatch({
        response <- store$analysis_data_uploaded_df[[cols_y[[1]]]]
        is_cont_binary <- clean_detect_continuous_or_logical(response)
        if(isFALSE(is_cont_binary)){
          is_cont_binary <- clean_detect_binary(response)
          if (isTRUE(is_cont_binary)) {
            # get user entered level
            outcome_level <- input$outcome_level
            # clean it up to match df
            outcome_level <-
              gsub(
                pattern = paste(input$analysis_select_outcome, '= '),
                replacement = '',
                outcome_level
              )
            # recode data
            store$analysis_data_uploaded_df[[cols_y[1]]] <-
              ifelse(store$analysis_data_uploaded_df[[cols_y[1]]] == outcome_level,
                     TRUE,
                     FALSE)
          }
        }else{
          outcome_level <- 'outcome is continuous'
        }
        is_cont_binary
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
      )

      # is blocking variable categorical?
      is_block_categorical <- TRUE
      if (input$analysis_design == 'Block randomized treatment'){
        is_block_categorical <- purrr::map_lgl(input$analysis_upload_data_dragdrop_blocks, function(var){
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

      # save design info
      store$analysis_select_design <- input$analysis_design
      store$analysis_select_weights <- input$analysis_weights
      store$analysis_select_random_effects <- input$analysis_random_effects

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
                          '\n\tdesign: ', input$analysis_design,
                          '\n\ttreatment: ', cols_z,
                          '\n\ttreatment level:', treatment_level,
                          '\n\tresponse: ', cols_y,
                          '\n\tresponse success:', outcome_level,
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
