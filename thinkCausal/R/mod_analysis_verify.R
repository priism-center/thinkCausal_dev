#' analysis_verify UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_verify_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::box(
        width = 3,
        collapsible = FALSE,
        title = "Rename and verify your data",
        p("Ensure your data is named and the data types are correct."),
        htmlOutput(outputId = ns('analysis_verify_data_text_na')),
        br(),
        actionButton(
          inputId = ns('analysis_verify_data_button_reset'),
          label = 'Reset variable changes'
        ),
        actionButton(inputId = ns('analysis_verify_help'),
                     label = 'Help me'),
        actionButton(
          inputId = ns('analysis_verify_data_save'),
          class = "nav-path",
          label = 'Save & continue'
        )
      ),
      bs4Dash::box(
        width = 9,
        collapsible = FALSE,
        title = 'Column types and names',
        wellPanel(style = "overflow-y:scroll; max-height: 400px; background-color: transparent; padding: 15px 15px 0 15px;",
                  uiOutput(outputId = ns(
                    'analysis_verify_data_modify_UI'
                  )),
                  div(class = 'bottom-shadow')),
        hr(style = "height: 1px; background-color: #bfbfbf"),
        h4("Your data", style = "padding: 0; margin: 0"),
        wellPanel(style = "background-color: transparent;",
                  reactable::reactableOutput(outputId = ns('analysis_verify_data_table')))
      )
    )
  )
}

#' analysis_verify Server Functions
#'
#' @noRd
mod_analysis_verify_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_verify_help, {
      open_help_sidebar(store, 'Verify data')
    })

    # maintain a user modified dataframe that is continuously updated
    observe({
      # stop here if data hasn't been uploaded and columns assigned
      validate_columns_assigned(store)

      # stop here if analysis_verify_data_modify_UI hasn't been rendered yet
      req(input$analysis_verify_data_1_changeDataType)

      # use assigned dataframe as the template
      # user_modified_df <- store$analysis$data$col_assignment_df
      user_modified_df <- store$analysis_data_assigned_df

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
      req(all(new_data_types %in% c('Categorical', 'Binary', 'Continuous')))
      user_modified_df <- convert_data_types(.data = user_modified_df, new_data_types = new_data_types)

      # save the data to the store
      store$analysis$verify$user_modified_df <- user_modified_df
    })

    # reset dataframe back to original when user clicks button
    observeEvent(input$analysis_verify_data_button_reset, {

      # reset dataframe
      # store$analysis$verify$user_modified_df <- store$analysis$data$col_assignment_df
      store$analysis$verify$user_modified_df <- store$analysis_data_assigned_df

      ## reset UI
      # set indices to map over
      # all_col_names <- colnames(store$analysis$data$col_assignment_df)
      # default_data_types <- convert_data_type_to_simple(store$analysis$data$col_assignment_df)
      all_col_names <- colnames(store$analysis_data_assigned_df)
      default_data_types <- convert_data_type_to_simple(store$analysis_data_assigned_df)
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

      # data to derive table from
      # .data <- store$analysis$data$col_assignment_df
      .data <- store$analysis_data_assigned_df

      # get default data types
      default_data_types <- convert_data_type_to_simple(.data)

      # add default column types to store
      store$current_simple_column_types <- default_data_types

      # create UI table
      UI_table <- create_data_summary_grid(
        ns = ns,
        .data = .data,
        default_data_types = default_data_types,
        ns_prefix = 'analysis_verify_data',
        design = store$analysis_design_design,
        random_effect = store$column_assignments$ran_eff,
        survey_weight = store$column_assignments$weight,
        blocking_variables = store$column_assignments$blocks
      )

      return(UI_table)
    })

    # update percentNAs fields with actual data
    observeEvent(store$analysis$verify$user_modified_df, {

      # stop here if columns haven't been assigned and grouped
      validate_columns_assigned(store)

      # original data column indices
      indices <- seq_along(colnames(store$analysis$verify$user_modified_df))

      # update the percent NA
      lapply(X = indices, function(i) {
        percent_NA_num <- mean(is.na(store$analysis$verify$user_modified_df[[i]]))
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
    output$analysis_verify_data_table <- reactable::renderReactable({

      # stop here if columns haven't been assigned and grouped
      validate_columns_assigned(store)

      # stop here if analysis_verify_data_modify_UI hasn't yet rendered
      # this works because colnames of user_modified_df is overwritten by the UI
      # which defaults to an empty string before it renders
      validate(need(
        unique(colnames(store$analysis$verify$user_modified_df)) != '',
        'Loading...'
      ))

      # create JS datatable
      tab <- store$analysis$verify$user_modified_df %>%
        na.omit() %>%
        reactable::reactable()

      return(tab)
    })

    # render the text indicating how many rows are being removed due to NAs
    output$analysis_verify_data_text_na <- renderUI({

      # stop if previous steps aren't completed
      req(store$analysis$verify$user_modified_df)

      # calculate stats
      n_rows_original <- nrow(store$analysis$verify$user_modified_df)
      n_rows_removed <- n_rows_original - nrow(na.omit(store$analysis$verify$user_modified_df))
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

    # when user hits 'save column assignments', create a new dataframe from store$$uploaded_df
    # with the new columns
    # create updated options for plotting and modeling pages
    observeEvent(input$analysis_verify_data_save, {
      req(store$analysis$verify$user_modified_df)

      # remove saved dataframes if they exist
      store <- remove_downstream_data(store, 'verify')

      # new column names
      old_col_names <- colnames(store$analysis$verify$user_modified_df)
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
      store$verified_df <- store$analysis$verify$user_modified_df
      colnames(store$verified_df) <- new_col_names

      # remove rows with NAs
      store$verified_df <- na.omit(store$verified_df)

      # save the column names by their respective class
      store$column_types <- clean_detect_column_types(store$verified_df)
#
#       # fit BART model
#       show_popup_fitting_BART_waiting(session)
#
#       bart_model <- fit_bart(
#         .data = store$verified_df,
#         .weights = store$column_assignments$weight,
#         ran_eff = store$column_assignments$ran_eff,
#         .estimand = 'ate', # estimand handled latter in post processing
#         rct = store$analysis_select_design == 'Completely Randomized Experiment'
#       )
#       store$analysis$model$model <- bart_model
#
#       # check common support
#       store$analysis$model$overlap_checks <- check_overlap_rules(.model = bart_model)
#
#
#       # close the alert
#       # shinyWidgets::closeSweetAlert()
#       close_popup(session = session)


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

      # add to log
      log_event <- paste0(
        'Ran BART model with following specification: \n',
        # '\t', 'Experiment design: ', input$analysis_model_radio_design, '\n',
        '\t', 'Causal estimand: ', input$analysis_model_estimand, '\n',
        '\t', 'Remove observations without overlap: ', input$analysis_model_support, '\n',
        '\t', 'Moderators: ', paste0(input$analysis_model_moderator_vars, collapse = "; "), '\n',
        # '\t', 'Model outcome: ', input$analysis_model_outcome, '\n',
        # '\t', 'Propensity score fit: ', input$analysis_model_pscore, '\n',
        '\t', 'Good model fit: ', store$analysis$model$fit_good
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
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_visualize')
    })

    # update moderator options
    observeEvent(input$analysis_verify_data_save, {
      cols_categorical <- store$column_types$categorical
      cols_continuous <- store$column_types$continuous
      X_cols_categorical <- grep("^X_", cols_categorical, value = TRUE)
      X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
      cols_categorical_cleaned <- gsub("X_", '', X_cols_categorical)
      cols_continuous_cleaned <- gsub("X_", '', X_cols_continuous)
      updateSelectInput(session = store$session_global, inputId = "plotBart_waterfall_order",
                        choices = c("ICATE", cols_continuous_cleaned))
      updateSelectInput(session = store$session_global, inputId = "plotBart_waterfall_color",
                        choices = c("None", cols_categorical_cleaned))
      updateSelectInput(session = store$session_global, inputId = "plotBart_ICATE_color",
                        choices = c("None", cols_categorical_cleaned))
      moderator_options <- gsub("X_", '', names(store$verified_df)[3:length(names(store$verified_df))])
      updateSelectInput(session = store$session_global, inputId = "plotBart_moderator_vars",
                        choices = c('', moderator_options))
    })

    # save into store for reproducible script
    # group_list_data <- reactive(group_list$data)
    analysis_verify_data_upload_name <- reactive(input$analysis_verify_data_upload$name)
    analysis_verify_data_header <- reactive(input$analysis_verify_data_header)
    analysis_verify_data_delim_value <- reactive(input$analysis_verify_data_delim_value)
    analysis_verify_data_save <- reactive(input$analysis_verify_data_save)

    observeEvent(input$analysis_verify_data_save, {
      #store$analysis$data$group$group_list <- group_list_data()
      # store$analysis$data$upload$analysis_verify_data_upload$name <- analysis_verify_data_upload_name()
      # store$analysis$data$upload$analysis_verify_data_header <- analysis_verify_data_header()
      # store$analysis$data$upload$analysis_verify_data_delim_value <- analysis_verify_data_delim_value()
      store$analysis$data$verify$analysis_verify_data_save <- analysis_verify_data_save()
    })

    return(store)

  })
}
