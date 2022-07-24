#' analysis_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_upload_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::box(
        width = 3,
        collapsible = FALSE,
        title = "Upload your data",
        HTML(
          "<p>Data should be rectangular and in wide format where each column represents one variable.</p>
                        <p> Files can be uploaded from .csv, .txt, Excel (.xlsx), SPSS (.sav) or STATA (.dta) formats.</p>"
        ),
        div(
          id = "upload_file_div",
          fileInput(
            inputId = ns("analysis_upload_data_upload"),
            label = "Choose file:",
            buttonLabel = 'Browse',
            multiple = FALSE,
            accept = c('.csv', '.txt', '.xlsx', '.sav', '.dta'),
            placeholder = 'csv, txt, xlsx, sav, or dta'
          ),
        ),
        conditionalPanel(
          condition = "output.show_delim == true",
          ns = ns,
          radioButtons(
            inputId = ns('analysis_upload_data_delim_value'),
            label = "Column delimiter:",
            selected = ' ',
            choices = list('[space]' = ' ', '[tab]' = '\t', ',', '|', '-', ':'),
            inline = FALSE
          )
        ),
        HTML('<details><summary>Advanced options</summary>'),
          checkboxInput(
            inputId = ns("analysis_upload_data_header"),
            label = "Data contains a header row",
            value = TRUE
          ),
          checkboxInput(
            inputId = ns("analysis_upload_test_data"),
            label = "Use lalonde test data",
            value = FALSE
          ),
        HTML('</details><br>'),
        actionButton(inputId = ns('analysis_upload_help'),
                     label = 'Help me'),
        actionButton(
          inputId = ns('analysis_upload_data_button_columnAssignSave'),
          class = 'nav-path',
          label = 'Save role assignments'
        )
      ),
      bs4Dash::box(
        width = 9,
        collapsible = FALSE,
        title = 'Variable selection',
        uiOutput(
          outputId = ns('analysis_upload_data_UI_dragdrop')
        )
      )
    )
  )
}

#' analysis_upload Server Functions
#'
#' @noRd
mod_analysis_upload_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_upload_help, {
      open_help_sidebar(store, 'Data')
    })

    # read in the uploaded file
    uploaded_df <- reactive({

      if (input$analysis_upload_test_data){

        uploaded_file <- readr::read_csv(
          file = app_sys('extdata', 'lalonde.csv', mustWork = TRUE),
          col_names = TRUE
        )

      } else {

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

      }

      # clean column names; bad names will crash the server
      colnames(uploaded_file) <- clean_names(colnames(uploaded_file))

      return(uploaded_file)
    })

    # add dataframe to store object
    observeEvent(uploaded_df(), {

      # remove any previous dataframes from the store
      store$analysis_data_uploaded_df <- NULL
      store <- remove_downstream_data(store, page = 'upload')

      # stop here if uploaded data seems invalid
      validate(need(
        ncol(uploaded_df()) > 1,
        'Uploaded dataframe only has one column. Is the delimiter correct?'
      ))

      # add to log
      log_event <- paste0('Uploaded ', input$analysis_upload_data_upload$name)
      store$log <- append(store$log, log_event)

      # add to store
      store$analysis$data$filename <- input$analysis_upload_data_upload$name
      store$analysis$data$header <- input$analysis_upload_data_header
      store$analysis$data$delim <- input$analysis_upload_data_delim_value

      # retrieve the raw uploaded data frame
      uploaded_df <- uploaded_df()

      # auto convert all of the logical columns
      auto_cleaned_df <- clean_auto_convert_logicals(uploaded_df)

      # auto convert columns that are integers and few levels to factors
      auto_cleaned_df <- clean_auto_convert_integers(auto_cleaned_df)

      # add to store
      store$analysis_data_uploaded_df <- auto_cleaned_df
    })

    # render the drag and drop UI
    output$analysis_upload_data_UI_dragdrop <- renderUI({

      validate_design(store)
      validate_data_uploaded(store)

      # render the drag-drop UI
      drag_drop_html <- create_drag_drop_roles(ns = ns,
                                               .data = store$analysis_data_uploaded_df,
                                               ns_prefix = 'analysis_upload_data',
                                               design = store$analysis_design_design,
                                               weights = store$analysis_design_weights,
                                               ran_eff = store$analysis_design_random_effects)

      return(drag_drop_html)
    })

    # create new dataframe when user saves column assignments and move to next page
    observeEvent(input$analysis_upload_data_button_columnAssignSave, {

      validate_design(store)
      req(store$analysis_data_uploaded_df)

      # remove any previous dataframes from the store
      store <- remove_downstream_data(store, page = 'upload')

      # get user inputs
      cols_z <- input$analysis_upload_data_dragdrop_treatment
      cols_y <- input$analysis_upload_data_dragdrop_response
      cols_x <- input$analysis_upload_data_dragdrop_covariates
      if(store$analysis_design_design != "Block randomized treatment") cols_block <- NULL
      else cols_block <- input$analysis_upload_data_dragdrop_block
      if(store$analysis_design_weights != 'Yes') cols_weight <- NULL
      else cols_weight <- input$analysis_upload_data_dragdrop_weight
      if (store$analysis_design_random_effects != 'Yes') cols_ran_eff <- NULL
      else cols_ran_eff <- input$analysis_upload_data_dragdrop_ran_eff

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

    return(store)
  })
}
