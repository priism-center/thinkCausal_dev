#' analysis_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 3,
      bs4Dash::box(
        width = 12,
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
        actionButton(
          inputId = ns('analysis_upload_help'),
          label = 'Help me'
        ),
        actionButton(
          inputId = ns('analysis_upload_data_button_columnAssignSave'),
          class = 'nav-path',
          label = 'Next'
        )
      )
    ),
    column(width = 9,
           fluidRow(column(
             width = 12,
             bs4Dash::box(
               collapsible = FALSE,
               width = 12,
               title = 'Uploaded Data',
               reactable::reactableOutput(ns('analysis_upload_show_data'))
             )
           ))

    )))
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



    output$analysis_upload_show_data <- reactable::renderReactable({
      validate_data_uploaded(store)
      tab <- store$analysis_data_uploaded_df %>%
        na.omit() %>%
        reactable::reactable()
      return(tab)
    })

    # create new dataframe when user saves column assignments and move to next page
    observeEvent(input$analysis_upload_data_button_columnAssignSave, {

      #validate_data_uploaded(store)
      req(store$analysis_data_uploaded_df)

      # remove any previous dataframes from the store
      store <- remove_downstream_data(store, page = 'upload')

      # move to next page
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_describe')

    })

    return(store)
  })
}
