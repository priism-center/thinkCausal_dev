#' reproduce UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reproduce_ui <- function(id){
  ns <- NS(id)
  tagList(

    downloadButton(
      outputId = ns('settings_log_download'),
      label = 'Download log',
      style = 'max-width: 300px'
    ),
    downloadButton(
      outputId = ns('analysis_results_button_download'),
      label = 'Download R script',
      style = 'max-width: 300px'
    ),
    br(), br(),
    bs4Dash::box(
      width = 8,
      collapsible = FALSE,
      title = 'Log',
      verbatimTextOutput(
        outputId = ns('settings_log_text')
      )
    )
  )
}

#' reproduce Server Functions
#'
#' @noRd
mod_reproduce_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # print the log
    # the log is created by appending text descriptions of events to store$log
    output$settings_log_text <- renderText({
      log <- store$log
      if (length(log) == 0) log <- "No logged events to display"
      log <- paste0(log, collapse = '\n\n')
      return(log)
    })

    # download the log
    output$settings_log_download <- downloadHandler(
      filename <- function() {
        time <- gsub("-|:| ", "", Sys.time())
        paste0(time, '_thinkCausal_log.txt')
      },
      content <- function(filename){
        fileConn <- file(filename)
        log <- paste0(paste0(store$log, collapse = '\n\n'), "\n")
        writeLines(log, fileConn)
        close(fileConn)
      }
    )

    output$analysis_results_button_download <- downloadHandler(
      filename <- function() {
        'thinkCausal.zip'
        # paste("output", "zip", sep=".")
      },

      content <- function(filename) {

        req(store$analysis$model$fit_good)

        # go to a temp dir to avoid permission issues
        owd <- setwd(tempdir())
        on.exit(setwd(owd))

        # browser()
        # copy over template files
        files <- list.files(app_sys('reproduce'), full.names = TRUE)
        file.copy(files, '.', recursive = TRUE)
        files_new <- list.files()

        # remove extraneous files

        # modify templates
        edit_reproducible_script(files_new, store)

        # zip it
        utils::zip(filename, file = files_new)#, extras = '-j')
      },
      contentType = "application/zip"
    )

    # download reproducible script
    # output$analysis_results_button_download <- downloadHandler(
    #   filename <- function() {
    #     time <- gsub("-|:| ", "", Sys.time())
    #     paste0(time, '_thinkCausal_script.zip')
    #   },
    #   content <- function(filename){
    #
    #     # prevent download if model is not yet fit
    #     validate_model_fit(store)
    #
    #     # go to a temp dir to avoid permission issues
    #     owd <- setwd(tempdir())
    #     on.exit(setwd(owd))
    #     files <- NULL
    #
    #     zip(app_sys('inst/reproduce'))
    #
    #     # copy directory
    #
    #
    #     # edit file
    #
    #
    #     # zip it
    #
    #     # # create README
    #     # fileConn <- file("README.txt")
    #     # writeLines(create_script_readme(), fileConn)
    #     # close(fileConn)
    #     # files <- c('README.txt', files)
    #     #
    #     # # write function code to individual files
    #     # functions <- c(
    #     #   'clean_auto_convert_logicals',
    #     #   'clean_dummies_to_categorical',
    #     #   'plot_exploration',
    #     #   'clean_detect_column_types',
    #     #   'clean_confounders_for_bart'
    #     # )
    #     # files <- write_function_files(files, functions)
    #
    #     # # create the script file
    #     # fileConn <- file("thinkCausal_script.R")
    #     # writeLines(reproducible_script(), fileConn)
    #     # close(fileConn)
    #     # files <- c('thinkCausal_script.R', files)
    #
    #     # create the zip file
    #     # zip(filename, files)
    #   }
    # )

  })
}

## To be copied in the UI
# mod_reproduce_ui("reproduce_1")

## To be copied in the server
# mod_reproduce_server("reproduce_1")
