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
      outputId = ns('analysis_results_button_download'),
      label = 'Download R script',
      style = 'max-width: 300px'
    ),
    downloadButton(
      outputId = ns('settings_log_download'),
      label = 'Download log',
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

    # reproducible script
    # TODO: this hasn't been thoroughly tested
    reproducible_script <- reactive({

      # TODO: these probably should be stored in realtime and then extracted here
      # this would prevent issues if user goes back and changes something but doesn't save it

      # browser()

      # file inputs
      uploaded_file_name <- store$analysis$data$filename
      uploaded_file_type <-  tools::file_ext(uploaded_file_name)
      uploaded_file_header <- store$analysis$data$header
      uploaded_file_delim <- store$analysis$data$delim

      # get the selected columns and names
      selected_columns <- colnames(store$analysis$data$col_assignment_df)
      column_names <- colnames(store$analysis$verify$user_modified_df)

      # data type changes
      change_data_type <- store$analysis$data$group$group_list

      # eda
      descriptive_plot <-
        if(!is.null(dim(store$analysis$eda$downloaded_descriptive_plot_parameters))){
          as.data.frame(store$analysis$eda$downloaded_descriptive_plot_parameters) %>% distinct()
        }else{
          NULL
        }

      # overlap
      overlap_plot <-
        if(!is.null(dim(store$analysis$eda$downloaded_overlap_plot_parameters))){
          as.data.frame(store$analysis$eda$downloaded_overlap_plot_parameters) %>% distinct()
        }else{
          NULL
        }

      # balance
      balance_plot <-
        if(!is.null(dim(store$analysis$eda$downloaded_balance_plot_parameters))){
          as.data.frame(store$analysis$eda$downloaded_balance_plot_parameters) %>% distinct()
        }else{
          NULL
        }

      # model
      common_support_rule <- store$analysis$model$analysis_over_ride_common_support
      if (store$analysis$model$analysis_model_support == 'No') common_support_rule <- 'none'

      ran_eff <- store$analysis$model$analysis_random_intercept
      if (is.null(ran_eff)) ran_eff <- NA
      estimand <- base::tolower(store$analysis$model$analysis_model_estimand)
      if (is.null(estimand)) estimand <- NA

      BART_model <-
        if(isTRUE(store$analysis$model$fit_good)){ # if a model successfully fitted

          if(!is.null(store$analysis$model$analysis_model_moderator_vars)){ # if moderators are specified
            data.frame(support = common_support_rule,
                       ran.eff = ran_eff,
                       estimand = estimand,
                       moderators = store$analysis$model$analysis_model_moderator_vars
            )
          }else{
            data.frame(support = common_support_rule,
                       ran.eff = ran_eff,
                       estimand = estimand,
                       moderators = NA
            )
          }

        }else{
          NULL
        }

      # create the script
      reproducible_script <- create_script(
        uploaded_file_name = uploaded_file_name,
        uploaded_file_type = uploaded_file_type,
        uploaded_file_header = uploaded_file_header,
        uploaded_file_delim = uploaded_file_delim,
        selected_columns = selected_columns,
        column_names = column_names,
        change_data_type = change_data_type,
        descriptive_plot = descriptive_plot,
        overlap_plot = overlap_plot,
        balance_plot = balance_plot,
        BART_model = BART_model
      )

      return(reproducible_script)
    })

    # download reproducible script
    output$analysis_results_button_download <- downloadHandler(
      filename <- function() {
        time <- gsub("-|:| ", "", Sys.time())
        paste0(time, '_thinkCausal_script.zip')
      },
      content <- function(filename){

        # prevent download if model is not yet fit
        validate_model_fit(store)

        # go to a temp dir to avoid permission issues
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL

        # create README
        fileConn <- file("README.txt")
        writeLines(create_script_readme(), fileConn)
        close(fileConn)
        files <- c('README.txt', files)

        # write function code to individual files
        functions <- c(
          'clean_auto_convert_logicals',
          'clean_dummies_to_categorical',
          'plot_exploration',
          'clean_detect_column_types',
          'clean_confounders_for_bart'
        )
        files <- write_function_files(files, functions)

        # create the script file
        fileConn <- file("thinkCausal_script.R")
        writeLines(reproducible_script(), fileConn)
        close(fileConn)
        files <- c('thinkCausal_script.R', files)

        # create the zip file
        zip(filename, files)
      }
    )

  })
}

## To be copied in the UI
# mod_reproduce_ui("reproduce_1")

## To be copied in the server
# mod_reproduce_server("reproduce_1")
