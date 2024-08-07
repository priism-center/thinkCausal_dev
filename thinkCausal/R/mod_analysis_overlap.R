#' analysis_overlap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_overlap_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        width = 3,
        collapsible = FALSE,
        title = 'Check overlap',
        selectInput(
          inputId = ns("analysis_overlap_trim"),
          label = 'Check overlap of:',
          choices = c("ATE", 'ATT', 'ATC')
        ),
        selectInput(
          inputId = ns("analysis_overlap_type"),
          label = "View:",
          choices = c("By Propensity Score" = 1,
                      "By Variables" = 2
          ),
          selected = 1
        ),
        conditionalPanel("input.analysis_overlap_type == '1'",
                         ns = ns,
                         checkboxInput(
                           inputId = ns('trim'),
                          label = 'Trim plot:',
                          value = FALSE
                         )
                         ),
        selectInput(
          inputId = ns("analysis_overlap_method"),
          label = "Plot type:",
          choices = c('Histogram', 'Density'),
          selected = 'Histogram'
        ),
        conditionalPanel(condition = "input.analysis_overlap_type == '2'",
                         ns = ns,
                         selectInput(
                           inputId = ns("analysis_overlap_select_var"),
                           label = "Select variables for overlap check:",
                           multiple = FALSE,
                           choices = NULL,
                           selected = NULL
                         )),
        br(),
        actionButton(inputId = ns('analysis_overlap_help'),
                     label = 'What is this plot telling me?'),
        downloadButton(ns('download_overlap_plot'), label = "Download plot"),
        actionButton(
          inputId = ns("analysis_overlap_button_next"),
          class = "nav-path",
          label = "Next"
        )
      ),

      bs4Dash::box(
        width = 9,
        collapsible = FALSE,
        title = 'Overlap plot',
        plotOutput(
          outputId = ns("analysis_overlap_plot"),
          height = 800
        )
      )
    )
  )
}

#' analysis_overlap Server Functions
#'
#' @noRd
mod_analysis_overlap_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_overlap_help, {
      open_help_sidebar(store, 'Overlap')
    })

    # next button
    observeEvent(input$analysis_overlap_button_next, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_model')
    })

    # render default values when verified data is saved
    observeEvent(store$analysis$data$verify$analysis_verify_data_save,{

      # get covariates
      new_col_names <- colnames(store$verified_df)
      cols_categorical <- store$column_types$categorical
      cols_continuous <- store$column_types$continuous
      X_cols <- grep("^X_", new_col_names, value = TRUE)
      X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)

      # send them off to the UI
      updateSelectInput(session = session,
                        inputId = 'analysis_overlap_select_var',
                        choices = X_cols,
                        selected = NULL
      )

    })

    # calculate propensity scores
    pscores <- reactive({
      # stop here if data hasn't been uploaded and selected
      validate_data_verified(store)

      # get variables
      X <- clean_to_indicator(store$verified_df)
      col_names <- colnames(X)
      treatment_col <- grep("^Z_", col_names, value = TRUE)
      response_col <- grep("^Y_", col_names, value = TRUE)
      confounder_cols <- grep("^X_", col_names, value = TRUE)

      # calculate pscores
      if(store$analysis_select_design == 'Observational Study (Treatment not Randomized)'){
        p_call <- paste0(treatment_col, '~', paste0(names(X)[3:length(X)], collapse = '+'))
        pscores <- dbarts::bart2(as.formula(p_call), data = X, seed = 2)
        pscores <- dbarts::extract(pscores, 'ev')
        pscores <- apply(pscores, 2, mean)
      }else{
        p_call <- paste0(treatment_col, '~', paste0(names(X)[3:length(X)], collapse = '+'))
        pscores <- glm(as.formula(p_call), data = X)
        pscores <- predict(pscores, type = 'response')
      }


      return(pscores)
    })

    # create the overlap plot
    overlap_plot <- reactive({

      # stop here if data hasn't been uploaded and selected
      validate_data_verified(store)

      # stop here if there are no numeric columns selected
      # validate(need(length(input$analysis_overlap_select_var) > 0,
      #               "No continuous columns available or currently selected"))

      # get variables for input into plotting functions
      X <- store$verified_df
      col_names <- colnames(X)
      treatment_col <- grep("^Z_", col_names, value = TRUE)
      response_col <- grep("^Y_", col_names, value = TRUE)
      cols_continuous <- store$column_types$continuous
      confounder_cols <- grep("^X_", cols_continuous, value = TRUE)
      plt_type <- input$analysis_overlap_method

      # plot either the variables or the 1 dimension propensity scores
      if(input$analysis_overlap_type == 2){
        if(is.numeric(X[[input$analysis_overlap_select_var]])){
          trim <- switch (input$analysis_overlap_trim,
                          'ATE' = c(min(X[[input$analysis_overlap_select_var]]),
                                    max(X[[input$analysis_overlap_select_var]])),
                          'ATT' = c(min(X[[input$analysis_overlap_select_var]][X[[treatment_col]] == 1]),
                                    max(X[[input$analysis_overlap_select_var]][X[[treatment_col]] == 1])),
                          'ATC' = c(min(X[[input$analysis_overlap_select_var]][X[[treatment_col]] == 0]),
                                    max(X[[input$analysis_overlap_select_var]][X[[treatment_col]] == 0]))
          )

          X <- X[X[[input$analysis_overlap_select_var]] >= trim[1],]
          X <- X[X[[input$analysis_overlap_select_var]] <= trim[2],]
        }

        p <- tryCatch(
          plotBart::plot_overlap_vars(
            .data = X,
            treatment = treatment_col,
            confounders = input$analysis_overlap_select_var,
            plot_type = plt_type
          ),
          error = function(e) NULL
        )
      }

      else if(input$analysis_overlap_type == 1){

        trim <- switch (input$analysis_overlap_trim,
                        'ATE' = c(NULL, NULL),
                        'ATT' = c(min(pscores()[X[[treatment_col]] == 1]),max(pscores()[X[[treatment_col]] == 1])),
                        'ATC' = c(min(pscores()[X[[treatment_col]] == 0]),max(pscores()[X[[treatment_col]] == 0]))
        )


        p <- tryCatch({
          plotBart::plot_overlap_pScores(
            .data = X,
            treatment = treatment_col,
            plot_type = plt_type,
            pscores = pscores(),
            min_x = trim[1],
            max_x = trim[2],
            trim = input$trim
          )
        },
        error = function(e) NULL
        )
      }

      # add theme
      p <- p + store$options$theme_custom

      return(p)
    })
    output$analysis_overlap_plot <- renderPlot({

      # stop here if data hasn't been uploaded and selected
      validate_data_verified(store)

      # stop here if there are no numeric columns selected
      # validate(need(length(input$analysis_overlap_select_var) > 0,
      #               "No continuous columns available or currently selected"))

      # build plot
      p <- overlap_plot()

      # TODO: this is often triggered; I think discrete variables cause the propensity score calculation to fail
      # stop if p is not a plot
      validate(need(
        inherits(p, 'ggplot'),
        'Error in building plot. Error likely occured in propensity score calculation. Did you select variables?'
      ))

      return(p)
    })

    # to save the parameters of downloaded overlap plots for reproducible script
    downloaded_overlap_plot_parameters <- reactiveValues(df = list())

    # parameters of current overlap plot
    overlap_plot_parameters <- reactive({
      # get variables
      X <- store$verified_df
      col_names <- colnames(X)
      treatment_col <- grep("^Z_", col_names, value = TRUE)
      response_col <- grep("^Y_", col_names, value = TRUE)
      cols_continuous <- store$column_types$continuous
      confounder_cols <- grep("^X_", cols_continuous, value = TRUE)

      if(input$analysis_overlap_type == 1){ # overlap plot by variables
        list(analysis_overlap_type = 1,
             treatment = treatment_col,
             response = response_col,
             confounders = paste(input$analysis_overlap_select_var, collapse = ','),
             plot_type = input$analysis_overlap_method)
      }else{ # p-score plot
        list(analysis_overlap_type = 2,
             treatment = treatment_col,
             response = response_col,
             confounders = paste(confounder_cols, collapse = ','),
             plot_type = input$analysis_overlap_method)
      }
    })

    output$download_overlap_plot <- downloadHandler(
      filename = 'overlap_plot.png',
      content = function(file) {
        ggsave(file,
               plot = overlap_plot(),
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
        # save the parameters of the downloaded overlap plot
        downloaded_overlap_plot_parameters$df <- rbind(downloaded_overlap_plot_parameters$df, overlap_plot_parameters())
      })


    # save into store for reproducible script
    downloaded_overlap_plot_parameters_df <- reactive(downloaded_overlap_plot_parameters$df)

    observeEvent(input$analysis_support_button_next, {
      store$analysis$eda$downloaded_overlap_plot_parameters <- downloaded_overlap_plot_parameters_df()
    })

    return(store)
  })
}
