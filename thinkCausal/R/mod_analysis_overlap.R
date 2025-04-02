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
        width = 12,
        collapsible = FALSE,
        title = 'Select a Causal Estimand & Check Overlap',
        selectInput(
          inputId = ns("analysis_inference_group"),
          label = 'Who would you like to make causal inferences about?',
          choices = c(
            '',
            'Both those that received the treatment and those that did not recieve the treatment.' = 'ate',
            'Only those that received the treatment.' = 'att',
            'Only those that did not receive the treatment.' = 'atc',
            "I don't understand this question" = 'help'
          )
        ),
        # selectInput(
        #   inputId = ns("analysis_inference_uncertainty"),
        #   label = 'How far to extend inference (sample, conditional, population)',
        #   choices = c('', 'Sample', 'Conditional', 'Population')
        # ),
        conditionalPanel(condition = "input.analysis_inference_group != ''",
                         ns = ns,
                         bs4Dash::box(title = 'Checking Overlap',
                                      status = 'secondary',
                                      id = ns('analysis_message_overlap'),
                                      width = 12,
                                      height = 500,
                                      collapsible = FALSE,
                                      conditionalPanel("output.check_overlap_view == '1'",
                                                       ns = ns,
                                                       uiOutput(ns('analysis_estimand_text'))),
                                      conditionalPanel("output.check_overlap_view == '0'",
                                                       ns = ns,
                                                       fluidRow(
                                                         column(
                                                           width = 3,
                                                           selectInput(inputId = ns('analysis_overlap_confirm'),
                                                                       label = 'Confirm Sufficent Overlap:',
                                                                       choices = NULL
                                                                       ),
                                                           checkboxInput(
                                                             inputId = ns('trim'),
                                                             label = 'Trim plot',
                                                             value = FALSE
                                                           ),
                                                           br(),
                                                           create_go_to_learning_btn('learn_estimands2', 'analysis_overlap', 'Learn how to check overlap'),
                                                           downloadButton(ns('download_overlap_plot'),
                                                                          label = "Download plot"),
                                                           actionButton(
                                                             inputId = ns("analysis_overlap_button_next"),
                                                             class = "nav-path",
                                                             label = "Next"
                                                           )
                                                         ),
                                                         column(width = 9,
                                                                plotOutput(
                                                                  outputId = ns("analysis_overlap_plot"),
                                                                  height = 450
                                                                )
                                                         )
                                                       )
                                      )

                         )
                         )

        # selectInput(
        #   inputId = ns('analysis_overlap_confirm'),
        #   label = ''
        # ),
        #textOutput(ns('analysis_inference_group_text')),


      )

      # bs4Dash::box(
      #   width = 9,
      #   collapsible = FALSE,
      #   title = 'Overlap plot',
      #   plotOutput(
      #     outputId = ns("analysis_overlap_plot"),
      #     height = 800
      #   )
      # )
    )
  )
}

#' analysis_overlap Server Functions
#'
#' @noRd
mod_analysis_overlap_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    page_control <- reactiveValues(status = 0,
                                   estimand = NULL,
                                   violated = NULL,
                                   satisfied = NULL)

    observeEvent(input$analysis_inference_group, {
      req(input$analysis_inference_group)
      if(input$analysis_inference_group == 'ate'){
        page_control$estimand <- 'ATE.'
      }

      if(input$analysis_inference_group == 'att'){
        page_control$estimand <- 'ATT.'
      }

      if(input$analysis_inference_group == 'atc'){
        page_control$estimand <- 'ATC.'
      }


      page_control$satisfied <- paste0('Yes, there is sufficent overlap to estimate the ', page_control$estimand)
      page_control$violated <- paste0('No, there is not sufficent overlap to estimate the ', page_control$estimand)

      updateSelectInput(inputId = 'analysis_overlap_confirm',
                        choices = c('', page_control$satisfied, page_control$violated)
                        )

    })

    observeEvent(input$analysis_overlap_confirm, {
      req(input$analysis_inference_group)
      if(input$analysis_overlap_confirm == page_control$violated){
        show_popup_overlap_warning(session, ns = ns)
      }
    })

    output$analysis_estimand_text <- renderUI({

      if(input$analysis_inference_group == 'ate'){
       out <- tagList(
         p('Based on your response above, you are interested in estimating the Average Treatment Effect (ATE).'),
         p('thinkCausal will estimate the ATE by:'),
         br(),
         p('- Predicting what would have happened to those in the treatment group had they not received the treatment.'),
         p('- Predicting what would have happened to those in the control group had they received the treatment.')
       )
      }



      if(input$analysis_inference_group == 'atc'){
        out <- tagList(
          p('Based on your response above, you are interested in estimating the Average Treatment Effect on the Control (ATC).'),
          p('thinkCausal will estimate the ATC by:'),
          br(),
          p('- Predicting what would have happened to those in the control group had they received the treatment.')
        )
      }

      if(input$analysis_inference_group == 'att'){
        out <- tagList(
          p('Based on your responses above, you are interested in estimating the Average Treatment Effect on the Treated(ATT).'),
          p('thinkCausal will estimate the ATT by:'),
          br(),
          p('- Predicting what would have happened to those in the treatment group had they not received the treatment.')
        )
      }

      out <- tagList(
        out,
        br(),
        p('Without sufficicent overlap thinkCausal will not be able to accuratly make these predictions.'),
        p(strong('Check for sufficent overlap before continuing!!')),
        br(),
        fluidRow(
          column(width = 3,
          actionButton(ns('analysis_check_overlap'), label = 'Check Overlap'),
          br(),
          create_go_to_learning_btn('learn_estimands2', 'analysis_overlap', 'Learn how to check overlap')

          #actionButton(ns('analysis_learn_overlap'), label = 'Learn how to check overlap')
          )
        )

      )

      return(out)
    })


    # remove message box when choice is made
    observeEvent(input$analysis_check_overlap, {
      page_control$status <- page_control$status + 1
      #bs4Dash::updateBox("analysis_message_overlap", action = "remove")
    })

    output$check_overlap_view <- reactive({
      ifelse(page_control$status%%2 == 0, 1, 0)
    })

    outputOptions(output, "check_overlap_view", suspendWhenHidden = FALSE)





    # open help on button click
    observeEvent(input$analysis_overlap_help, {
      open_help_sidebar(store, 'Overlap')
    })

    # next button
    observeEvent(input$analysis_overlap_button_next, {
      # save model estiand to access on fit model page
      store$analysis$model$analysis_model_estimand <- input$analysis_inference_group
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_model')
    })



    observeEvent(input$analysis_overlap_popup_stop, {
      close_popup(session = session)
    })

    # render default values when verified data is saved
    # observeEvent(store$analysis$data$verify$analysis_verify_data_save,{
    #
    #   # get covariates
    #   new_col_names <- colnames(store$verified_df)
    #   cols_categorical <- store$column_types$categorical
    #   cols_continuous <- store$column_types$continuous
    #   X_cols <- grep("^X_", new_col_names, value = TRUE)
    #   # X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
    #   call <- paste0('Y_', store$column_assignments$y, '~', paste0(X_cols, collapse = '+'))
    #   lmfit <- lm(as.formula(call), data = store$verified_df)
    #
    #   lmfit <- summary(step(lmfit))
    #   X_cols <- rownames(coef(lmfit))[2:length(rownames(coef(lmfit)))]
    #
    #   X_cols <- gsub('TRUE', '', X_cols)
    #
    #
    #   # send them off to the UI
    #   updateSelectInput(session = session,
    #                     inputId = 'analysis_overlap_select_var',
    #                     choices = X_cols,
    #                     selected = NULL
    #   )
    #
    # })

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
        pscores <- dbarts::bart2(as.formula(p_call), data = X, seed = 2, combineChains = T)
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
      #plt_type <- input$analysis_overlap_method


      trim <- switch (input$analysis_inference_group,
                      'ate' = c(NULL, NULL),
                      'att' = c(min(pscores()[X[[treatment_col]] == 1]),max(pscores()[X[[treatment_col]] == 1])),
                      'atc' = c(min(pscores()[X[[treatment_col]] == 0]),max(pscores()[X[[treatment_col]] == 0]))
      )


      p <- tryCatch({
        plotBart::plot_overlap_pScores(
          .data = X,
          treatment = treatment_col,
          plot_type = 'Histogram',
          pscores = pscores(),
          min_x = trim[1],
          max_x = trim[2],
          trim = input$trim
        )
      },
      error = function(e) NULL
      )

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
