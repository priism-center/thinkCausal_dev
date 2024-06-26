#' analysis_subgroup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_analysis_subgroup_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # side pannel
      column(width = 3,
             bs4Dash::box(
               width = 12,
               collapsible = FALSE,
               title = 'Subgroup analysis',
               shinyWidgets::radioGroupButtons(
                 inputId = ns('analysis_subgroup_type'),
                 label = 'Subgroup analysis step',
                 choices = c('Treatment effect variation',
                             'Predictors of variation',
                             'Exploratory subgroup analysis'),
                 selected = NULL,
                 direction = 'vertical'),
               # if pre-specified
               conditionalPanel(
                 condition = "input.analysis_subgroup_type == 'Pre-specified subgroup analysis'",
                 ns = ns,
                 selectInput(
                   inputId = ns('analysis_subgroup_prespecified'),
                   label = 'Define pre-specified subgroups by:',
                   choices = NULL,
                   selected = NULL
                 ),
                 selectInput(
                   inputId = ns('analysis_subgroup_prespecified_type'),
                   label = 'Plot type:',
                   choices = c('Histogram' = 'histogram',
                               'Density' = 'density',
                               'Error bar' = 'errorbar')
                 ),
                 conditionalPanel(condition = "input.analysis_subgroup_prespecified_type != 'errorbar' ",
                                  ns = ns,
                                  checkboxInput(inputId = ns('analysis_prespecified_subgroup_facet'),
                                                label = 'Overlay Plots',
                                                value = FALSE,
                                  ))
               ),
               # ICATE variation
               conditionalPanel(
                 condition = "input.analysis_subgroup_type == 'Treatment effect variation'",
                 ns = ns,
                 selectInput(
                   inputId = ns('analysis_subgroup_icate'),
                   label = 'Plot type:',
                   choices = c('Waterfall', 'Histogram')
                 ),
                 conditionalPanel(
                   condition = "input.analysis_subgroup_icate == 'Histogram'",
                   ns = ns,
                   sliderInput(
                     inputId = ns('analysis_subgroup_icate_bins'),
                     label = 'number of bins:',
                     value = 20,
                     min = 5,
                     max = 50,
                     step = 1
                   )
                 )
               ),
               # Rpart tree
               conditionalPanel(
                 condition = "input.analysis_subgroup_type == 'Predictors of variation'",
                 ns = ns,
                 sliderInput(
                   inputId = ns('analysis_subgroup_treedepth'),
                   label = 'Max tree depth:',
                   value = 2,
                   min = 1,
                   max = 3
                 )),
               # exploratory group analysis
               conditionalPanel(
                 condition = "input.analysis_subgroup_type == 'Exploratory subgroup analysis'",
                 ns = ns,
                 selectInput(inputId = ns('analysis_subgroup_exploratory'),
                             label = 'Define exploratory subgroups by:',
                             choices = NULL,
                             selected = NULL),
                 uiOutput(outputId = ns('render_analysis_subgroup_x_levels')),
                 selectInput(inputId = ns('analysis_subgroup_exploratory_type'),
                             label = 'Plot type:',
                             choices = NULL,
                             selected = NULL),
                 conditionalPanel(condition = "input.analysis_subgroup_exploratory_type != 'errorbar' ", ns = ns,
                                  checkboxInput(inputId = ns('analysis_subgroup_facet'),
                                                label = 'Overlay Plots',
                                                value = FALSE,
                                  )
                                  # numericInput(inputId = ns('analysis_subgroup_ncol'),
                                  #              label = 'Number of Columns:',
                                  #              value = 1,
                                  #              min = 1,
                                  #              max = 5)
                 )
               ),
               # break before standard options
               br(), br(),
               actionButton(inputId = ns('analysis_subgroup_help'),
                            label = 'What is this plot telling me?'),
               downloadButton(ns('download_subgroup_plot'), label = "Download plot"),
               actionButton(inputId = ns('analysis_moderator_analyses_button_reproduce'),
                            label = 'View analysis log')
             )
      ), # end side bar
      # main panel
      column(
        width = 9,
        bs4Dash::box(
          width = 12,
          collapsible = FALSE,
          # bs4Dash::tabBox(id = ns("analysis_subgroup_tabs"),
          title = textOutput(ns('analysis_subgroup_plot_title')),
          br(),
          plotOutput(
            outputId = ns('analysis_subgroup_plot'),
            height = 500
          ),
          conditionalPanel(condition = "input.analysis_subgroup_type == 'Pre-specified subgroup analysis'",
                           ns = ns,
                           reactable::reactableOutput(
                             outputId = ns('analysis_prespecifed_table')
                           )
          ),
          conditionalPanel(condition = "input.analysis_subgroup_type == 'Exploratory subgroup analysis'",
                           ns = ns,
                           reactable::reactableOutput(
                             outputId = ns('analysis_subgroup_table')
                           )
                           )
        )
      ) # end of main panel
    )
  )
}


#' analysis_subgroup Server Functions
#'
#' @noRd

mod_analysis_subgroup_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_subgroup_help, {
      open_help_sidebar(store, 'Subgroup analyses')
    })

    # nav buttons
    observeEvent(input$analysis_moderator_analyses_button_reproduce, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'reproduce')
    })

    output$analysis_subgroup_plot_title <- renderText({
      out <- switch(input$analysis_subgroup_type,
                    'Pre-specified subgroup analysis' = 'Results of pre-specified subgroup analysis',
                    'Treatment effect variation' = 'Treatment effect variation',
                    'Predictors of variation' = 'Predictors of treatment effect variation',
                    'Exploratory subgroup analysis' = 'Exploratory subgroup analysis',
      )

      return(out)
    })

    # include pre-specifed or not
    observeEvent(store$analysis$model$fit_good, {
      if (!is.null(store$analysis$subgroup$prespecified_subgroups)) {
        subgroup_options <- c(
          'Pre-specified subgroup analysis',
          'Treatment effect variation',
          'Predictors of variation',
          'Exploratory subgroup analysis'
        )
      } else{
        subgroup_options <- c('Treatment effect variation',
                              'Predictors of variation',
                              'Exploratory subgroup analysis')
      }

      shinyWidgets::updateRadioGroupButtons(
        inputId = 'analysis_subgroup_type',
        choices = subgroup_options,
        selected = NULL)

      X <- grep("^X_", names(store$verified_df), value = TRUE)
      R <- grep("^R_", names(store$verified_df), value = TRUE)
      B <- grep("^B_", names(store$verified_df), value = TRUE)
      X <- c(X, R, B)
      X <- substr(X, 3, nchar(X))
      X <- c('', X)
      updateSelectInput(
        inputId = 'analysis_subgroup_exploratory',
        choices = X,
        selected = NULL

      )

      updateSelectInput(
        inputId = 'analysis_subgroup_prespecified',
        choices = store$analysis$subgroup$prespecified_subgroups,
        selected = store$analysis$subgroup$prespecified_subgroups[1]
      )
    })

    find_levels <- reactive({
      # identify all logicals and indicators
      cols_indicators <- names(store$verified_df)[-c(1:2)][store$current_simple_column_types[-c(1,2)] == 'Binary']

      validate_model_fit(store)

      if(paste0('X_', input$analysis_subgroup_exploratory) %in%  cols_indicators){
        levels <- identify_indicators(
          x = paste0('X_', input$analysis_subgroup_exploratory), # level to test
          store$verified_df[, cols_indicators] # possible categorical variables
        )

        levels$best <- substr(levels$best, 3, nchar(levels$best))

        return(levels$best)
      } else return(NULL)
    })


    output$render_analysis_subgroup_x_levels <- renderUI({
      levels_ui <- find_levels()
      if(length(levels_ui) > 1) {
        selectInput(
          inputId = ns("analysis_subgroup_x_levels"),
          label = paste0("Contrast ", input$analysis_subgroup_exploratory, " with:"),
          multiple = TRUE,
          # removing selected .x from options
          choices = levels_ui[which(levels_ui != input$analysis_subgroup_exploratory)],
          selected = levels_ui[which(levels_ui != input$analysis_subgroup_exploratory)]
        )
      }else NULL

    })


    # Analysis ----------------------------------------------------------------
    # ICATE
    icate_plot <- reactive({
      # div_id <- 'analysis_subgroup_plot'
      # show_message_updating(div_id)

      validate_model_fit(store)

      if (input$analysis_subgroup_icate == 'Histogram') {
        p <- plotBart::plot_ICATE(store$analysis$model$model, n_bins = input$analysis_subgroup_icate_bins)
      } else{
        p <- plotBart::plot_waterfall(store$analysis$model$model)
      }

      p <- p + store$options$theme_custom

      return(p)
    })

    # Predict variation
    predict_icate_plot <- reactive({
      # div_id <- 'analysis_subgroup_plot'
      # show_message_updating(div_id)
      validate_model_fit(store)

      p <- plotBart::plot_moderator_search(store$analysis$model$model,
                                           max_depth = input$analysis_subgroup_treedepth)

      p <- p + store$options$theme_custom

      return(p)
    })

    #subgroup
    observeEvent(input$analysis_subgroup_exploratory, {
      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      if(input$analysis_subgroup_exploratory %in% cols_categorical){
      updateSelectInput(inputId = 'analysis_subgroup_exploratory_type',
                        choices = c('Histogram' = 'histogram',
                                    'Density' = 'density',
                                    'Error bar' = 'errorbar')
                        )
      }else{
        updateSelectInput(inputId = 'analysis_subgroup_exploratory_type',
                          choices = c('Histogram' = 'histogram',
                                      'Density' = 'density',
                                      'Error bar' = 'errorbar',
                                      'Loess')
        )
      }

    })
    exploratory_plot <- reactive({
      validate(need(input$analysis_subgroup_exploratory, 'Select a variable to define subgroups'))
      # div_id <- 'analysis_subgroup_plot'
      # show_message_updating(div_id)
      validate_model_fit(store)
      if (input$analysis_subgroup_exploratory_type != 'errorbar') {
        .facet <- !input$analysis_subgroup_facet
      } else{
        .facet <- FALSE
      }

      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      cols_continuous <- gsub('X_', '', store$column_types$continuous)

      if(input$analysis_subgroup_exploratory %in% cols_categorical){
        if (length(find_levels()) > 1) {
          .moderator <-
            clean_dummies_to_categorical(
              df = store$verified_df,
              group_names =  paste0('X_', find_levels()),
              new_name = 'moderator'
            )[['moderator']]

          .moderator <- gsub('X_', '', .moderator)
        }else{
          .moderator <- store$verified_df[[paste0('X_', input$analysis_subgroup_exploratory)]]
        }

        p <- plotBart::plot_moderator_d(store$analysis$model$model,
                                        type = input$analysis_subgroup_exploratory_type,
                                        moderator = .moderator,
                                        facet = .facet)
      }else{
        .moderator <- store$verified_df[[paste0('X_', input$analysis_subgroup_exploratory)]]

        if(input$analysis_subgroup_exploratory_type != 'Loess'){
          p <- plotBart::plot_moderator_c_bin(store$analysis$model$model,
                                              moderator = .moderator,
                                              .name = input$analysis_subgroup_exploratory,
                                              type = input$analysis_subgroup_exploratory_type,
                                              facet = .facet)
        }else{

          p <- plotBart::plot_moderator_c_loess(
            store$analysis$model$model,
            moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_exploratory)]]
            )
        }



      }

      p <- p + store$options$theme_custom

      return(p)
    })

    # Pre-specified
    output$analysis_prespecifed_table <- reactable::renderReactable({
      validate_model_fit(store)
      validate(need(input$analysis_subgroup_prespecified, ''))

      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      .moderator <- store$verified_df[[paste0('X_', input$analysis_subgroup_prespecified)]]

      icate <- bartCause::extract(store$analysis$model$model, 'icate')
      x <- store$verified_df[[paste0('X_', input$analysis_subgroup_prespecified)]]
      if(store$analysis$model$analysis_model_estimand == 'ATT'){
        x <- x[store$verified_df[[paste0('Z_', store$column_assignments$z)]] == TRUE]
        .moderator <- .moderator[store$verified_df[[paste0('Z_', store$column_assignments$z)]] == TRUE]
      }

      if(store$analysis$model$analysis_model_estimand == 'ATC'){
        x <- x[store$verified_df[[paste0('Z_', store$column_assignments$z)]] == FALSE]
        .moderator <- .moderator[store$verified_df[[paste0('Z_', store$column_assignments$z)]] == FALSE]
      }

      if(input$analysis_subgroup_prespecified %in% cols_categorical){
        subgroup <- vector()
        estimate <- vector()
        se <- vector()
        lci <- vector()
        uci <- vector()
        for (i in 1:length(unique(.moderator))) {
          post <- apply(icate[, x == unique(.moderator)[i]], 1, mean)
          subgroup[i] <- paste0(input$analysis_subgroup_prespecified, ' = ', unique(.moderator)[i])
          estimate[i] <- round(mean(post), 4)
          se[i] <- round(sd(post), 4)
          lci[i] <- round(quantile(post, prob = .025), 4)
          uci[i] <- round(quantile(post, prob = .975), 4)

        }

        ci_table <- data.frame(subgroup, estimate, se, lci, uci)

      }else{
        ci_table <- plotBart::table_moderator_c_bin()
      }


      ci_table %>%
        reactable::reactable()

    })

    prespecifed_plot <- reactive({
      # div_id <- 'analysis_subgroup_plot'
      # show_message_updating(div_id)

      validate_model_fit(store)
      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      cols_continuous <- gsub('X_', '', store$column_types$continuous)

      if (input$analysis_subgroup_prespecified_type != 'errorbar') {
        .facet <- !input$analysis_prespecified_subgroup_facet
      } else{
        .facet <- FALSE
      }

      if (input$analysis_subgroup_prespecified %in% cols_categorical) {
        p <- plotBart::plot_moderator_d(store$analysis$model$model,
                                        type = input$analysis_subgroup_prespecified_type,
                                        moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_prespecified)]],
                                        facet = .facet)
      } else{
        p <- plotBart::plot_moderator_c_bin(store$analysis$model$model,
                                            moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_prespecified)]],
                                            .name = input$analysis_subgroup_prespecified,
                                            type = input$analysis_subgroup_prespecified_type,
                                            facet = .facet)
      }

      p <- p + store$options$theme_custom

      return(p)
    })

    output$analysis_subgroup_table <- reactable::renderReactable({
      validate_model_fit(store)
      validate(need(input$analysis_subgroup_exploratory, ''))

      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      .moderator <- store$verified_df[[paste0('X_', input$analysis_subgroup_exploratory)]]

      icate <- bartCause::extract(store$analysis$model$model, 'icate')
      x <- store$verified_df[[paste0('X_', input$analysis_subgroup_exploratory)]]
      if(store$analysis$model$analysis_model_estimand == 'ATT'){
        x <- x[store$verified_df[[paste0('Z_', store$column_assignments$z)]] == TRUE]
        .moderator <- .moderator[store$verified_df[[paste0('Z_', store$column_assignments$z)]] == TRUE]
      }

      if(store$analysis$model$analysis_model_estimand == 'ATC'){
        x <- x[store$verified_df[[paste0('Z_', store$column_assignments$z)]] == FALSE]
        .moderator <- .moderator[store$verified_df[[paste0('Z_', store$column_assignments$z)]] == FALSE]
      }

      if(input$analysis_subgroup_exploratory %in% cols_categorical){
        subgroup <- vector()
        estimate <- vector()
        se <- vector()
        lci <- vector()
        uci <- vector()
        for (i in 1:length(unique(.moderator))) {
          post <- apply(icate[, x == unique(.moderator)[i]], 1, mean)
          subgroup[i] <- paste0(input$analysis_subgroup_exploratory, ' = ', unique(.moderator)[i])
          estimate[i] <- round(mean(post), 4)
          se[i] <- round(sd(post), 4)
          lci[i] <- round(quantile(post, prob = .025), 4)
          uci[i] <- round(quantile(post, prob = .975), 4)

        }

        ci_table <- data.frame(subgroup, estimate, se, lci, uci)
      }else{
        ci_table <- plotBart::table_moderator_c_bin(.model = store$analysis$model$model, moderator = .moderator)
      }


      ci_table %>%
          reactable::reactable()

    })

    output$analysis_subgroup_plot <- renderPlot({

      validate_model_fit(store)
      # # add overlay
      # div_id <- 'analysis_subgroup_plot'
      # # show_message_updating(div_id)
      p <- switch (input$analysis_subgroup_type,
                   'Treatment effect variation' = icate_plot(),
                   'Predictors of variation' = predict_icate_plot(),
                   'Pre-specified subgroup analysis' = prespecifed_plot(),
                   'Exploratory subgroup analysis' = exploratory_plot()
      )

      #remove overlay
      #close_message_updating(div_id)
      return(p)

    })

    # download plot
    output$download_subgroup_plot <- downloadHandler(
      filename = function() {
        switch(
          req(input$analysis_subgroup_tabs),
          "Pre-specified Subgroup Analysis" = 'pre_specified_subgroup_plot.png',
          "Search" = 'subgroup_regression_tree.png',
          "Exploratory Subgroup Analysis" = 'exploratory_subgroup_plot.png',
          "ICATE" = 'ICATE_plot.png'
        )
      },
      content = function(file) {

        # get the plot that is on the active tab
        active_plot <- switch(
          req(input$analysis_subgroup_tabs),
          'Pre-specified Subgroup Analysis' = analysis_pre_specified_moderators(),
          'Search' = analysis_subgroup_search_plot(),
          "Exploratory Subgroup Analysis" = analysis_explore_moderators(),
          'ICATE' = analysis_moderators_icate_plot()
        )

        # write out plot
        ggsave(file,
               plot = active_plot,
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
      }
    )
    return(store)


  })
}
